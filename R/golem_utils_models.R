mod_L3 <-  function(data, flight_date = "date", predictor = "median.NDVI", sowing_date = NULL){
  modfun <- function(x, b0, b1, b2) {
    b0 / (1 + exp((b1-x)/b2))
  }
  # first derivative
  fdfun <- function(x, b0, b1, b2) {
    # D(expression(b0 / (1 + exp((b1-x)/b2))), "x")
    b0 * (exp((b1 - x)/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2
  }

  # second derivative
  sdfun <- function(x, b0, b1, b2) {
    # D(expression(b0 * (exp((b1 - x)/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2), "x")
    -(b0 * (exp((b1 - x)/b2) * (1/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2 -
        b0 * (exp((b1 - x)/b2) * (1/b2)) * (2 * (exp((b1 - x)/b2) *
                                                   (1/b2) * (1 + exp((b1 - x)/b2))))/((1 + exp((b1 - x)/b2))^2)^2)
  }
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Apply the model
  results <-
    dftemp %>%
    dplyr::mutate(model_results = purrr::map(data, function(df) {
      df <- as.data.frame(df)

      if(!is.null(sowing_date)){
        flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df$date)$yday + 1
      }
      fflight <- min(flights)
      lflight <- max(flights) + 20
      flights_seq <- fflight:lflight
      y <- df %>% pull()

      # Logistic regression to predict median.NDVI as a function of flights
      model <- try(nls(y ~ SSlogis(flights, Asym, xmid, scal),
                       control = nls.control(maxiter = 1000)), silent = TRUE)

      if (inherits(model, "try-error")) return(data.frame())

      coefslog <- coef(model)
      b0 <- coefslog[1]
      b1 <- coefslog[2]
      b2 <- coefslog[3]

      # Critical points
      inflec <- optimise(fdfun, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = FALSE)
      cp1 <- optimise(sdfun, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = FALSE)
      cp2 <- optimise(sdfun, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = TRUE)

      xfd <- seq(min(flights), ceiling(inflec$minimum), length.out = 500)
      yfd <- fdfun(xfd, b0, b1, b2)

      dfreg <- data.frame(x = c(min(xfd), max(xfd)), y = c(max(yfd), min(yfd)))
      regmod <- lm(y ~ x, data = dfreg)
      predline <- predict(regmod, newdata = data.frame(x = xfd))
      distances <- abs(yfd - predline)
      head <- xfd[which.max(abs(yfd - predline))]

      maturation <- cp2$maximum
      int1 <- integrate(modfun, lower = fflight, upper = lflight, b0 = b0, b1 = b1, b2 = b2)
      int2 <- integrate(modfun, lower = head, upper = cp2$maximum, b0 = b0, b1 = b1, b2 = b2)
      int3 <- integrate(modfun, lower = fflight, upper = head, b0 = b0, b1 = b1, b2 = b2)

      dplyr::tibble(b0 = b0,
                    b1 = b1,
                    b2 = b2,
                    heading = head,
                    inflection = inflec$minimum,
                    maturity = maturation,
                    repr_period = cp2$maximum - head,
                    auc = int1$value,
                    auc_vege_period = int3$value,
                    auc_repr_period = int2$value,
                    parms = list(model = modfun,
                                 modeladj = model,
                                 fd = fdfun,
                                 sd = sdfun,
                                 coefs = list(
                                   b0 = b0,
                                   b1 = b1,
                                   b2 = b2
                                 ),
                                 xmin = fflight,
                                 xmax = lflight))
    })) %>%
    unnest(cols = c(model_results)) |>
    dplyr::select(-data) |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)
  return(results)
}

mod_L3_thresh <-  function(data, flight_date = "date", predictor = "median.NDVI", sowing_date = NULL, threshold){
  modfun <- function(x, b0, b1, b2) {
    b0 / (1 + exp((b1-x)/b2))
  }
  # first derivative
  fdfun <- function(x, b0, b1, b2) {
    # D(expression(b0 / (1 + exp((b1-x)/b2))), "x")
    b0 * (exp((b1 - x)/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2
  }

  # second derivative
  sdfun <- function(x, b0, b1, b2) {
    # D(expression(b0 * (exp((b1 - x)/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2), "x")
    -(b0 * (exp((b1 - x)/b2) * (1/b2) * (1/b2))/(1 + exp((b1 - x)/b2))^2 -
        b0 * (exp((b1 - x)/b2) * (1/b2)) * (2 * (exp((b1 - x)/b2) *
                                                   (1/b2) * (1 + exp((b1 - x)/b2))))/((1 + exp((b1 - x)/b2))^2)^2)
  }
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Apply the model
  results <-
    dftemp %>%
    dplyr::mutate(model_results = purrr::map(data, function(df) {
      df <- as.data.frame(df)

      if(!is.null(sowing_date)){
        flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df$date)$yday + 1
      }
      fflight <- min(flights)
      lflight <- max(flights) + 20
      flights_seq <- fflight:lflight
      y <- df %>% pull()

      # Logistic regression to predict median.NDVI as a function of flights
      model <- try(nls(y ~ SSlogis(flights, Asym, xmid, scal),
                       control = nls.control(maxiter = 1000)), silent = TRUE)

      if (inherits(model, "try-error")) return(data.frame())

      coefslog <- coef(model)
      b0 <- coefslog[1]
      b1 <- coefslog[2]
      b2 <- coefslog[3]

      # Critical points
      inflec <- optimise(fdfun, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = FALSE)
      cp1 <- optimise(sdfun, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = FALSE)
      cp2 <- optimise(sdfun, interval = c(fflight, lflight), b0 = b0, b1 = b1, b2 = b2, maximum = TRUE)

      xfd <- seq(min(flights), ceiling(inflec$minimum), length.out = 500)
      yfd <- fdfun(xfd, b0, b1, b2)

      dfreg <- data.frame(x = c(min(xfd), max(xfd)), y = c(max(yfd), min(yfd)))
      regmod <- lm(y ~ x, data = dfreg)
      predline <- predict(regmod, newdata = data.frame(x = xfd))
      distances <- abs(yfd - predline)
      head <- xfd[which.max(abs(yfd - predline))]

      maturation <- cp2$maximum
      int1 <- integrate(modfun, lower = fflight, upper = lflight, b0 = b0, b1 = b1, b2 = b2)
      int2 <- integrate(modfun, lower = head, upper = cp2$maximum, b0 = b0, b1 = b1, b2 = b2)
      int3 <- integrate(modfun, lower = fflight, upper = head, b0 = b0, b1 = b1, b2 = b2)

      dplyr::tibble(b0 = b0,
                    b1 = b1,
                    b2 = b2,
                    heading = head,
                    inflection = inflec$minimum,
                    maturity = maturation,
                    repr_period = cp2$maximum - head,
                    auc = int1$value,
                    auc_vege_period = int3$value,
                    auc_repr_period = int2$value,
                    parms = list(model = modfun,
                                 modeladj = model,
                                 fd = fdfun,
                                 sd = sdfun,
                                 coefs = list(
                                   b0 = b0,
                                   b1 = b1,
                                   b2 = b2
                                 ),
                                 xmin = fflight,
                                 xmax = lflight))
    })) %>%
    unnest(cols = c(model_results)) |>
    dplyr::select(-data) |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)
  return(results)
}





mod_L4 <-  function(data, flight_date = "date", predictor = "median.NDVI", sowing_date = NULL){
  # Logistic model L.4()
  modfun <-  function(x, b0, b1, b2, b3) {
    b1 + (b2 - b1) / (1 + exp(b0 * (x - b3)))
  }

  # derivada em relação a x do modelo W1.4()

  fdfun <- function(x, b0, b1, b2, b3){
    -((b2 - b1) * (exp(b0 * (x - b3)) * b0)/(1 + exp(b0 * (x - b3)))^2)
  }
  sdfun  <- function(x, b0, b1, b2, b3) {
    # D(expression(-((b2 - b1) * (exp(b0 * (x - b3)) * b0)/(1 + exp(b0 * (x - b3)))^2)), "x")
    -((b2 - b1) * (exp(b0 * (x - b3)) * b0 * b0)/(1 + exp(b0 * (x -
                                                                  b3)))^2 - (b2 - b1) * (exp(b0 * (x - b3)) * b0) * (2 * (exp(b0 *
                                                                                                                                (x - b3)) * b0 * (1 + exp(b0 * (x - b3)))))/((1 + exp(b0 *
                                                                                                                                                                                        (x - b3)))^2)^2)
  }

  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Apply the model
  results <-
    dftemp %>%
    dplyr::mutate(model_results = purrr::map(data, function(df) {
      df <- as.data.frame(df)
      if(!is.null(sowing_date)){
        flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df$date)$yday + 1
      }
      fflight <- min(flights)
      lflight <- max(flights) + 20
      flights_seq <- fflight:lflight
      y <- df %>% pull()
      model <- drc::drm(y ~ flights, fct = drc::L.4(), data = df)
      coefslog <- coef(model)
      b0 <- coefslog[1]
      b1 <- coefslog[2]
      b2 <- coefslog[3]
      b3 <- coefslog[4]

      # CRITICAL POINTS
      inflec <-
        optimise(fdfun,
                 interval = c(fflight, lflight),
                 b0 = b0,
                 b1 = b1,
                 b2 = b2,
                 b3 = b3,
                 maximum = FALSE)

      # # maturation, estimated as the maximum point of the second derivative
      cp1 <-
        optimise(sdfun,
                 interval = c(fflight, lflight),
                 b0 = b0,
                 b1 = b1,
                 b2 = b2,
                 b3 = b3,
                 maximum = FALSE)
      cp2 <-
        optimise(sdfun,
                 interval = c(fflight, lflight),
                 b0 = b0,
                 b1 = b1,
                 b2 = b2,
                 b3 = b3,
                 maximum = TRUE)
      # Heading
      xfd <- seq(min(flights), ceiling(cp1$minimum),  length.out = 500)
      yfd <- sdfun(xfd, b0, b1, b2, b3)
      # Heading, estimated by the maximum curvature of the second derivative
      # from the first flight to the inflection point

      # linear decreasing
      dfreg <- data.frame(x = c(min(xfd), max(xfd)),
                          y = c(max(yfd), min(yfd)))

      regmod <- lm(y ~ x, data = dfreg)
      coefsr <- coef(regmod)
      predline <- predict(regmod, newdata = data.frame(x = xfd))
      distances <- abs(yfd - predline)
      head <- xfd[which.max(abs(yfd - predline))]

      # Maturation
      xfd2 <- seq(ceiling(b3), lflight,  length.out = 500)
      yfd2 <- fdfun(xfd2, b0, b1, b2, b3)
      # Heading, estimated by the maximum curvature of the second derivative
      # from the first flight to the inflection point

      # linear decreasing
      dfreg2 <- data.frame(x = c(min(xfd2), max(xfd2)),
                           y = c(min(yfd2), max(yfd2)))

      regmod2 <- lm(y ~ x, data = dfreg2)
      # coefsr <- coef(regmod2)
      predline2 <- predict(regmod2, newdata = data.frame(x = xfd2))
      maturation <- xfd2[which.max(abs(yfd2 - predline2))]
      # integrate median.NDVI below the curve
      int1 <- integrate(modfun, lower = fflight, upper = lflight, b0 = b0, b1 = b1, b2 = b2, b3 = b3)
      int2 <- integrate(modfun, lower = head, upper = maturation, b0 = b0, b1 = b1, b2 = b2, b3 = b3)


      tibble::tibble(b0 = b0,
                     b1 = b1,
                     b2 = b2,
                     inflection = b3,
                     heading = head,
                     maturity = maturation,
                     repr_period = maturation - head,
                     auc = int1$value,
                     auc_repr_period = int2$value,
                     parms = list(model = modfun,
                                  modeladj = model,
                                  fd = fdfun,
                                  sd = sdfun,
                                  coefs = list(
                                    b0 = b0,
                                    b1 = b1,
                                    b2 = b2,
                                    b3 = b3
                                  ),
                                  xmin = fflight,
                                  xmax = lflight))
    })) %>%
    unnest(cols = c(model_results)) |>
    dplyr::select(-data) |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)

  return(results)
}


mod_L5 <-  function(data, flight_date = "date", predictor = "median.NDVI", sowing_date = NULL){
  # Logistic model L.5()
  modfun <- function(x, b0, b1, b2, b3, b4) {
    b1 + (b2 - b1) / (1 + exp(b0 * (x - b3)))^b4
  }
  # derivada em relação a x do modelo W1.4()
  fdfun <- function(x, b0, b1, b2, b3, b4){
    D(expression(  b1 + (b2 - b1) / (1 + exp(b0 * (x - b3)))^b4), "x")
    -((b2 - b1) * ((1 + exp(b0 * (x - b3)))^(b4 - 1) * (b4 * (exp(b0 *
                                                                    (x - b3)) * b0)))/((1 + exp(b0 * (x - b3)))^b4)^2)
  }
  sdfun <- function(x, b0, b1, b2, b3, b4){
    # D(expression(-((b2 - b1) * ((1 + exp(b0 * (x - b3)))^(b4 - 1) * (b4 * (exp(b0 *
    #                                                                              (x - b3)) * b0)))/((1 + exp(b0 * (x - b3)))^b4)^2)), "x")
    -((b2 - b1) * ((1 + exp(b0 * (x - b3)))^((b4 - 1) - 1) * ((b4 -
                                                                 1) * (exp(b0 * (x - b3)) * b0)) * (b4 * (exp(b0 * (x - b3)) *
                                                                                                            b0)) + (1 + exp(b0 * (x - b3)))^(b4 - 1) * (b4 * (exp(b0 *
                                                                                                                                                                    (x - b3)) * b0 * b0)))/((1 + exp(b0 * (x - b3)))^b4)^2 -
        (b2 - b1) * ((1 + exp(b0 * (x - b3)))^(b4 - 1) * (b4 * (exp(b0 *
                                                                      (x - b3)) * b0))) * (2 * ((1 + exp(b0 * (x - b3)))^(b4 -
                                                                                                                            1) * (b4 * (exp(b0 * (x - b3)) * b0)) * ((1 + exp(b0 *
                                                                                                                                                                                (x - b3)))^b4)))/(((1 + exp(b0 * (x - b3)))^b4)^2)^2)
  }

  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()


  # Apply the model
  results <-
    dftemp %>%
    dplyr::mutate(model_results = purrr::map(data, function(df) {
      df <- as.data.frame(df)
      if(!is.null(sowing_date)){
        flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df$date)$yday + 1
      }
      fflight <- min(flights)
      lflight <- max(flights) + 20
      flights_seq <- fflight:lflight
      y <- df %>% pull()

      model <- drc::drm(y ~ flights, fct = drc::L.5(), data = df)
      coefslog <- coef(model)
      b0 <- coefslog[1]
      b1 <- coefslog[2]
      b2 <- coefslog[3]
      b3 <- coefslog[4]
      b4 <- coefslog[5]

      # CRITICAL POINTS
      inflec <-
        optimise(fdfun,
                 interval = c(fflight, lflight),
                 b0 = b0,
                 b1 = b1,
                 b2 = b2,
                 b3 = b3,
                 b4 = b4,
                 maximum = FALSE)

      # # maturation, estimated as the maximum point of the second derivative
      cp1 <-
        optimise(sdfun,
                 interval = c(fflight, lflight),
                 b0 = b0,
                 b1 = b1,
                 b2 = b2,
                 b3 = b3,
                 b4 = b4,
                 maximum = FALSE)
      cp2 <-
        optimise(sdfun,
                 interval = c(fflight, lflight),
                 b0 = b0,
                 b1 = b1,
                 b2 = b2,
                 b3 = b3,
                 b4 = b4,
                 maximum = TRUE)
      # Heading
      xfd <- seq(min(flights), ceiling(cp1$minimum),  length.out = 500)
      yfd <- sdfun(xfd, b0, b1, b2, b3, b4)
      # Heading, estimated by the maximum curvature of the second derivative
      # from the first flight to the inflection point

      # linear decreasing
      dfreg <- data.frame(x = c(min(xfd), max(xfd)),
                          y = c(max(yfd), min(yfd)))

      regmod <- lm(y ~ x, data = dfreg)
      coefsr <- coef(regmod)
      predline <- predict(regmod, newdata = data.frame(x = xfd))
      distances <- abs(yfd - predline)
      head <- xfd[which.max(abs(yfd - predline))]

      # Maturation
      xfd2 <- seq(inflec$minimum, lflight,  length.out = 500)
      yfd2 <- fdfun(xfd2, b0, b1, b2, b3, b4)
      # Heading, estimated by the maximum curvature of the second derivative
      # from the first flight to the inflection point

      # linear decreasing
      dfreg2 <- data.frame(x = c(min(xfd2), max(xfd2)),
                           y = c(min(yfd2), max(yfd2)))

      regmod2 <- lm(y ~ x, data = dfreg2)
      # coefsr <- coef(regmod2)
      predline2 <- predict(regmod2, newdata = data.frame(x = xfd2))
      maturation <- xfd2[which.max(abs(yfd2 - predline2))]
      # integrate median.NDVI below the curve
      int1 <- integrate(modfun, lower = fflight, upper = lflight, b0 = b0, b1 = b1, b2 = b2, b3 = b3, b4 = b4)
      int2 <- integrate(modfun, lower = head, upper = maturation, b0 = b0, b1 = b1, b2 = b2, b3 = b3, b4 = b4)


      tibble::tibble(b0 = b0,
                     b1 = b1,
                     b2 = b2,
                     b3 = b3,
                     b4 = b4,
                     inflection = inflec$minimum,
                     heading = head,
                     maturity = maturation,
                     repr_period = maturation - head,
                     auc = int1$value,
                     auc_repr_period = int2$value,
                     parms = list(model = modfun,
                                  modeladj = model,
                                  fd = fdfun,
                                  sd = sdfun,
                                  coefs = list(
                                    b0 = b0,
                                    b1 = b1,
                                    b2 = b2,
                                    b3 = b3,
                                    b4 = b4
                                  ),
                                  xmin = fflight,
                                  xmax = lflight))
    })) %>%
    unnest(cols = c(model_results)) |>
    dplyr::select(-data) |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)
  return(results)
}


# Threshold-based methods
mod_loess <-  function(data, flight_date = "date", predictor = "median.NDVI", sowing_date = NULL, threshold){
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()
  # Apply the model
  results <-
    dftemp %>%
    dplyr::mutate(model_results = purrr::map(data, function(df) {
      df <- as.data.frame(df)
      if(!is.null(sowing_date)){
        flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df$date)$yday + 1
      }
      fflight <- min(flights)
      lflight <- max(flights) + 20
      flights_seq <- fflight:lflight
      y <- df %>% pull()

      if(nrow(df) <= 7) {
        # extract the date of maturity
        model <- loess(y ~ flights, span = 0.85)
      } else {
        model <- loess(y ~ flights)
      }
      # Maturation
      fitted.nge_loop <- predict(model, flights_seq)
      list_date_pred <- approx(fitted.nge_loop, flights_seq, xout = threshold)


      tibble::tibble(maturity = list_date_pred$y,
                     threshold = threshold,
                     parms = list(modeladj = model,
                                  xmin = fflight,
                                  xmax = lflight))
    })) %>%
    unnest(cols = c(model_results)) |>
    dplyr::select(-data) |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)
  return(results)
}


mod_segmented <-  function(data, flight_date = "date", predictor = "median.NDVI", sowing_date = NULL, threshold, slope = "min"){
  dftemp <-
    data |>
    dplyr::mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Apply the model
  results <-
    dftemp %>%
    dplyr::mutate(model_results = purrr::map(data, function(df) {
      df <- as.data.frame(df)
      if(!is.null(sowing_date)){
        flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df$date)$yday + 1
      }
      fflight <- min(flights)
      lflight <- max(flights) + 20
      flights_seq <- fflight:lflight
      y <- df %>% pull()

      # create linear model
      mod<-lm(y ~ flights)
      # set attempts to 0
      attempts = 0
      # set if.false to false
      if.false <- F
      # while if.false is false
      while(if.false == F){
        attempts <- attempts + 1
        #if the number of rows in the data is greater than 7 and the number of attempts is less than 100, then run the segmented function
        if(nrow(df) > 7 && attempts < 100){
          #run the segmented function with the following parameters
          seg_loop<-try(segmented::segmented(mod,
                                             seg.Z = ~ flights,
                                             npsi = 2,
                                             control = segmented::seg.control(n.boot = 50, random=T, tol=0.01)),
                        silent = T)

          #if the segmented function returns an error, then run the lm function
          if("try-error" %in% class(seg_loop)) {
            #run the lm function with the following parameters
            seg_loop<-lm(y ~ flights)
            #create a variable called slps that is equal to the second coefficient of the lm function
            slps <- (seg_loop$coefficients)[2]
            #create a variable called ncpt that is equal to the first coefficient of the lm function
            ncpt <- (seg_loop$coefficients)[1]
            #create a variable called DPM that is equal to the difference between the thresholdold and the intercept divided by the slope
            DPM <- (threshold - ncpt) / slps

            #if the segmented function does not return an error and the psi variable is not null, then run the following code
          } else if (!is.null(seg_loop$psi)) {

            #create a variable called slps that is equal to the slope of the segmented function
            slps <- segmented::slope(seg_loop)$flights
            #create a variable called ncpt that is equal to the intercept of the segmented function
            ncpt <- segmented::intercept(seg_loop)$flights

            #if the vegetation index is GLI or TGI, then set the slope variable equal to the minimum slope
            if (slope == "min") {
              slope <- min(slps[,1])
              #if the vegetation index is HI, then set the slope variable equal to the maximum slope
            } else {
              slope <- max(slps[,1])
              #if the vegetation index is not GLI, TGI, or HI, then print an error message
            }
            #create a variable called slope_interc that is equal to the index of the slope variable
            slope_interc <- which(slps[,1] == slope)
            #create a variable called B1_interc that is equal to the intercept at the slope_interc index
            B1_interc <- ncpt[slope_interc,1]

            #create a variable called DPM that is equal to the difference between the thresholdold and the intercept divided by the slope
            DPM <- (threshold - B1_interc) / slope
            #if the segmented function does not return an error and the psi variable is null, then run the following code
          } else {
            #run the lm function with the following parameters
            seg_loop<-lm(y ~ flights)
            #create a variable called slps that is equal to the second coefficient of the lm function
            slps <- (seg_loop$coefficients)[2]
            #create a variable called ncpt that is equal to the first coefficient of the lm function
            ncpt <- (seg_loop$coefficients)[1]
            #create a variable called DPM that is equal to the difference between the thresholdold and the intercept divided by the slope
            DPM <- (threshold - ncpt) / slps

          }

        } else {
          seg_loop<-try(segmented::segmented(mod,
                                             seg.Z = ~ flights,
                                             npsi = 1,
                                             control = seg.control(n.boot = 50, random=T, tol=0.01)),
                        silent = T) # try to run the segmented function

          if("try-error" %in% class(seg_loop)) { # if the segmented function fails, run a linear model
            seg_loop<-lm(y ~ flights) # run a linear model
            slps <- (seg_loop$coefficients)[2] # get the slope of the linear model
            ncpt <- (seg_loop$coefficients)[1] # get the intercept of the linear model
            DPM <- (threshold - ncpt) / slps # calculate the DPM

          } else if (!is.null(seg_loop$psi)) {

            slps <- segmented::slope(seg_loop)$flights
            ncpt <- segmented::intercept(seg_loop)$flights

            if (slope == "min") {
              slope <- min(slps[,1])
              #if the vegetation index is HI, then set the slope variable equal to the maximum slope
            } else {
              slope <- max(slps[,1])
              #if the vegetation index is not GLI, TGI, or HI, then print an error message
            }
            slope_interc <- which(slps[,1] == slope)
            B1_interc <- ncpt[slope_interc,1]

            DPM <- (threshold - B1_interc) / slope

          } else {
            seg_loop<-lm(y ~ flights)
            slps <- (seg_loop$coefficients)[2]
            ncpt <- (seg_loop$coefficients)[1]
            DPM <- (threshold - ncpt) / slps
          }
        }
        if.false <- T
      }


      tibble::tibble(maturity = as.numeric(DPM),
                     threshold = threshold,
                     parms = list(modeladj = seg_loop))
    })) %>%
    unnest(cols = c(model_results)) |>
    dplyr::select(-data) |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE) |>
    tidyr::nest(parms = parms)
  return(results)
}

# modl <- mod_segmented(df, predictor = "median.GLI", sowing_date = "2018-05-17",
#                       threshold = 0.04)
# modl2 <- mod_loess(df, predictor = "median.GLI", sowing_date = "2018-05-17",
#                       threshold = 0.04)

