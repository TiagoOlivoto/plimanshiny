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
    mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Apply the model
  results <-
    dftemp %>%
    mutate(model_results = purrr::map(data, function(df) {
      df <- as.data.frame(df)

      if(!is.null(sowing_date)){
        flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df$date)$yday + 1
      }
      fflight <- min(flights)
      lflight <- max(flights) + 60
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
                                 fd = fdfun,
                                 sd = sdfun,
                                 b0 = b0,
                                 b1 = b1,
                                 b2 = b2,
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
    mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()

  # Apply the model
  results <-
    dftemp %>%
    mutate(model_results = purrr::map(data, function(df) {
      df <- as.data.frame(df)
      if(!is.null(sowing_date)){
        flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df$date)$yday + 1
      }
      fflight <- min(flights)
      lflight <- max(flights) + 60
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


      data.frame(b0 = b0,
                 b1 = b1,
                 b2 = b2,
                 inflection = b3,
                 heading = head,
                 maturity = maturation,
                 repr_period = maturation - head,
                 auc = int1$value,
                 auc_repr_period = int2$value)
    })) %>%
    unnest(cols = c(model_results)) |>
    dplyr::select(-data) |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE)

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
    mutate(unique_plot = paste0(block, "_", plot_id)) |>
    dplyr::select(dplyr::all_of(c("unique_plot", flight_date, predictor))) |>
    dplyr::group_by(unique_plot) |>
    tidyr::nest()


  # Apply the model
  results <-
    dftemp %>%
    mutate(model_results = purrr::map(data, function(df) {
      df <- as.data.frame(df)
      if(!is.null(sowing_date)){
        flights <- as.POSIXlt(df$date)$yday + 1 -  (as.POSIXlt(sowing_date)$yday + 1)
      } else {
        flights <- as.POSIXlt(df$date)$yday + 1
      }
      fflight <- min(flights)
      lflight <- max(flights) + 60
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


      data.frame(b0 = b0,
                 b1 = b1,
                 b2 = b2,
                 b3 = b3,
                 b4 = b4,
                 inflection = inflec$minimum,
                 heading = head,
                 maturity = maturation,
                 repr_period = maturation - head,
                 auc = int1$value,
                 auc_repr_period = int2$value)
    })) %>%
    unnest(cols = c(model_results)) |>
    dplyr::select(-data) |>
    tidyr::separate_wider_delim(unique_plot, names = c("block", "plot_id"), delim = "_", cols_remove = FALSE)

  return(results)
}




