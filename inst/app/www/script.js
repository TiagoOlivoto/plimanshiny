function moveDivisor() {
  divisor.style.width = slider.value+"%";
}

$(document).on("shiny:connected", function(event) {
  var divisor = document.getElementById("divisor"),
  slider = document.getElementById("slider");
});

