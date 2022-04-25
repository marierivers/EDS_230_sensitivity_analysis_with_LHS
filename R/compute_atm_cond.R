#' Compute atmospheric conductance
#'
#' @param zm is the height at which windspeed is measured - must be higher than the vegetation (cm); usually measured 200 cm above vegetation
#' @param h is vegetation height (cm)
#' @param v is windspeed (cm/s)
#' @param kd constant; default = 0.7
#' @param k0 constant; default = 0.1
#'
#' @return c_at = computed atmospheric conductance, cm/s (how easily vapor diffuses from vegetation surfaces)
#' @export
#'
#' @examples
compute_atm_cond = function(h, zm = 200 + h, v, kd = 0.7, k0 = 0.1) {
  zd = kd * h
  z0 = k0 * h
  if (zm < h) {
    stop("height of windspeed measurement must be greater than vegetation height")
  }
  
  c_at = v / (6.25 * log(((zm - zd) / z0)) ^ 2)
  
  return(list(c_at=c_at))
}