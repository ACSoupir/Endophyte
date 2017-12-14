#' Set confidence interval to be used when running summarySYM()
#' 
#' @author Alex Soupir
#'
#' @return temp variable that holds confidence interval for summarySYM()
#' @export
#' @example
#' setConf()
#' 

setConf <- function() {
  temp <- readline(prompt = "Enter Confidence Level Desired (whole number): ")
  temp <<- as.integer(temp)/100
}