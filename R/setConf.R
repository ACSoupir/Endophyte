#' Set confidence interval to be used when running summarySYM()
#'
#' @return temp variable that holds confidence interval for summarySYM()
#' @examples
#' setConf()
#' 

setConf <- function() {
  temp <- readline(prompt = "Enter Confidence Level Desired (whole number): ")
  temp <<- as.integer(temp)/100
}