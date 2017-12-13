#' Adds another column to the table output by summarySYM with the percent change
#'
#' @param data The table that was output by summarySYM to add a percent change column to
#' @param perChange The row label that is to be compared to
#' @param na.rm Set true to not include cells with missing values
#' @return Table with new column of percent change about \code{perChange}
#' @examples
#' perChange(summary_table, perChange="Control", na.rm=TRUE)
#' new.summary_table <- perChange(summary_table, perChange="Control")

perChange <- function(data=NULL, perChange=NULL, na.rm=FALSE){
  
  i=1
  while(perChange != data[i,1]){
    i=i+1
  }
  
  j=1
  data$Percent <- ((data[,4] - data[i,4]) / data[i,4])*100
  
  print(data)
  return(data)
}