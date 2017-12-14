#' Creates a summary table for a metric that is set using measurevar
#'
#' @param data A dataset that has raw data to be summarized
#' @param measurevar A column heading that is to be summarized
#' @param change The row label that is to be used to compare to (control)
#' @param groupvars The column heading(s) for summary (isolate)
#' @param na.rm Set true to not include cells with missing values
#' @return Table with summarized information about \code{measurevar} sorted by \code{groupvars}
#' @examples
#' summarySYM(plant_merged, measurevar="Root_Length", groupvars=c("Isolate","Nutrient"), na.rm=TRUE)
#' summary_table <- summarySYM(plant_merged, measurevar="Root_Length", groupvars="Isolate", na.rm=TRUE)

summarySYM <- function(data=NULL, measurevar, change=NULL, groupvars=NULL, na.rm=FALSE,
                                conf.interval=temp, .drop=TRUE) {
  library(plyr)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, median, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     median = median(xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  #sets the variable in the table to what you want the 
  i=1
  while(change != datac[i,1]){
    i=i+1
  }
  
  #calculates the change for each variable set before
  datac$change <- ((datac[,4] - datac[i,4]) / datac[i,4])
  
  #returns the new table
  # use "<- summarySYM()" to set global table
  return(datac)
}