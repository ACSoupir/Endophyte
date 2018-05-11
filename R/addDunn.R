#' Merges summarySYM with Dunn's Results
#'
#' @import stringr
#' @import plyr
#' @import FSA
#'
#' @author Alex Soupir
#'
#' @param data the raw data to be tested statistically
#' @param summaryData A data frame output by the summarySYM function
#' @param dunnComp the variable name in column 1 to
#' @param padj how to adjust the P value, if desired
#' @return The merged file of \code{summaryData} and FSA's dunnTest results
#' @export
#' @examples
#' mergeSYM(weightdata = plant_weight_data, winrhizo = root_winrhizo_data, cropname = "Plant Name)
#' merged_df <- mergeSYM(weightdata = plant_weight_data, winrhizo = root_winrhizo_data, cropname = "Plant Name)

addDunn <- function(data, summaryData, dunnComp, padj = NULL){
  x <- colnames(summaryData)[1]
  y <- colnames(summaryData)[3]

  i=1
  while(y != colnames(data)[i]){
    i=i+1
  }

  j=1
  while(x != colnames(data)[j]){
    j=j+1
  }

  if(is.null(padj)){
    padj = "none"
  }

  dunn <- dunnTest(data[[i]] ~ data[[j]], data, method = padj, two.sided = FALSE)[["res"]]

  df <- rename(data.frame(str_split_fixed(dunn$Comparison, " - ", 2)),
               c("X1"="X1","X2"="X2"))

  df2 <- data.frame(cbind(df,dunn))
  df3 <- df2
  df2 = df2[df2$X1 == dunnComp,]

  if(is.na(df2[1,1]) == TRUE){
    df3 = df3[df3$X2 == dunnComp,]
    df3 = df3[,-c(2,3,4)]
    df3 <- rename(df3, c("X1" = x))
    t=1
  } else{
    df3 = df3[df3$X1 == dunnComp,]
    df3 = df3[,-c(1,3,4)]
    df3 <- rename(df3, c("X2" = x))
    t=0
  }

  if(padj=="none"){
    df3 <- df3[,-c(3)]
    colnames(df3)[2] <- "Dunn.P.unadj"
  }else{
    colnames(df3)[2] <- "Dunn.P.unadj"
    df3 <- rename(df3, c("P.adj" = padj))
  }

  df4 <- merge(summaryData, df3, by.x=colnames(summaryData)[1], by.y=colnames(df3)[1], all.x=TRUE)

  return(df4)
}
