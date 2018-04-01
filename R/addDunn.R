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
               c("X1"=colnames(summaryData)[1],"X2"="X2"))

  df2 <- data.frame(cbind(df,dunn))
  df2 = df2[df2$X2 == dunnComp,]

  if(padj=="none"){
    df2 <- df2[,-c(2:4,6)]
    colnames(df2)[2] <- "Dunn.P.unadj"
  }else{
    df2 <- df2[,-c(2:4)]
    colnames(df2)[2] <- "Dunn.P.unadj"
    df2 <- rename(df2, c("P.adj" = padj))
  }

  df3 <- merge(summaryData, df2, by.x=colnames(summaryData)[1], by.y=colnames(df2)[1], all.x=TRUE)

  return(df3)
}
