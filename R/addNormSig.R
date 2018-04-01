#' Merges summarySYM with Dunn's Results
#'
#' @import stringr
#' @import plyr
#' @import agricolae
#'
#' @author Alex Soupir
#'
#' @param anova the anova of data of which significance is to be calculated
#' @param summaryData A data frame output by the summarySYM function
#' @param groupvar variable to group data by (independent variable)
#' @param alpha significance level, default is 0.05 when set to NULL
#' @return The merged file of \code{summaryData} and FSA's dunnTest results
#' @export
#' @examples
#' mergeSYM(weightdata = plant_weight_data, winrhizo = root_winrhizo_data, cropname = "Plant Name)
#' merged_df <- mergeSYM(weightdata = plant_weight_data, winrhizo = root_winrhizo_data, cropname = "Plant Name)

addNormSig <- function(anova, summaryData, groupvar, alpha=NULL){

  if(is.null(alpha)){
    alpha = "0.05"
  }

  temp.letter = HSD.test(anova, "Isolate", alpha, group=TRUE, console=FALSE)

  df = data.frame(temp.letter[["groups"]])
  df$Isolate = row.names(df)
  df = df[c(3,2)]

  df2 = merge(summaryData, df, by="Isolate")

  return(df2)
}
