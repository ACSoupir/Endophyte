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
#' @param signif significance level, default is 0.05 when set to NULL
#' @return The merged file of \code{summaryData} and FSA's dunnTest results
#' @export
#' @examples
#' mergeSYM(weightdata = plant_weight_data, winrhizo = root_winrhizo_data, cropname = "Plant Name)
#' merged_df <- mergeSYM(weightdata = plant_weight_data, winrhizo = root_winrhizo_data, cropname = "Plant Name)

addNormSig <- function(anova, summaryData, groupvar, signif, keepAll=FALSE){

  if (is.null(signif)) {
    signif = "0.05"
  }
  temp.letter = HSD.test(anova, groupvar, signif, group = TRUE,
                         console = FALSE)
  df = data.frame(temp.letter[["groups"]])
  df$temp = row.names(df)
  colnames(df)[3] = groupvar
  df = df[c(3, 2)]
  df2 = merge(summaryData, df, by = groupvar, all = keepAll)
  return(df2)
}
