% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addDunn.R
\name{addDunn}
\alias{addDunn}
\title{Merges summarySYM with Dunn's Results}
\usage{
addDunn(data, summaryData, dunnComp, padj = NULL, tukey = NULL)
}
\arguments{
\item{data}{the raw data to be tested statistically}

\item{summaryData}{A data frame output by the summarySYM function}

\item{dunnComp}{the variable name in column 1 to}

\item{padj}{how to adjust the P value, if desired}
}
\value{
The merged file of \code{summaryData} and FSA's dunnTest results
}
\description{
Merges summarySYM with Dunn's Results
}
\examples{
mergeSYM(weightdata = plant_weight_data, winrhizo = root_winrhizo_data, cropname = "Plant Name)
merged_df <- mergeSYM(weightdata = plant_weight_data, winrhizo = root_winrhizo_data, cropname = "Plant Name)
}
\author{
Alex Soupir
}
