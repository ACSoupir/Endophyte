#' Merges WinRhizo and mass files together
#' 
#' @import stringr
#' @import plyr
#' 
#' @author Alex Soupir
#'
#' @param weightdata A .csv file containing with headers "SYM Jar_Replicate Seed_Replicate Crop Condition Condition Start_Date Observation_Date Weight_g Root_Length_Pix SurfArea"
#' @param winrhizo A file containing the raw output of WinRhizo converted converted to .csv format
#' @param cropname A name that will be added to the output file along with the date that the function was run
#' @return The merged file of \code{weightdata} and \code{winrhizo}
#' @export
#' @examples
#' mergeSYM(weightdata = plant_weight_data, winrhizo = root_winrhizo_data, cropname = "Plant Name)
#' merged_df <- mergeSYM(weightdata = plant_weight_data, winrhizo = root_winrhizo_data, cropname = "Plant Name)

mergeSYM <- function(weightdata , winrhizo , cropname = NULL){
  
  winrhizo <- data.frame(winrhizo)
  
  #Checks for whether the WinRhizo *.csv has been edited
  if(winrhizo[1,1] == "RHIZO 2016a"){
    print("RAW WinRhizo Document")
    
    colnames(winrhizo) <- winrhizo[1,]
    winrhizo <- winrhizo[-c(1,2,3,4,5),]
    
    colnames(winrhizo)[1] <- "RHIZO.2016a"
    colnames(winrhizo)[16] <- "Length"
    colnames(winrhizo)[20] <- "SurfArea"
    colnames(winrhizo)[26] <- "RootVolume"
  }else if(winrhizo[4,1] == "SampleId"){
    print("RAW WinRhizo Document")
    
    winrhizo <- winrhizo[-c(1,2,3,4),]
    
    colnames(winrhizo)[1] <- "RHIZO.2016a"
    colnames(winrhizo)[16] <- "Length"
    colnames(winrhizo)[20] <- "SurfArea"
    colnames(winrhizo)[26] <- "RootVolume"
  }else{
    print("Edited WinRhizo Document")
    
    colnames(winrhizo)[1] <- "RHIZO.2016a"
    colnames(winrhizo)[16] <- "Length"
    colnames(winrhizo)[20] <- "SurfArea"
    colnames(winrhizo)[26] <- "RootVolume"
  }
  
  #All winrhizo files given they follow formatting
  #splits the winrhizo analysis names
  df <- rename(data.frame(str_split_fixed(winrhizo$RHIZO.2016a, "-", 3)),
               c("X1"="SYM","X2"="Jar_Replicate","X3"="Seed_Replicate"))
  
  #adds a new column with "SYM" content  
  df$temp <- "SYM"
  df<- df[,c(4,1,2,3)]
  
  #Checks which rows have "SYM" in front of the isolates number and removes it
  df$SYM <- as.character(df$SYM)
  i=1
  while(!is.na(df[i,2])==TRUE){
    df[i,2] <- gsub("^.*?M", "", df[i,2])
    i=i+1
  }
  
  #then adds sym to every isolate name and deletes the temp file 
  df$SYM <- paste(df$temp,df$SYM, sep = "")
  df <- df[,-c(1)]
  
  #adds a temp column with "SYM" as content, then checks which rows in the SYM column header have "SYM" in the name
  #removes "SYM" letters in "SYM" column then merges the temp column with the SYM column
  #this ensures that all entries have "SYM" in the name for merging with winrhizo
  df2 <- weightdata
  colnames(df2)[1] <- "SYM"
  df2$temp <- "SYM"
  df2 <- df2[,c(12,1:11)]
  
  df2$SYM <- as.character(df2$SYM)
  i=1
  while(!is.na(df2[i,2])==TRUE){
    df2[i,2] <- gsub("^.*?M", "", df2[i,2])
    i=i+1
  }
  
  df2$SYM <- paste(df2$temp,df2$SYM, sep="")
  df2 <- df2[,-c(1)]
  
  #adds the new document with the winrhizo information split back to winrhizo
  winCom <- data.frame(cbind(df, winrhizo))
  winCom$Seed_Replicate <- as.character(winCom$Seed_Replicate)
  winCom$Jar_Replicate <- as.numeric(winCom$Jar_Replicate)
  
  #couldn't figure out a way to make this shorter
  #renames the seed replicate to a integer rather than string
  if (winCom[1,3] == "A.tif_1") {
    winCom[winCom$Seed_Replicate=="A.tif_1","Seed_Replicate"]=1
    winCom[winCom$Seed_Replicate=="A.tif_2","Seed_Replicate"]=2
    winCom[winCom$Seed_Replicate=="A.tif_3","Seed_Replicate"]=3
    winCom[winCom$Seed_Replicate=="A.tif_4","Seed_Replicate"]=4
    winCom[winCom$Seed_Replicate=="A.tif_5","Seed_Replicate"]=5
    winCom[winCom$Seed_Replicate=="B.tif_1","Seed_Replicate"]=6
    winCom[winCom$Seed_Replicate=="B.tif_2","Seed_Replicate"]=7
    winCom[winCom$Seed_Replicate=="B.tif_3","Seed_Replicate"]=8
    winCom[winCom$Seed_Replicate=="B.tif_4","Seed_Replicate"]=9
    winCom[winCom$Seed_Replicate=="B.tif_5","Seed_Replicate"]=10
    winCom[winCom$Seed_Replicate=="C.tif_1","Seed_Replicate"]=11
    winCom[winCom$Seed_Replicate=="C.tif_2","Seed_Replicate"]=12
    winCom[winCom$Seed_Replicate=="C.tif_3","Seed_Replicate"]=13
    winCom[winCom$Seed_Replicate=="C.tif_4","Seed_Replicate"]=14
    winCom[winCom$Seed_Replicate=="C.tif_5","Seed_Replicate"]=15
  } else if (winCom[1,3] == "001.tif_1") {
    winCom[winCom$Seed_Replicate=="001.tif_1","Seed_Replicate"]=1
    winCom[winCom$Seed_Replicate=="001.tif_2","Seed_Replicate"]=2
    winCom[winCom$Seed_Replicate=="001.tif_3","Seed_Replicate"]=3
    winCom[winCom$Seed_Replicate=="001.tif_4","Seed_Replicate"]=4
    winCom[winCom$Seed_Replicate=="001.tif_5","Seed_Replicate"]=5
    winCom[winCom$Seed_Replicate=="002.tif_1","Seed_Replicate"]=6
    winCom[winCom$Seed_Replicate=="002.tif_2","Seed_Replicate"]=7
    winCom[winCom$Seed_Replicate=="002.tif_3","Seed_Replicate"]=8
    winCom[winCom$Seed_Replicate=="002.tif_4","Seed_Replicate"]=9
    winCom[winCom$Seed_Replicate=="002.tif_5","Seed_Replicate"]=10
    winCom[winCom$Seed_Replicate=="003.tif_1","Seed_Replicate"]=11
    winCom[winCom$Seed_Replicate=="003.tif_2","Seed_Replicate"]=12
    winCom[winCom$Seed_Replicate=="003.tif_3","Seed_Replicate"]=13
    winCom[winCom$Seed_Replicate=="003.tif_4","Seed_Replicate"]=14
    winCom[winCom$Seed_Replicate=="003.tif_5","Seed_Replicate"]=15
    
  }
  
  winCom$Length <- as.numeric(as.character(winCom$Length))
  winCom$SurfArea <- as.numeric(as.character(winCom$SurfArea))
  winCom$RootVolume <- as.numeric(as.character(winCom$RootVolume))
  
  #creates the summary table for the winrhizo document  
  WinRhizoSummary <- ddply(winCom,c("SYM","Jar_Replicate"),summarise,
                           N=length(Length),
                           mlength=mean(Length,na.rm=TRUE),
                           mSurfArea=mean(SurfArea,na.rm=TRUE),
                           mVolume=mean(RootVolume,na.rm=TRUE))
  View(WinRhizoSummary)
  
  #pauses function to allow user to check for errors that may have been made  
  if(interactive()){
    invisible(readline(prompt="Press 'Enter' if table looks acceptable..."))
  }
  
  #removes the length and surface area columns of the weights file  
  df3 <- df2[,1:9]
  
  df3$Weight_g <- as.numeric(as.character(df3$Weight_g))
  
  #creates the summary table for the weights  
  WeightSummary <- ddply(df3,c("SYM", "Jar_Replicate"),summarise,
                         N=length(Seed_Replicate),
                         mMass=mean(Weight_g,na.rm=TRUE))
  View(WeightSummary)
  
  #pauses function to allow user to check for errors that may have been made  
  if(interactive()){
    invisible(readline(prompt="Press 'Enter' if table looks acceptable..."))
  }
  
  #sets all of the columns that are going to be used to merge the weights to winrhizo to the same type (character)  
  df3 <- data.frame(df3)
  winCom <- data.frame(winCom)
  df3[,1] <- as.character(df3[,1])
  winCom[,1] <- as.character(winCom[,1])
  df3[,2] <- as.character(df3[,2])
  winCom[,2] <- as.character(winCom[,2])
  df3[,3] <- as.character(df3[,3])
  winCom[,3] <- as.character(winCom[,3])
  
  #merges the two files together  
  mdf <- merge(df3, winCom, by=c("SYM","Jar_Replicate","Seed_Replicate"))
  
  #makes final table including everything desired (includes volume of roots as well)
  FinalTable <- mdf[,c(1:9,25,29,35)]
  View(FinalTable)
  
  #pauses function to allow user to check for errors that may have been made  
  if(interactive()){
    invisible(readline(prompt="Press 'Enter' if table looks acceptable..."))
  }
  
  #creates a files with the date that the 2 documents were merged together  
  currentDate <- Sys.Date()
  fileName <- paste(cropname, " Merged ", currentDate, ".csv",sep="")
  write.csv(mdf[,c(1:9, 25, 29, 35)], file = fileName)
  
}