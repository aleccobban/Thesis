##ConsolidatingData.R
#Created November 2018 by Alec B. Cobban for consolidating growth and lipid data in Leavitt Lab at Dartmouth
#Current modifications made by ABC in April 2019
#All functions included here are original unless specified otherwise. 

##Creates an empty dataframe and returns the resulting object for use with this file
initializeDataFrame <- function(){
  ##Imports
  require(readxl)
  require(datasets)
  require(data.table)
  require(DataCombine)
  require(purrr)
  require(ggplot2)
  require(broom)
  ##Set up data frame with initial header columns. Add additional columns here. 
  x <-  c("ID", "Temp", "RPM", "pH", "Replicate", "Growth Curve", "Lipid Peaks")
  
  #Setup the dataframe with hte column names as defined by vector x
  df <- data.frame(matrix(ncol=length(x), nrow = 0))
  colnames(df) <- x
  return(df)
  
}

##Load data in from a growth file and add to the dataframe
##TODO work with lipid data to automate lipid analyses
##parameters are the dataframe to add data to, the ID of the dataset, the temperature of the dataset, the shaking speed, the pH and the growth data/lipid data and data format.
addNewRow <- function(df,ID, Temp, RPM, pH, growthData, lipidData, dataFormat){
  #If empty put in a blank row that can be overwritten later. This allows rbind to work on an otherwise empty dataset.
  if(nrow(df) == 0){
      df <- data.table(ID = numeric(0), Temp = numeric(0),  RPM = numeric(0), pH = numeric(0), Replicate = character(), GrowthCurves=character(), LipidPeaks=numeric(0))
   }
  
  #Determine dataformat and perform corresponding analysis. Leftmost column must be sampling times. All following columns must be replicate data
  if(dataFormat == "csv" || dataFormat == "xlsx"){
    
    #load excel formatted files
    if(dataFormat == "xlsx"){
      mygrowthData <- as.data.frame(read_excel(growthData))
      myLipidData <- as.data.frame(read_excel(lipidData))
    }
    #load csv files
    if(dataFormat == "csv"){
      mygrowthData <- as.data.frame(read.csv(growthData))
      myLipidData <- as.data.frame(read.csv(lipidData))
    }
    
    #Compress growth data into a string and add 1 growth curve/row. There's some funny things with the index due to R indexing on 1. 
    for (i in 0:(ncol(mygrowthData)-2)){
      #parses growth curve into 2 lists seperated by a slash (1 for OD600 and one for Sampling time. There is one per replicate.)
      myGrowthDatal <- list(as.list(mygrowthData[,1]),"/", as.list(mygrowthData[,i+2]))
        myGrowthDatal1 <- paste(myGrowthDatal, collapse = ' ')
        ##inherits name of row header for growth curve from the initial sample
        myReplicate = colnames(mygrowthData)[i+2]
        ##If the first row, you need to insert row, because rbind will fail
      if(nrow(df) == 0){
        df <- InsertRow(df, list(ID, Temp, RPM, pH,myReplicate, myGrowthDatal1, " "))
      }
      else{
        
        df <- rbind(df, list(ID, Temp,RPM, pH, myReplicate, myGrowthDatal1,  " "))
      }
    }
    ##returns the dataframe containing the added rows
    return(df)
  }
  else{
    print("Data format must be either csv or xlsx")
  }
}


##parses growth curves into a mini dataframe and return with columns OD and Time. Input is a single cell of a dataframe containing data formatted with addNewRows from this file
parseGrowthCurve <- function(x){
  ##parse growth curves out of an individual cell into a dataframe
  GrowthRateList <- as.list(strsplit(x, split = "/"))
  GrowthRateList <- unlist(GrowthRateList)
  
  ##parses data values out of text, splits on commas due to previous formatting
  Time <- substr(trimws(as.character(GrowthRateList[1])), start = 6, stop = nchar(GrowthRateList[1])-2)
  Time <- as.numeric(unlist(strsplit(Time, split = ",")))
  
  OD <- substr(as.character(trimws(GrowthRateList[2])), start = 6, stop = nchar(GrowthRateList[2])-2)
  OD <-  as.numeric(unlist(strsplit(OD, split = ",")))
  
  ##returns a growth curve with a time and OD column
  GrowthCurve <- data.frame(Time, OD)
  return(GrowthCurve)
}



##calculates the growth rate based on a moving average of n Fpoints. A regression is calculated
##for these points and the slope and correlation coefficient are determined. 
##Assumes the dataframe being input has a list of Timepoints and 1 list of OD's
##percent similarity should be a fraction of 1
calculateMovingAverageGrowthRate <- function(df, n, percent_similarity){
  df <- subset(df, OD>0)
  listlen <- nrow(df)
  counter <- 1
  value = 0
  model <- NULL
  startCount <- 0
  df$lnOD <- log(df$OD)
  ##Go through every set of n points and get slopes and correlations
  while(counter + n <= listlen){
    dataset <- df[counter:(counter+n),]
    model <- lm(lnOD~Time, data =dataset)

    slope <- as.numeric(coef(model)[2])
    correlation <- sqrt(summary(model)$r.squared)
    output <- slope * correlation

    ##only record new values if they are larger than the old ones
    if(output > value){
      value = output
      startCount <- counter
      previousmodel <- model
      }
    counter = counter + 1
  }
  
  #This part of the algorithm takes the regression that you calculated before and expands it one point at a time
  #This continues until either the product of correlation and slope remains above a threshold of the max value, or the list ends.
  endcount <- startCount + n 
  while(endcount <= listlen){
    model <- lm(lnOD~Time, data = df[startCount:endcount,])
    slope <- as.numeric(coef(model)[2])
    correlation <- sqrt(summary(model)$r.squared)
    output <- slope * correlation
    ##If the product of the slope and correlation is less than a given percent of the prevoius max then return what was in the previous max 
    if (output < percent_similarity*value){
      if((dataset$Time[2]-dataset$Time[1]) >5)
        Rate= as.numeric(coef(model)[2])*3600
      
      ##Sometimes Tdiff calcullates in days instead of seconds depending on the time format you use.
      ##This happens only if it thinks there are less than 5 seconds between samples, the value is most likely in days
      ##And no one usually has more than 5 days between samples. If you do then this needs to be changed.
      ##For all experiments up until now this has been a valid construct due to mismatch of data formats.
      else{
        Rate = as.numeric(coef(model)[2])/24
      }
      return(Rate)
    }
    #Make the model size one bigger
    endcount <-endcount +1
  }
  if((Time[2]-Time[1]) >5)
      Rate= as.numeric(coef(model)[2])*3600
  else{
    Rate = as.numeric(coef(model)[2])/24
  }
  
    
  return(Rate)
}

##A helper function that can be used to make growth rates based on applying to a dataframe full of growth curves
makeGrowthRates <- function(x){ 
  GrowthCurves <- parseGrowthCurve(x)
  ##Changing the parameters here will change the parameters given to the function calculating the moving average
  rate = calculateMovingAverageGrowthRate(GrowthCurves, 4, .95)
  return(rate)
}

##Average the growth curve based on a specific subset of the data. Provide the dataframe, pH, Temperature and RPM. 
##Because there may be multiple provide an experiment number and a Unique ID for each one--These should be numeric values made up to identify each experiment
AverageGrowthCurve <- function(df, mypH, myTemp, myRPM, ExperimentNumber, UniqueID){
  require(matrixStats)
  ##Pick out only the data that matches the search
  myDf <- subset(df, pH == mypH &RPM == myRPM& Temp == myTemp)
  GrowthCurveFrame <- NULL
  ##for every growth curve
  for (each in myDf$GrowthCurves) {
    ##make a growth curve dataframe if none exist, and pass in the times and ODs
    if(is.null(GrowthCurveFrame)){
      GrowthCurveFrame <- data.frame(time = parseGrowthCurve(each)$Time, OD1 = parseGrowthCurve(each)$OD)
    }
    ##if one does exist just pass in the rest of ODs.
  else{
    GrowthCurveFrame <- cbind(GrowthCurveFrame,parseGrowthCurve(each)$OD)
    }
  }
  ##column 1 is time, so ODs start at column 2
    initialCol = 2
    finalCol = ncol(GrowthCurveFrame)
    ##Calculate means and standard error for each set of growth curves
    GrowthCurveFrame$mean = rowMeans(GrowthCurveFrame[initialCol:finalCol])
    GrowthCurveMatrix <- data.matrix(GrowthCurveFrame, rownames.force = NA)
    GrowthCurveFrame$se = rowSds(GrowthCurveMatrix, cols = initialCol:finalCol)/ncol(GrowthCurveFrame)
    ##Setup elapsed time so that the growth curve starts at 0 hours
    for(myTimeIndex in 1:nrow(GrowthCurveFrame)){
      if (myTimeIndex ==1){
        GrowthCurveFrame$timeElapsed[myTimeIndex] =0
      }
      else{
        ##else growth curve is represented in elapsed time/hr
        GrowthCurveFrame$timeElapsed[myTimeIndex]= (GrowthCurveFrame$time[myTimeIndex] - GrowthCurveFrame$time[1])/3600
      }
    }
    ##Add unique ID to the growth curve and provide unique names for each one 
    names(GrowthCurveFrame) <- make.names(names(GrowthCurveFrame), unique = TRUE)
    GrowthCurveFrame$ID = UniqueID
    return(GrowthCurveFrame[grep("mean", colnames(GrowthCurveFrame)):grep("ID", colnames(GrowthCurveFrame))])
    }

##Provides summary statistics from Rate data of a specific dataframe, with grouping being performed by values in a specific column.
##For instance, if you want summary based on temperature --getRateSummaryStats(TempData, TempData$Temp)
getRateSummaryStats<- function(dataframe, column){
  print("Summarized")
  means <- unlist(by(dataframe$Rate, column, mean, simplify = FALSE))
  sds <- unlist(by(dataframe$Rate, column, sd, simplify = FALSE))
  nrows <- unlist(by(dataframe, column, nrow, simplify = FALSE))
  
  sumStats <- do.call(rbind, Map(data.frame, mean = means, sd = sds, nrow =nrows))
  sumStats$se <- sumStats$sd/sumStats$nrow
  sumStats <- cbind(summarystat = rownames(sumStats), sumStats)
  rownames(sumStats) <- NULL
   return(sumStats)
}

##Get ring index summary statsfrom a specific dataframe. Works same as getRateSummary
getRISummaryStats<- function(dataframe, column){
  print("Summarized")
  means <- unlist(by(dataframe$RI, column, mean, simplify = FALSE))
  sds <- unlist(by(dataframe$RI, column, sd, simplify = FALSE))
  nrows <- unlist(by(dataframe, column, nrow, simplify = FALSE))
  
  sumStats <- do.call(rbind, Map(data.frame, mean = means, sd = sds, nrow =nrows))
  sumStats$se <- sumStats$sd/sumStats$nrow
  sumStats <- cbind(summarystat = rownames(sumStats), sumStats)
  rownames(sumStats) <- NULL
  return(sumStats)
}

##Get the last OD600 of a growth curve
getSacrificeOD <- function(GrowthCurveList){
  GrowthCurves <- parseGrowthCurve(GrowthCurveList)
  SacrificeOD <- GrowthCurves$OD[length(GrowthCurves$OD)]
  return(SacrificeOD)
}

##Creates summary stats based on sacrifice OD. 
getSacrificeSummaryStats<- function(dataframe, column){
  print("Summarized")
  means <- unlist(by(dataframe$SacrificeOD, column, mean, simplify = FALSE))
  sds <- unlist(by(dataframe$SacrificeOD, column, sd, simplify = FALSE))
  nrows <- unlist(by(dataframe, column, nrow, simplify = FALSE))
  
  sumStats <- do.call(rbind, Map(data.frame, mean = means, sd = sds, nrow =nrows))
  sumStats$se <- sumStats$sd/sumStats$nrow
  sumStats <- cbind(summarystat = rownames(sumStats), sumStats)
  rownames(sumStats) <- NULL
  return(sumStats)
}
