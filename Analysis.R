###Batch Analysis
df <- initializeDataFrame()
##Make sure your working directory contains the right files 

##These are placeholder files because I didn't set up automatic read in of lipids yet
Lipid1 <- "pH396.xlsx"
Lipid2 <- "pH396.xlsx"
Lipid3<- "pH396.xlsx"
Lipid4<- "pH396.xlsx"
Lipid5<- "pH396.xlsx"
Lipid6<- "pH396.xlsx"
Lipid7<- "pH396.xlsx"
Lipid8<- "pH396.xlsx"
Lipid9<- "pH396.xlsx"
Lipid10<- "pH396.xlsx"

##Add the growth curves
df <- addNewRow(df, 1, 70, 200, 3.96, "pH396.xlsx", Lipid1, "xlsx")
df <- addNewRow(df, 2, 70, 200, 2.05, "pH205.xlsx", Lipid2, "xlsx")
df <- addNewRow(df, 3, 65, 200, 3, "65C.xlsx", Lipid3, "xlsx")
df <- addNewRow(df, 4, 65, 200, 3, "65C2.xlsx", Lipid4, "xlsx")
df <- addNewRow(df, 5, 70, 200, 3, "70C.xlsx", Lipid5, "xlsx")
df <- addNewRow(df, 6, 75, 200, 3, "75C.xlsx", Lipid6, "xlsx")
df <- addNewRow(df, 7, 80, 200, 3, "80C.xlsx", Lipid7, "xlsx")
df <- addNewRow(df, 8, 70, 50, 3, "50RPM.xlsx", Lipid8, "xlsx")
df <- addNewRow(df, 9, 70, 125, 3, "125RPM.xlsx", Lipid9, "xlsx")
df <- addNewRow(df, 10, 70, 300, 3, "300RPM.xlsx", Lipid8, "xlsx")

dfMinusNeg <- subset(df, Replicate != "Neg")
write.csv(dfMinusNeg, "dataframe.csv")
##With the new csv you should add any other data you have for individual samples
##RI, pH, etc. anything that is not covered already
##In the case of Saci I added the RI and GDGT peak sizes, as well as the pHpost experiment
dfPlusExtras = read.csv("dataframe.csv")
dfPlusExtras$GrowthCurves <- paste(dfPlusExtras$GrowthCurves)
dataframe = as.data.frame(dfPlusExtras, stringsAsFactors=FALSE)

#Run with 5 points in make growth curves
dataframe$Rate <- lapply(dataframe$GrowthCurves, makeGrowthRates)
dataframe$Rate <- unlist(dataframe$Rate)
dataframe$SacrificeOD <- lapply(dataframe$GrowthCurves, getSacrificeOD)
dataframe$SacrificeOD <- unlist(dataframe$SacrificeOD)

##Summarize batch experiments
#Temp
TempData <- subset(dataframe, pH == 3.00 & RPM == 200)
TempSummary <- getRateSummaryStats(TempData, TempData$Temp)
#pH
pHData <- subset(dataframe, Temp == 70 & RPM == 200)
pHSummary <- getRateSummaryStats(pHData, pHData$pH)
#Shaking
shakingData <-  subset(dataframe, Temp == 70 & pH == 3.00)
shakingRateSummary <- getRateSummaryStats(shakingData, shakingData$RPM)
shakingRateSummary$summarystat <- factor(shakingRateSummary$summarystat, levels = c("50", "125", "200" ,"300"))

##Statistics
require(userfriendlyscience)
##Run ANOVA's and Games Howell Posthoc tests on all subsets of the data.
oneway.test(RI~Temp, data = TempData, var.equal = FALSE)
oneway.test(RI~RPM, data = shakingData, var.equal = FALSE)
oneway.test(RI~pH, data = pHData, var.equal = FALSE)
oneway.test(Rate~Temp, data = TempData, var.equal = FALSE)
oneway.test(Rate~RPM, data = shakingData, var.equal = FALSE)
oneway.test(Rate~pH, data = pHData, var.equal = FALSE)
oneway(as.factor(TempData$Temp), y = TempData$RI, posthoc = 'games-howell')
oneway(as.factor(shakingData$RPM), y = shakingData$RI, posthoc = 'games-howell')
oneway(as.factor(pHData$pH), y = pHData$RI, posthoc = 'games-howell')
oneway(as.factor(TempData$Temp), y = TempData$Rate, posthoc = 'games-howell')
oneway(as.factor(shakingData$RPM), y = shakingData$Rate, posthoc = 'games-howell')
oneway(as.factor(pHData$pH), y = pHData$Rate, posthoc = 'games-howell')

#Regression on all data
regression.lm = lm(RI~Rate, dataframe)
summary(regression.lm)

###Fed Batch Data
dOdf <- initializeDataFrame();
setwd("C:/Users/aleco/Desktop/Google Drive/Dartmouth/LeavittLab/Sulfolobus_acidocaldarius/SacidocaldariusGrowthExperiments/RawData/dOExperiments")
require(data.table)
require(readxl)
Lipid1 <- "SaciChemostats12_17_18Data.xlsx"
Lipid2 <- "SaciChemostats12_17_18Data.xlsx"
Lipid3<- "SaciChemostats12_17_18Data.xlsx"

dOdf <- addNewRow(dOdf, 1, 70, 20, 3, "SaciChemostats12_10_18Data.xlsx", Lipid1, "xlsx")
dOdf <- addNewRow(dOdf, 2, 70, 2, 3, "SaciChemostats12_17_18Data.xlsx", Lipid1, "xlsx")
dOdf <- addNewRow(dOdf, 3, 70, 0.2, 3, "Saci_Chemostats1_16_19Responses.xlsx", Lipid1, "xlsx")
dOdf <- addNewRow(dOdf, 4, 70, 0.5, 3, "Saci_Chemostats2_22_19Data.xlsx", Lipid1, "xlsx")
dOdf <- addNewRow(dOdf, 5, 70, 1, 3, "Saci_Chemostats3_11_19Data.xlsx", Lipid1, "xlsx")
dOdf <- subset(dOdf, Replicate != "Commands")
dOdf <- subset(dOdf, Replicate != "Notes")

dOdf$Rate <- lapply(dOdf$GrowthCurves, makeGrowthRates)
dOdf$Rate <- unlist(dOdf$Rate)

dOSummary <- getRateSummaryStats(dOdf, dOdf$RPM)
##are rates different. 
oneway.test(Rate~RPM, data = dOdf, var.equal = FALSE)
oneway(as.factor(dOdf$RPM), y = dOdf$Rate, posthoc = 'games-howell')
##Rate vs RI
regressiondO.lm = lm(RI~Rate, dOdf)
summary(regressiondO.lm)