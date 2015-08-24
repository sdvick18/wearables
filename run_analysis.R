################################################################################################
######  Introduction 
################################################################################################

# This  R script called run_analysis.R  does the following :
#  1) Merges the training and the test sets to create one data set.
#  2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#  3) Gives descriptive activity names to name the activities in the data set
#  4) Labels  the data set with descriptive variable names. 
#  5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

# Set up params for place to put zip file
etlProjDir       <- "~/SDV_ETL_Proj"
dataDir          <- sprintf("%s/%s", etlProjDir , "data")
zipFileName      <-  sprintf("%s/%s", dataDir  ,  "Dataset.zip"   )
zipFileLocation  <- sprintf("%s/%s", dataDir  ,   "UCI HAR Dataset")

# Setup data
if (!file.exists(dataDir ) ) { 
  dir.create(dataDir, recursive = TRUE);
}

zipFileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 

if (!file.exists(zipFileName ) ) {
  download.file(zipFileURL,destfile= zipFileNme, method = "curl") 
  unzip(zipFileName)
}

# Make sure that the UCI HAR Dataset is loaded
# THe root directory for the Dataset is in the variable ucrHarDatasetRoot


ucrHarDatasetRoot <- zipFileLocation;

# Useful constants

outputSep = "#==============================================================================================="

makeSetLocation <- function(setType) {
  return (sprintf("%s/%s", ucrHarDatasetRoot ,setType))
}

trainingDataLocation <- makeSetLocation("train")
testDataLocation     <- makeSetLocation("test")

dataTypeValues      <- c("X"           , "y",          "subject")
descriptiveDataType <- c("featureName" , "activityId", "subjectId" )

setTypeValues  <- c("test", "train" )

# Naming/Value restrictions  for types
#   setType  must be in setTypeValues 
#   dataType must be in dataTypeValues



announceFiles <- function(setType){
  print("")
  print(outputSep)
  print(sprintf("%s:%s", setType, list.files(makeSetLocation(setType))))
  print(outputSep)
  print("")
}

makeDataTableName <- function (dataType, setType){
  return (sprintf("%s_%s.txt" , dataType, setType))
}

#### Under Root
loadActivityLabels <- function(){
  table <- read.table(sprintf("%sactivity_labels.txt", ucrHarDatasetRoot))
  return(table)
}

loadActivityLabels <- function(){
  table <- read.table(sprintf("%s/activity_labels.txt", ucrHarDatasetRoot))
  return(table)
}

loadFeatures <- function(){
  table <- read.table(sprintf("%s/features.txt", ucrHarDatasetRoot))
  return(table)
}

# Returns a Dataframe 
loadDataSet <- function (dataType, setType){
  fullTableName <- sprintf("%s/%s", makeSetLocation(setType),  makeDataTableName (dataType, setType))
  print (fullTableName)
  table <- read.table(fullTableName)
  
  return(table)
}

loadDataFrame <- function (setType){  
  for (d in dataTypeValues)  {
    ds  <- loadDataSet(d,setType)
    names(ds)
  }  
}


library(dplyr)

################################################################################################
### Step 0 - Loading the training and test dataset
################################################################################################

print("========= Loading Features==============================================")
theFeatures <- loadFeatures()
theFeatureNames  <- theFeatures[,2]

print("========= Loading Activity Labels======================================")
theActivityLabels <- loadActivityLabels()
writeLines("")


print("========= Load training data===========================================")

print("=============== Load feature data==== (3) =============================")
xTrain <- loadDataSet ("X", "train")
colnames(xTrain) <- theFeatureNames 
featureTrainTable <- tbl_df(xTrain)
rm(xTrain)

print("=============== Load Activity data==== (4) ==========================")
activityTrain <- loadDataSet ("y", "train")
colnames(activityTrain) <- c("activityId")
activityTrainTable <- tbl_df(activityTrain)
rm(activityTrain)

print("=============== Load subject data==== (5) =============================")
subjectTrain <- loadDataSet ("subject", "train")
colnames(subjectTrain) <- c("subjectId")
subjectTrainTable <- tbl_df(subjectTrain)
rm(subjectTrain)

# Put together the subject, activity and features 
trainTable <- bind_cols(subjectTrainTable , bind_cols(activityTrainTable,featureTrainTable) )
writeLines("")
writeLines("")


print("========= Load test data==============================================")

print("=============== Load feature data==== (3) ============================")
xTest <- loadDataSet ("X", "test")
colnames(xTest) <- theFeatureNames 
featureTestTable <- tbl_df(xTest)
rm(xTest)

print("=============== Load Activity data==== (4) ==========================")
activityTest <- loadDataSet ("y", "test")
colnames(activityTest) <- c("activityId")
activityTestTable <- tbl_df(activityTest)
rm(activityTest)

print("=============== Load subject data==== (5) =============================")
subjectTest <- loadDataSet ("subject", "test")
colnames(subjectTest) <- c("subjectId")
subjectTestTable <- tbl_df(subjectTest)
rm(subjectTest)


# Put together the subject, activity and features 
testTable <- bind_cols(subjectTestTable , bind_cols(activityTestTable,featureTestTable) )

writeLines("")
writeLines("")




################################################################################################
### Step 1 - Merging the training and test sets
################################################################################################
mergedTable  <- rbind(trainTable, testTable)

################################################################################################
### Step 2 Extraction of the measurements on the mean and standard deviation 
################################################################################################
# get all the columns of the merged table
columNames     <- colnames (mergedTable)

# Get the columns that refer to std dev's
colsWithStds  <- function (cn) { grepl("std\\(", cn )  }
sCols <-Filter(colsWithStds  , columNames )

# Get the columns that refer to means
colsWithMeans  <- function (cn) { grepl("mean\\(", cn )  }
mCols <- Filter(colsWithMeans , columNames )

# Combine them (this could prolly be done w/ a single regular expression but for noww, this works)
msCols <- union (mCols , sCols)
idCols <- colnames (mergedTable) [ 1:2]

# id fields as well as means and std's are in this
colsToSelect <-  append (idCols ,  msCols)

#select the column names
extractedTable <- mergedTable[,colsToSelect]


################################################################################################
### Step 3 Give  descriptive activity names
################################################################################################
findActivityLevel <- function (a) {
  theActivityLabels[a,2] 
}
aCol  <- extractedTable[,2]

col1 <- extractedTable[,1]
descriptiveACol <- mapply(findActivityLevel, aCol)
restCols  <- extractedTable[, 3:length(colnames(extractedTable)) ]

step3 <- cbind(col1 , cbind (descriptiveACol ,restCols ))

################################################################################################
### Step 4 - Label the data set with descriptive variable names. 
################################################################################################
# this was already done in Step 0


################################################################################################
### Step 5 - Tidy data set with the average of each variable for each activity and each subject
################################################################################################


# The resultant table (step5) is tidy
# There is a feature for each mean and std variable for each subject and activity

tidydata <- step3 %>% group_by(subjectId, activityId ) %>% summarise_each(funs(mean))
write.table(tidydata, file = "tidydata.txt", row.names = FALSE)
