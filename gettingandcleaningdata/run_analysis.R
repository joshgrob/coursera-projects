#project imports
library(dplyr)
library(stringr)
#directory variables
dataDir <- "./tidy_data_prj/"
dataZipName <- "prj_data.zip"
downloadFileFullPath <- str_c(dataDir, dataZipName)

dataFilesPath <- str_c(dataDir, "UCI HAR Dataset/")
#file handles
activityLabelsFullPath <-
  str_c(dataFilesPath, "activity_labels.txt")
featuresFullPath <- str_c(dataFilesPath, "features.txt")

#' Aquires and unzips the HAR dataset 
#'
#' @return output of unzip command
#' @export
#'
acquireData <- function() {
  dataUrl <-
    "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(dataUrl, downloadFileFullPath)
  unzip(downloadFileFullPath, exdir = dataDir)

}

#' Modifies column names of HAR dataset to be more human readible
#'
#' @param messyColNames.  Vector of column names to cleanup 
#'
#' @return Vector of clean column names with order preserved from input
#' @export
#'
#' @example cleanColNames <- tidyColumnNames(messyCols)
tidyColumnNames <- function(messyColNames){
  #first strip out BodyBody
  tidyNames <- str_replace(messyColNames, "BodyBody", "Body")
  #now Modify Mean and Std Columns
  tidyNames <- str_replace(tidyNames, "-std\\(\\)-|std\\(\\)", "Std")
  tidyNames <- str_replace(tidyNames, "-mean\\(\\)-|-mean\\(\\)", "Mean")
  tidyNames <- str_replace(tidyNames, "-meanFreq\\(\\)-|-meanFreq\\(\\)", 
                           "MeanFreq")
  
}

#' Helper function to create a dataframe from either the test or train data 
#' files of the HAR dataset.
#'
#' @param dataSetType.  String value of either "train" or "test"
#'
#' @return Dataframe of the all datafiles from test or train datasets
#' @export
#' 
#' @note HAR data bundle must already be present 
#' @examples testDataSetTidy = createTidyDF("test")
createTidyDF <- function(dataSetType) {
  #test to make sure dataSetType has one of the appropriate values
  if (is.null(dataSetType) |
     !(dataSetType %in% c("test", "train")))
  {
    stop("Did not specify a train or test dataSetType")
  }
  
  #setup file handles
  dataSetFileName <- str_c("X_", dataSetType, ".txt")
  subjectFileName <- str_c("subject_", dataSetType, ".txt") 
  activityFileName <- str_c("y_", dataSetType, ".txt")
  
  #now get the full file path
  dataSetFullPath = str_c(dataFilesPath, dataSetType, "/", 
                          dataSetFileName)
  subjectFileFullPath = str_c(dataFilesPath, dataSetType, "/",
                              subjectFileName)
  activityFileFullPath = str_c(dataFilesPath, dataSetType, "/",
                               activityFileName)
  
  rawData = read.table(dataSetFullPath, col.names =
                         featureLabels[["featureLabel"]], check.names = FALSE)
  rawData <- rawData[, grep("mean|std", colnames(rawData))]
  rawData$personId <- read.table(subjectFileFullPath, col.names =
                                   "PersonId")[["PersonId"]]
  rawData$activityId <- read.table(activityFileFullPath, 
                                   col.names ="ActivityId")[["ActivityId"]]
  rawData <- merge(rawData, activityLabels, by.x = "activityId", by.y = "id")
  
  #Now cleanup colnames
  colnames(rawData) <- tidyColumnNames(colnames(rawData))
  
  rawData <- group_by(rawData, personId, activityName)
  rawData$activityId <- NULL
  
  rawDataMeanVariables = summarise_each(rawData, funs(mean))
  
  #rawDataMeanVariables$dataSetType <- rep(dataSetType,nrow(rawDataMeanVariables))
  rawDataMeanVariables <- cbind("dataSetType"= dataSetType, rawDataMeanVariables)
  
  #rename to reflect the mean was taken
  colnames(rawDataMeanVariables)[4:length(rawDataMeanVariables)] <- 
    str_c("meanAll", names(rawDataMeanVariables)[4:length(rawDataMeanVariables)])

  return(rawDataMeanVariables)
}

#setup up directories and acquire data if it doesn't exist
if (!file.exists(dataDir)) {
  dir.create(dataDir)
  acquireData()
} else if (!file.exists(downloadFileFullPath)) {
  acquireData()
} else if (file.exists(downloadFileFullPath) &&
           (!file.exists(dataFilesPath))) {
  unzip(downloadFileFullPath, exdir = dataDir)
}

#First get the label info
activityLabels <- read.table(activityLabelsFullPath, sep = " ",
                             col.names = c("id","activityName"))
featureLabels <- read.table(featuresFullPath, sep = " ",
                            col.names = c("id", "featureLabel"))

# Now get a dataframe for each test and train data
testDataSetTidy = createTidyDF("test")
trainDataSetTidy = createTidyDF("train")

#merge together in one file.  In short a full outer join.
finalDataSetTidy = merge(testDataSetTidy, trainDataSetTidy, all=TRUE)

#write out final data set
write.table(finalDataSetTidy, file = str_c(dataDir,"tidy_data.txt"), 
            row.names = FALSE)
