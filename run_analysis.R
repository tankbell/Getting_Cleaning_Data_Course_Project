## (1) Merges the training and the test sets to create one data set.
## (2) Extracts only the measurements on the mean and standard deviation for each measurement.
## (3) Uses descriptive activity names to name the activities in the data set by changing
##     activity ids to their respective names.
## (4) Appropriately labels the data set with descriptive variable names by
##     applying the feature vector as column headers to the test and train
##     data sets , appending "subject" and "activity" columns to the datasets
## (5) creates a second, independent tidy data set with the average of each variable
##     for each activity and each subject.
runAnalysis <- function() {
  library(reshape)
  ## Get the pre-processed test and train data sets
  ## They have "subject" and "activity" column headers in addition
  ## to the columns named after the elements in the feature vector.
  ## The activity column has descriptive activity names
  testData <- preProcessDataSet("test/X_test.txt","test/subject_test.txt", "test/y_test.txt")
  trainData <- preProcessDataSet("train/X_train.txt","train/subject_train.txt", "train/y_train.txt")

  ## Merge test and train data
  testAndTrainData <- rbind(testData, trainData)
  
  ## Extracts only the measurements on the mean and standard deviation
  ## for each measurement. Inorder to do this , create a vector of column
  ## headers to be dropped.
  columns <- colnames(testAndTrainData)
  ## Create a vector of columns headers to keep
  headersToBeKept <- c("subject", "activity", columns[grepl("std|mean", columns)])
  ## Extract the mean and std deviation for each measurement
  testAndTrainData <- testAndTrainData[,(colnames(testAndTrainData) %in% headersToBeKept)]
  
  ## From the data set created above,create an independent tidy data set with 
  ## the average of each variable for each activity and each subject.

  ## Melt the data frame with id.vars = "subject" and "activity"
  ## and measure.vars being all the mean and std deviation
  ## measurements.
  meltedData <- melt(testAndTrainData, c("subject", "activity"))
  
  tidyData <- cast(meltedData, subject + activity ~ variable, mean)
  
  write.table(tidyData, "tidyDataSetUCI.txt", row.names = FALSE)
  
  return (tidyData)
}

## PreProcesses an input data set "x"(where x
## could be test or training data sets) by :
## (1) assigning column names from the features vector
## (2) append "subject"(y) and "activity"(z) as columns
##     to the input data set.
## (3) assigning column names to "subject" and "activity"
##     the column headers would be "subject" and "activity"
## (4) using descriptive activity names to name the activities
##     in the data set by replacing the activity Ids with their
##     corresponding names
preProcessDataSet <- function(x, y, z) {
  ## Construct the path to the file
  fileDataSet <- paste("./UCI HAR Dataset/", x, sep = "")
  dataSet <- read.table(fileDataSet)

  ## Get the features data frame
  features <- read.table("./UCI HAR Dataset/features.txt")

  ## From the features data frame , create a vector
  ## of features.
  featureVector <- as.character(features[,2])

  ## Create column headers for the data set 
  ## using the featureVector.
  colnames(dataSet) <- featureVector

  ## For the data sets, read the subject and
  ## activity ids.
  fileSubject <- paste("./UCI HAR Dataset/", y, sep = "")
  subject <- read.table(fileSubject)
  fileActivityId <- paste("./UCI HAR Dataset/", z, sep = "")
  activities <- read.table(fileActivityId)

  ## assign column names to subject and activityIds
  colnames(subject) <- "subject"
  colnames(activities) <- "activity"
  
  ## USE DESCRIPTIVE ACTIVITY NAMES TO NAME
  ## THE ACTIVITIES IN THE DATA SETS BY REPLACING
  ## THE ACTIVITY IDS WITH THEIR NAMES
  activities$activity <- sapply(activities$activity, getActivityName)

  ## Append subject and activities together
  ## activities now has descriptive activity names
  subjectAndActivities <- cbind(subject, activities)
  
  ## Append subjectAndActivities to the input data set ( test or train)
  processedDataSet <- cbind(subjectAndActivities, dataSet)
  
  ## return the processed data set
  return (processedDataSet)
}

## Given an activity id , this function
## returns the name of the activity.
## e.g x = 1 would return "WALKING"
##     x = 5 would return "STANDING" 
getActivityName <- function(x) {
  activityNames <- read.table("./UCI HAR Dataset/activity_labels.txt")
  ## assign column names to the data frame , "activityNames"
  ## for clarity.
  colnames(activityNames) <- c("id", "name")
  ## coerce from factor to a character vector
  activityNames[,2] <- as.character(activityNames[,2])
  ## get the activityName , given the id
  activityName <- with(activityNames, name[activityNames[,1] == x])
  return (activityName)
}