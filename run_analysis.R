##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## Tadhg Adderley
## 16-12-2015

# run_analysis.r

# Create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################

# Clean up workspace
rm(list=ls())

# Set working directory and download data set
setwd("C:/Users/addertad.AUTH/Google Drive/Coursera/1_Data_Science/3_Getting & Cleaning Data/Assignments/Project")
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "temp")
unzip("temp")
setwd("C:/Users/addertad.AUTH/Google Drive/Coursera/1_Data_Science/3_Getting & Cleaning Data/Assignments/Project/UCI HAR Dataset")

#################################################################
# 1. Merges the training and the test sets to create one data set
#################################################################

# Read in Data #
activity <- read.table("activity_labels.txt", header = FALSE)
features <- read.table("features.txt", header = FALSE)

subject_train <- read.table("train/subject_train.txt", header = FALSE)
X_train <- read.table("train/X_train.txt", header = FALSE)
Y_train <- read.table("train/Y_train.txt", header = FALSE)

subject_test <- read.table("test/subject_test.txt", header = FALSE)
X_test <- read.table("test/X_test.txt", header = FALSE)
Y_test <- read.table("test/Y_test.txt", header = FALSE)

# Set Names of each Data Set #
colnames(activity) <- c("activity_id", "activity_type")
colnames(subject_train) <- "subject_id"
colnames(subject_test) <- "subject_id"
colnames(X_train) <- features[,2]
colnames(X_test) <- features[,2]
colnames(Y_train) <- "activity_id"
colnames(Y_test) <- "activity_id"

# Create Training Data
trainingData <- cbind(Y_train, subject_train, X_train)

# Create Test Data
testData <- cbind(Y_test, subject_test, X_test)

# Combine Training & Test Data
finalData <- rbind(trainingData, testData)

###########################################################################################
# 2. Extract only the measurements on the mean and standard deviation for each measurement
###########################################################################################

# Set colNames to be used to find mean and standard deviation
colNames <- colnames(finalData)
colNames <- data.frame(colNames)

# Create Logical Vector. Set True for Activity_ID, Subject_ID and all Mean & Standard Deviation
logicalVector <- (grepl("activity", colNames$colNames) |
                          grepl("subject", colNames$colNames) |
                          grepl("mean()", colNames$colNames) & 
                          !grepl("meanFreq()", colNames$colNames) |
                          grepl("std()", colNames$colNames))

# Create a new Final Data set that only includes the mean and standard deviation
finalData <- finalData[logicalVector == TRUE]

###########################################################################
# 3. Uses descriptive activity names to name the activities in the data set
###########################################################################

# Add Activity description to final data set
finalData <- merge(finalData, activity, by = "activity_id", all.x = TRUE)
# Move activity description to column 2 in data set
finalData <- finalData[, c(1, 69, 2:68)]

# Set col names of re-arranged final data set
colNames <- colnames(finalData)

######################################################################
# 4. Appropriately labels the data set with descriptive variable names
######################################################################

# Clean up the variable names
for (i in 1:length(colNames))
{
        colNames[i] <- gsub("-mean", "_Mean", colNames[i])
        colNames[i] <- gsub("-std", "_StdDev", colNames[i])
        colNames[i] <- gsub("^t", "Time_", colNames[i])
        colNames[i] <- gsub("^f", "Freq_", colNames[i])
        colNames[i] <- gsub("-X", "_X", colNames[i])
        colNames[i] <- gsub("-Y", "_Y", colNames[i])
        colNames[i] <- gsub("-Z", "_Z", colNames[i])
        colNames[i] <- gsub("\\()", "", colNames[i])
}

# Set new tidy column names for finalData set
colnames(finalData) <- colNames

#############################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject
#############################################################################

# Summarise the final data set to include just the mean of each variable for each activity & subject
tidyData <- aggregate(finalData[, names(finalData) != c("activity_id", "activity_type", "subject_id")],
                      by=list(activity_id = finalData$activity_id, activity_type = finalData$activity_type,
                              subject_id = finalData$subject_id),
                      mean)

# Sort data by activity_id
tidyData <- tidyData[order(tidyData$activity_id),]

# Export the tidyData set
write.table(tidyData, "tidyData.txt", row.names = FALSE)

