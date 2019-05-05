##-------------------------------------------------------
## Coursera Getting and Cleaning Data Course Project
## By Sreyashi Banik---sreyashibanik11@gmail.com---
##-------------------------------------------------------

# run_analysis.r File Description:
##-------------------------------------------------------
# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
##-------------------------------------------------------


# 1. Merge the training and the test sets to create one data set.

# Set working directory to the location where the UCI HAR Dataset was unzipped

setwd("C:\\Users\\User\\Desktop\\data science specialization\\Course_3_Getting and Cleaning Data\\DATA1\\UCI HAR Dataset\\")

# Loading dplyr and reshape2 packages

library(dplyr)
library(reshape2)

# Read features and activity names

features=read.table("features.txt",header = FALSE) # importing features.txt
activityName = read.table("activity_labels.txt",header=FALSE) #importing activity_labels.txt

# Assigning column names to the activityName data table

colnames(activityName)  = c("activityId","activityName")

# Reading train data

subjectTrain = read.table("train\\subject_train.txt",header=FALSE) #importing subject_train.txt
xTrain = read.table("train\\X_train.txt",header=FALSE) #importing X_train.txt
yTrain= read.table("train\\Y_train.txt",header=FALSE) #importing Y_train.txt

# Assign column names to the training set data tables

colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "activityId"

# Create the final training set by merging yTrain, subjectTrain, and xTrain

trainingSet=cbind(yTrain,subjectTrain,xTrain)

# Reading test data

subjectTest = read.table("test\\subject_test.txt",header=FALSE) #importing subject_test.txt
xTest = read.table("test\\X_test.txt",header=FALSE) #importing X_test.txt
yTest= read.table("test\\Y_test.txt",header=FALSE) #importing Y_test.txt

# Assign column names to the training set data tables

colnames(subjectTest)  = "subjectId"
colnames(xTest)        = features[,2]
colnames(yTest)        = "activityId"

# Create the final training set by merging yTrain, subjectTrain, and xTrain

testSet=cbind(yTest,subjectTest,xTest)

# Combine training and test data to create a final data set

finalDataSet = rbind(trainingSet,testSet)

# Creating a vector for the column names of finalDataSet data table

columnNames<-names(finalDataSet)

# 2. Extract only the measurements on the mean and standard deviation for each measurement

# create a logicalVector that contains TRUE values for the Id, mean and std columns and FALSE for the rest of them.

logicalVector = (grepl("activity..",columnNames) | grepl("subject..",columnNames) | grepl("-[Mm]ean..",columnNames)|grepl("-std..",columnNames))

# Subset finalData table based on the logicalVector to keep only desired columns

finalDataSet = finalDataSet[logicalVector==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalDataSet set with the acitivityName table to include descriptive activity names

finalDataSet=merge(activityName,finalDataSet,by="activityId",all.x=TRUE)

# Updating the colNames vector to include the new column names after merge

columnNames  = colnames(finalDataSet)

# 4. Appropriately label the data set with descriptive activity names 

# Removing activityId from the finalDataSet 

finalDataSet= select(finalDataSet,-activityId)

# Updating the colNames vector to include the new column names after removing activityId

columnNames  = colnames(finalDataSet)

# Giving syntactically valid column names

columnNames=make.names(columnNames)
columnNames=gsub("\\.","",columnNames)

# Making more descriptive column names

columnNames=gsub("mean","Mean",columnNames)
columnNames=gsub("[Ss]td","StandardDeviation",columnNames)
columnNames=gsub("[Mm]ag","Magnitude",columnNames)
columnNames=gsub("BodyBody","Body",columnNames)
columnNames=gsub("[Aa]cc","Accelerometer",columnNames)
columnNames=gsub("[Gg]yro","Gyroscope",columnNames)

# Reassigning the new descriptive column names to the finalDataSet

colnames(finalDataSet) = columnNames

# Melting the finalDataset for easier grouping

finalDataSet<-melt(finalDataSet,id=c("activityName","subjectId"))

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject

# Grouping data to get average of each variable for each activity and each subject

Groups<-group_by(finalDataSet,activityName,subjectId,variable)

# Summarizing the finalDataSet table to include just the mean of each variable for each activity and each subject

SummaryData<-summarize(Groups,Average=mean(value))

# Creating a new tidyDataSet

tidyDataSet=SummaryData

# Exporting the tidyDataSet 

write.table(tidyDataSet, "tidyDataSet.txt",row.names=TRUE,sep="\t")