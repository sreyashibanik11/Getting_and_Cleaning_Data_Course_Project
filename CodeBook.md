## CODE BOOK

	This code book will describe the data used in this project, as wll as the processing required to create the required tidy data set.

## Overview

	30 volunteers within an age bracket of 19-48 years performed 6 different activities(Walking, Walking Upstairs, Walking Downstairs, Sitting, Standing, Laying)  while wearing Samsung Galaxy S II smartphone on the waist. The smartphone captured various data about their movements using its embedded accelerometer and gyroscope.The obtained dataset had been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. Our ojective is to:
	* Merge the training and the test sets to create one data set
	* Extract only the measurements on the mean and standard deviation for each measurement
	* Use descriptive activity names to name the activities in the data set
	* Appropriately label the data set with descriptive activity names
	* Creates a second, independent tidy data set with the average of each variable for each activity and each subject
	
## Study Design

	* Data Source:
	* Data Description:
	
	Steps performed to collect the data:
	
	* 1. Downloaded the above zip folder from the course website
	* 2. Uzipeed the zip folder and stored the data in the working directory
	
	
## Analysis Process

	The analysis script, run_analysis.R reads in the processed experiment data and performs a number of steps to get it into summary form.

	* Both the processed test and training datasets are read in and merged into one data frame.
	* The data columns are then given names based on the features.txt file.
	* Columns that hold mean or standard deviation measurements are selected from the dataset, while the other measurement columns are excluded from the rest of the analysis.
	* Invalid characters (() and - in this case) are removed from the column names. Also, duplicate phrase BodyBody in some columns names is replaced with Body.
	* The data is then grouped by subject and activity, and the mean is calculated for every measurement column.
	* Finally, the summary dataset is written to a file, tidyDataSet.txt.

Each line in run_analysis.R is commented. Reference the file for more information on this process.


## Data Description

	The columns included in the output file are listed below:
	
	* "activityId" : activity identifier
	* "activityName" : activity type
	* "subjectId" : subject identifier
	* "variable" : measurement type
	* "Average" : mean calculated for every measurement type grouped by subject and activity
