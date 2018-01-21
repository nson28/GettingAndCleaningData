#This script will perform the  following operations :

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load r ackages and get the data from url
r_packages <- c("data.table", "reshape2")
sapply(r_packages, require, character.only=TRUE, quietly=TRUE)
directory <- getwd()
data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(data_url, file.path(directory, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

# Load activity labels and features
datasetActivityLabels <- fread(file.path(directory, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
datasetFeatures <- fread(file.path(directory, "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureNames"))
featuresRequired <- grep("(mean|std)\\(\\)", datasetFeatures[, featureNames])
dataMeasurements <- datasetFeatures[featuresRequired, featureNames]
dataMeasurements <- gsub('[()]', '', dataMeasurements)


# Load training datasets
trainingData <- fread(file.path(directory, "UCI HAR Dataset/train/X_train.txt"))[, featuresRequired, with = FALSE]
data.table::setnames(trainingData, colnames(trainingData), dataMeasurements)
trainActivitiesData <- fread(file.path(directory, "UCI HAR Dataset/train/y_train.txt")
                         , col.names = c("Activity"))
trainSubjectsData <- fread(file.path(directory, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
trainingData <- cbind(trainSubjectsData, trainActivitiesData, trainingData)


# Load test datasets
testData <- fread(file.path(directory, "UCI HAR Dataset/test/X_test.txt"))[, featuresRequired, with = FALSE]
data.table::setnames(testData, colnames(testData), dataMeasurements)
testActivities <- fread(file.path(directory, "UCI HAR Dataset/test/y_test.txt")
                        , col.names = c("Activity"))
testSubjects <- fread(file.path(directory, "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))
testData <- cbind(testSubjects, testActivities, testData)

# merge datasets
combinedData <- rbind(trainingData, testData)


# Convert classLabels to activityName basically. More explicit. 
combinedData[["Activity"]] <- factor(combinedData[, Activity]
                                 , levels = datasetActivityLabels[["classLabels"]]
                                 , labels = datasetActivityLabels[["activityName"]])

combinedData[["SubjectNum"]] <- as.factor(combinedData[, SubjectNum])
combinedData <- reshape2::melt(data = combinedData, id = c("SubjectNum", "Activity"))
combinedData <- reshape2::dcast(data = combinedData, SubjectNum + Activity ~ variable, fun.aggregate = mean)

data.table::fwrite(x = combinedData, file = "tidyData.txt", quote = FALSE)

