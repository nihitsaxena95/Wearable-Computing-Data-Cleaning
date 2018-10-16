##############################################################################
#
# FILE
#   run_analysis.R
#
# OVERVIEW
#   Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
#   See README.md for details.
#

library("dplyr")

##############################################################################
# STEP 0 - Read data
##############################################################################

data_path <- "Data/UCI HAR Dataset/"

#read test data
testSubject <- read.table(file.path(data_path, "test", "subject_test.txt"))
test_X <- read.table(file.path(data_path, "test", "X_test.txt"))
test_y <- read.table(file.path(data_path, "test", "y_test.txt"))

#read train data
trainSubject <- read.table(file.path(data_path, "train", "subject_train.txt"))
train_X <- read.table(file.path(data_path, "train", "X_train.txt"))
train_y <- read.table(file.path(data_path, "train", "y_train.txt"))

#read features.txt
features <- read.table(file.path(data_path, "features.txt"), as.is = TRUE)

#read activity
activity <- read.table(file.path(data_path, "activity_labels.txt"))
names(activity) <- c("activityId", "activityName")

##############################################################################
# Step 1 - Merge the training and the test sets to create one data set
##############################################################################

#merging data
activityData <- rbind(
  cbind(testSubject, test_X, test_y),
  cbind(trainSubject, train_X, train_y)
)

#remove extra objects to clean memory
rm(testSubject, test_X, test_y, trainSubject, train_X, train_y)

#assign updated variable names
names(activityData) <- c("subject", features[,2], "activity")

##############################################################################
# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement
##############################################################################

#reduce data set based on the pattern below
colToKeep <- grepl("subject|activity|mean|std", names(activityData))

activityData <- activityData[, colToKeep]

##############################################################################
# Step 3 - Use descriptive activity names to name the activities in the data
#          set
##############################################################################

# reconstruct activity column to its descritive values
activityData$activity <- factor(activityData$activity, 
                                levels = activity[,1], labels = activity[,2])

##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

#replace string according to the pattern mentioned below

activityDataCol <- names(activityData)

activityDataCol <- gsub("[\\(\\)-]","",activityDataCol)

activityDataCol <- gsub("^f","frequencyDomain",activityDataCol)
activityDataCol <- gsub("^t", "timeDomain", activityDataCol)
activityDataCol <- gsub("mean","Mean",activityDataCol)
activityDataCol <- gsub("std", "StandardDeviation", activityDataCol)
activityDataCol <- gsub("Acc","Accelerometer",activityDataCol)
activityDataCol <- gsub("Gyro","Gyroscope",activityDataCol)
activityDataCol <- gsub("Mag","Magnitude",activityDataCol)
activityDataCol <- gsub("Freq","Frequency",activityDataCol)
activityDataCol <- gsub("BodyBody","Body",activityDataCol)

names(activityData) <- activityDataCol

rm(activityDataCol,features,activity, colToKeep)

##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

# group by subject and activity and summarise using mean
activityDataMean <- activityData %>%
                      group_by(subject, activity) %>%
                      summarise_all(funs(mean))

write.table(activityDataMean, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
