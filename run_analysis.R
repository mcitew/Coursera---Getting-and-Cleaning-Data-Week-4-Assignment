
# Week 4 Cleaning and Getting Data Assignment
# Install dplyr using install.packages ("dplyr") if necessary

library(dplyr)
# Download the file and unzip files into SourceData directory (already set)

# create folder if does not exist
if(!file.exists("./SourceData")){dir.create("./SourceData")}

# Download and unzip into SourceData folder
FileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(FileUrl, destfile = "./SourceData/SmartPhoneData.zip")
unzip("./SourceData/SmartPhoneData.zip", exdir = "./SourceData")

#******************************************************************
#Step 1.Merges the training and the test sets to create one data set.
#******************************************************************

# Read in the features and activity labels information into separate vectors to be used later in the script
features <- read.table("./SourceData/UCI HAR Dataset/features.txt")
activity <- read.table("./SourceData/UCI HAR Dataset/activity_labels.txt")
colnames(activity) <- c("ActivityID", "Activity")

#  Read in the training table
x_train <- read.table("./SourceData/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./SourceData/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table ("./SourceData/UCI HAR Dataset/train/subject_train.txt")

#  Read in the test set data and associated reference files
x_test <- read.table("./SourceData/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./SourceData/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table ("./SourceData/UCI HAR Dataset/test/subject_test.txt")

# Merge the training and test data sets into one dataset.
MergeData <- rbind(x_train, x_test)

# Merge the activity and subject reference tables for the training and test sets into single reference tables.
MergeActivity <- rbind(y_train, y_test)
MergeSubjects <- rbind(subject_train, subject_test)


#******************************************************************
#Step 2.-Extracts only the measurements on the mean and standard deviation for each measurement.
#******************************************************************

# Add in appropriate descriptive labels to the column variables in the merged dataset using the created 'features' vector.  Add in descriptive column labels to the activity and subject tables.
colnames(MergeData) <- features[,2]
colnames(MergeActivity) <- "ActivityID"
colnames(MergeSubjects) <- "SubjectID"

# Add in columns to the merged data set so appropriate activity IDs and subject IDs are assigned to each row.
Merged_set_wlabels <- cbind(MergeSubjects, MergeActivity, MergeData)

# Extract only the measurements relating to mean and standard deviation from the merged dataset.
SelectedColumns <- grepl("*mean\\(\\)|*std\\(\\)|ActivityID|SubjectID", names(Merged_set_wlabels))
SelectedData <- Merged_set_wlabels[ , SelectedColumns]

#******************************************************************
#Step 3. Uses descriptive activity names to name the activities in the data set
#******************************************************************

# Replace the activity IDs with the descriptive names for the activity.
LabelledData <- merge(SelectedData, activity, by="ActivityID") 
LabelledData <- LabelledData[, c(2,ncol(LabelledData), 3:(ncol(LabelledData)-1))]


#******************************************************************
#Step 4. Appropriately labels the data set with descriptive variable names.
#******************************************************************

#******************************************************************
#Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#******************************************************************

# Create a tidy data set where the average for each of the variables has been calculated for each activity and subject combination and shown on a single row.
TidyData <- aggregate(.~SubjectID+Activity, LabelledData, mean)
#TidyData <- arrange(TidyData, SubjectID)
TidyData <- TidyData[order(TidyData$SubjectID, TidyData$Activity),]

# Copy tidy data set to a text file as required by the assignment
write.table(TidyData, "TidyData.txt", row.names = FALSE, quote = FALSE)
