# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# Download file and unzip, if it doesn't exists
if(!file.exists("./UCI HAR Dataset")){
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                destfile = "./getdata-projectfiles-UCI HAR Dataset.zip", 
                method = "curl")
  unzip("getdata-projectfiles-UCI HAR Dataset.zip")
}

# Loading subject and activity dataset
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
testActivity <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "activity")
trainActivity <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "activity")

# Loading activity labels 
actLabel <- read.table("./UCI HAR Dataset/activity_labels.txt", 
                       col.names = c("activityNumber", "activity"))

# Loading in variable names
colName <- read.table("./UCI HAR Dataset/features.txt", check.names = FALSE)

# Loading in train dataset
train <- read.table("./UCI HAR Dataset/train/X_train.txt",
                    colClasses = rep("numeric", 561),
                    col.names = colName[[2]], check.names = FALSE)

# Loading in test dataset
test <- read.table("./UCI HAR Dataset/test/X_test.txt",
                   colClasses = rep("numeric", 561),
                   col.names = colName[[2]], check.names = FALSE)

# put train and test subject data as a column, activity data as a column, and combine them
subAndAct <- cbind(rbind(trainSubject, testSubject), # combine column for subject number
                   rbind(trainActivity, testActivity))   # combine column for activity

# Replacing activity column with corresponding text labels from the activity label dataframe
subAndAct$activity <- actLabel[match(subAndAct$activity, actLabel[[1]]), 2]

# find all column/variable that contain "mean" or "std"
meanAndStd <- grep("mean|std", colName[[2]])

# Combine train/test datasets, subset to mean/std variables, combine w/ subject/activity columns
allData <- cbind(subAndAct, 
                 rbind(train, test)[,meanAndStd]) # combine train/test data set and subset

# get names of all variables 
varName <- names(allData)

# abbreviated strings in varName
abbr <- c("^f", "^t", "Acc", "-mean\\(\\)", "-meanFreq\\(\\)", "-std\\(\\)", "Gyro", "Mag", "BodyBody")

# corrections 
corrected <- c("freq", "time", "Acceleration", "Mean", "MeanFrequency", "Std", "Gyroscope", "Magnitude", 
               "Body")

# Replacing each abbreviated string with the corrected one
for(i in seq_along(abbr)){
  varName <- sub(abbr[i], corrected[i], varName)
}
# Replacing column names of allData with corrected version
names(allData) <- varName

# create independent data set with average for each variable or activity or subject
newData <- aggregate(allData[, 3:length(allData)], 
                     list(activity = allData$activity, subject = allData$subject), 
                     mean)

#Write data, as a txt file created with write.table() using row.name=FALSE 
write.table(newData, file = "UCI HAR Tidy Averages DataSet.txt", row.name = FALSE)

