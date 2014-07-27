library(reshape2)

download_file <- F

download_and_extract_files <- function() {
  # download and extract the file
  url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
  download.file(url,destfile = "./dataset.zip", method="curl",mode='wb')
  unzip('dataset.zip')
}

step_1_load_and_merge_data <- function() {
  # load the train and test sets
  subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  x_train       <- read.table("./UCI HAR Dataset/train/X_train.txt")
  y_train       <- read.table("./UCI HAR Dataset/train/y_train.txt")
  subject_test  <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  x_test        <- read.table("./UCI HAR Dataset/test/X_test.txt")
  y_test        <- read.table("./UCI HAR Dataset/test/y_test.txt")
  
  # set variable names for subject data
  names(subject_train) <- "subjectId"
  names(subject_test)  <- "subjectId"
  
  # set variable name for label data
  names(y_train) <- "activity"
  names(y_test)  <- "activity"
  
  # set variable names for train and test sets
  features       <- read.table("./UCI HAR Dataset/features.txt")
  names(x_train) <- features$V2
  names(x_test)  <- features$V2
    
  # combine files into one dataset 
  train <- cbind(subject_train, y_train, x_train)
  test  <- cbind(subject_test, y_test, x_test)
  rbind(train, test)
}


step_2_only_mean_and_std_variables <- function(dataset) {
  # find variables containing mean() or std()
  mean_and_std_variables <- grepl("mean\\(\\)", names(dataset)) |
    grepl("std\\(\\)", names(dataset))
  
  # ensure that we also keep the subjectID and activity columns
  mean_and_std_variables[1:2] <- TRUE
  
  # remove unnecessary columns
  dataset[, mean_and_std_variables]
}

step_3_set_descriptive_activities <- function(data){
  # translate the activity numbers to descriptive factors
  activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
  data$activity <- factor(data$activity,labels=activity_labels$V2)
  data
}

step_5_create_tidy_data <- function(data){
  # use reshape to calculate the means per subject and activity
  melted <- melt(data, id=c("subjectId","activity"))
  tidy   <- dcast(melted, subjectId+activity ~ variable, mean)
}

if(download_file){
  download_and_extract_files()
}

# task 1: Merges the training and the test sets to create one data set.
data <- step_1_load_and_merge_data()

# task 2: Extracts only the measurements on the mean and standard deviation for 
# each measurement.
data <- step_2_only_mean_and_std_variables(data)

# task 3: Uses descriptive activity names to name the activities in the data set
data <- step_3_set_descriptive_activities(data)

# task 4: Appropriately labels the data set with descriptive
# activity names. 
# See 1, labeling of variables already happened

# task 5: Creates a second, independent tidy data set with the
# average of each variable for each activity and each subject.
tidy <- step_5_create_tidy_data(data)

# write the tidy data set to a file
write.csv(tidy, "tidy.csv", row.names=FALSE)
