library(plyr)
library(dplyr)

# Make sure that the files you need are accessible
needed_files <- c("test/X_test.txt", "train/X_train.txt", "test/y_test.txt", "train/y_train.txt", "test/subject_test.txt", "train/subject_train.txt", "features.txt", "activity_labels.txt")
#data_dir <- "UCI HAR Dataset"


## Files needed: X_test.txt, X_train.txt, y_test.txt,
## y_train.txt, subject_test.txt, subject_train.txt,
## features.txt, activity_labels.txt

## Check to see if they are in the working directory.
## If not, check to see if they are arranged in the same directory structure
## as found in the data set zip file by searching in 'test' and 'train'
## directories.
## If data files are not found, output an error message and halt script.
have_files_in_wd <- file.exists(needed_files)
not_found_in_wd <- which(have_files_in_wd == FALSE)
if (length(not_found_in_wd) == 0) { # In other words, all of the files were found
  
  # Read in data sets
  features <- read.table(file = "features.txt", col.names = c("id","name"))
  x_test <- read.table(file = "test/x_test.txt", col.names = features$name)
  x_train <- read.table(file = "train/x_train.txt", col.names = features$name)
  subject_test <- read.table(file = "test/subject_test.txt")
  subject_train <- read.table(file = "train/subject_train.txt")
  y_test <- read.table(file = "test/y_test.txt")
  y_train <- read.table(file = "train/y_train.txt")
  activities <- read.table(file = "activity_labels.txt", col.names = c("id", "name"))
  
  # Merge train and test data sets into one data frame
  all_test <- cbind(x_test, subject = subject_test$V1, activity = activities$name[y_test$V1])
  all_train <- cbind(x_train, subject = subject_train$V1, activity = activities$name[y_train$V1])
  all_data <- bind_rows(all_test, all_train)
  # What follows is a brute force fix because I don't have the time to study out and "correct"
  # the cause of the 562nd column name always showing up as "v1".
  colnames(all_data)[562] <- "person"
  
  # Subset the data set to only include the mean and
  # standard deviation of each measurement (plus
  # subject and activity because they were added early).
  #selectedData <- select(all_data, matches("mean|std|subject|activity"))
  keeperColumns <- c(1, 2, 3, 4, 5, 6, 41, 42, 43, 44, 45, 46, 81, 82, 83,
                     84, 85, 86, 121, 122, 123, 124, 125, 126, 161, 162, 163,
                     164, 165, 166, 201, 202, 214, 215, 227, 228, 240, 241,
                     253, 254, 266, 267, 268, 269, 270, 271, 345, 346, 347,
                     348, 349, 350, 424, 425, 426, 427, 428, 429, 503, 504,
                     516, 517, 529, 530, 542, 543, 562, 563)
  selectedData <- select(all_data, keeperColumns)
  
  #Apply descriptive variable names
  # NOTE: I restricted this to only the first 66 columns because the last two, person and activity, already have decent names.
  
  names(selectedData)[1:66] <- c("MeanBodyLinearAccelerationOnX", "MeanBodyLinearAccelerationOnY", "MeanBodyLinearAccelerationOnZ", 
                           "StdDevBodyLinearAccelerationOnX", "StdDevBodyLinearAccelerationOnY", "StdDevBodyLinearAccelerationOnZ", 
                           "MeanGravityLinearAccelerationOnX", "MeanGravityLinearAccelerationOnY", "MeanGravityLinearAccelerationOnZ", 
                           "StdDevGravityLinearAccelerationOnX", "StdDevGravityLinearAccelerationOnY", "StdDevGravityLinearAccelerationOnZ", 
                           "MeanBodyLinearAccelerationJerkOnX", "MeanBodyLinearAccelerationJerkOnY", "MeanBodyLinearAccelerationJerkOnZ", 
                           "StdDevBodyLinearAccelerationJerkOnX", "StdDevBodyLinearAccelerationJerkOnY", "StdDevBodyLinearAccelerationJerkOnZ", 
                           "MeanBodyAngularVelocityOnX", "MeanBodyAngularVelocityOnY", "MeanBodyAngularVelocityOnZ", 
                           "StdDevBodyAngularVelocityOnX", "StdDevBodyAngularVelocityOnY", "StdDevBodyAngularVelocityOnZ", 
                           "MeanBodyAngularVelocityJerkOnX", "MeanBodyAngularVelocityJerkOnY", "MeanBodyAngularVelocityJerkOnZ", 
                           "StdDevBodyAngularVelocityJerkOnX", "StdDevBodyAngularVelocityJerkOnY", "StdDevBodyAngularVelocityJerkOnZ", 
                           "MeanBodyLinearAccelerationMagnitude", "StdDevBodyLinearAccelerationMagnitude", "MeanGravityLinearAccelerationMagnitude", 
                           "StdDevGravityLinearAccelerationMagnitude", "MeanBodyLinearAccelerationJerkMagnitude", "StdDevBodyLinearAccelerationJerkMagnitude", 
                           "MeanBodyAngularVelocityMagnitude", "StdDevBodyAngularVelocityMagnitude", "MeanBodyAngularVelocityJerkMagnitude", 
                           "StdDevBodyAngularVelocityJerkMagnitude", "FastFourierMeanBodyLinearAccelerationOnX", "FastFourierMeanBodyLinearAccelerationOnY", 
                           "FastFourierMeanBodyLinearAccelerationOnZ", "FastFourierStdDevBodyLinearAccelerationOnX", "FastFourierStdDevBodyLinearAccelerationOnY", 
                           "FastFourierStdDevBodyLinearAccelerationOnZ", "FastFourierMeanBodyLinearAccelerationJerkOnX", "FastFourierMeanBodyLinearAccelerationJerkOnY", 
                           "FastFourierMeanBodyLinearAccelerationJerkOnZ", "FastFourierStdDevBodyLinearAccelerationJerkOnX", "FastFourierStdDevBodyLinearAccelerationJerkOnY", 
                           "FastFourierStdDevBodyLinearAccelerationJerkOnZ", "FastFourierMeanBodyAngularVelocityOnX", "FastFourierMeanBodyAngularVelocityOnY", 
                           "FastFourierMeanBodyAngularVelocityOnZ", "FastFourierStdDevBodyAngularVelocityOnX", "FastFourierStdDevBodyAngularVelocityOnY", 
                           "FastFourierStdDevBodyAngularVelocityOnZ", "FastFourierMeanBodyLinearAccelerationMagnitude", "FastFourierStdDevBodyLinearAccelerationMagnitude", 
                           "FastFourierMeanBodyLinearAccelerationJerkMagnitude", "FastFourierStdDevBodyLinearAccelerationJerkMagnitude", "FastFourierMeanBodyAngularVelocityMagnitude", 
                           "FastFourierStdDevBodyAngularVelocityMagnitude", "FastFourierMeanBodyAngularVelocityJerkMagnitude", "FastFourierStdDevBodyAngularVelocityJerkMagnitude")
  
  ## Create a second, independent data set
  
  ## Data is the average of each variable
  ## for each activity and each subject.
  new_data <- summarize_each(group_by(selectedData, person, activity, add = TRUE), funs(mean))
  names(new_data)[3:68] <- sapply(names(new_data)[3:68], paste0, "Mean")
  write.table(x = new_data, file = "tidy_data_set.txt", row.names = FALSE)
} else {
  cat("The following files were not found in the current working directory:\n", needed_files[not_found_in_wd])
}
