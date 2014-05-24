
## A Function read the data based on the file name and path and get the x data and y data and subject data.
## The function will be called later twice once, for reading the test data and once for the training data

setwd("/Users/Ashwin/Documents/Ashwin Docs/Coursera/Data Science/Getting and Cleaning Data/UCI HAR Dataset")

readData <- function(file_name, path) {
 
  ## The files with the prefix y_test or y_train has one column and the column name is not there in the file. We read this filepath into a variable and then read the data into a
  ## variable for storing the y_data. Since the data doesn't have column names we add the column name "Activity_ID"
  fpath <- file.path(path, paste0("y_", file_name, ".txt"))
  y_data <- read.table(fpath, header=F, col.names=c("Activity_ID"))
  
  ## Read the path for the subject files and then read the file into a dataframe and append the column name Subject_ID
  fpath <- file.path(path, paste0("subject_", file_name, ".txt"))
  subject_data <- read.table(fpath, header=F, col.names=c("Subject_ID"))
  
  # We will read the column names from the features.txt. The file has two columns and only the second column is relevant.
  data_cols <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
  
  # We will read the X_test or X_train file data into a variable called data and then append the column names using the data_cols variable shown above
  fpath <- file.path(path, paste0("X_", file_name, ".txt"))
  data <- read.table(fpath, header=F, col.names=data_cols$MeasureName)
  
  # Since the data_cols variable has all the columns and we are only concerned only with the mean and std deviation we will now use the Grep variable to select only the mean and std dev columns
  subset_data_cols <- grep(".*mean\\(\\)|.*std\\(\\)", data_cols$MeasureName)
  
  # subset the data (done early to save memory)
  data <- data[,subset_data_cols]
  
  # append the activity id and subject id columns
  data$Activity_ID <- y_data$Activity_ID
  data$Subject_ID <- subject_data$Subject_ID
  
  # return the data
  data
}

# read test data set, in a folder named "test", and data file names suffixed with "test"
readTestData <- function() {
  readData("test", "test")
}

# read test data set, in a folder named "train", and data file names suffixed with "train"
readTrainData <- function() {
  readData("train", "train")
}

# Merge both train and test data sets
# Also make the column names nicer
mergeData <- function() {
  data <- rbind(readTestData(), readTrainData())
  cnames <- colnames(data)
  cnames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
  cnames <- gsub("\\.+std\\.+",  cnames, replacement="Std")
  colnames(data) <- cnames
  data
}

# Add the activity names as another column
applyActivityLabel <- function(data) {
  activity_labels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("Activity_ID", "ActivityName"))
  activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
  data_labeled <- merge(data, activity_labels)
  data_labeled
}

# Combine training and test data sets and add the activity label as another column
getMergedLabeledData <- function() {
  applyActivityLabel(mergeData())
}

# Create a tidy data set that has the average of each variable for each activity and each subject.
getTidyData <- function(merged_labeled_data) {
  library(reshape2)
  
  # melt the dataset
  id_vars = c("Activity_ID", "ActivityName", "Subject_ID")
  measure_vars = setdiff(colnames(merged_labeled_data), id_vars)
  melted_data <- melt(merged_labeled_data, id=id_vars, measure.vars=measure_vars)
  
  # recast 
  dcast(melted_data, ActivityName + Subject_ID ~ variable, mean)    
}

# Create the tidy data set and save it on to the named file
createTidyDataFile <- function(fname) {
  tidy_data <- getTidyData(getMergedLabeledData())
  write.table(tidy_data, fname)
}


createTidyDataFile("tidy.txt")
