
myfunction <- function() {
  
  download_data("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
  data <- merge_data("UCI HAR Dataset")
  data <- extract_mean_and_std(data)
  data <- labels_data(data)
}

writedata <- function(data) {
  
  averagetidy <- createaveragedata(data)
  a <- "averagetidy.txt"
  print(sprintf("Writing average tidy data set to file: %s", a))
  write.table(averagetidy, file=a, row.names=FALSE)
}

merge_data <- function(directory) {

  print("Merging training and test data sets")
  train <- merge_internal_data(directory, "train")
  test <- merge_internal_data(directory, "test")
  rbind(train, test)
}

merge_internal_data <- function(directory, dataset) {
  print(paste("--> Merging internal data set: ", dataset, sep=""))
  subjects <- read.table(file.path(directory, dataset,
                                   paste("subject_", dataset, ".txt", sep="")))
  activities <- read.table(file.path(directory, dataset,
                                     paste("y_", dataset, ".txt", sep="")))
  features <- read.table(file.path(directory, dataset,
                                   paste("x_", dataset, ".txt", sep="")))
  cbind(subjects, activities, features)
}

extract_mean_and_std <- function(data){

  print("Extracting data about mean and std")
  data[-c(9:42, 49:82, 89:122, 129:162, 169:202, 205:215, 218:228, 231:241, 244:254,
          257:267, 274:346, 353:425, 432:504, 507:517, 520:530,
          533:543, 546:563)]
}

labels_data <- function(data) {
  
  print("Labeling data")
  print("--> Giving column names")
  colnames(data) <- get_colnames()
  print("--> Replacing activity ID by names")
  data$ACTIVITIES <- as.character(data$ACTIVITIES)
  data$ACTIVITIES[data$ACTIVITIES == 1] <- "WALKING"
  data$ACTIVITIES[data$ACTIVITIES == 2] <- "WALKING_UPSTAIRS"
  data$ACTIVITIES[data$ACTIVITIES == 3] <- "WALKING_DOWNSTAIRS"
  data$ACTIVITIES[data$ACTIVITIES == 4] <- "SITTING"
  data$ACTIVITIES[data$ACTIVITIES == 5] <- "STANDING"
  data$ACTIVITIES[data$ACTIVITIES == 6] <- "LAYING"
  data
}

create_average_data_set <- function(data) {

  result <- matrix(ncol=ncol(data))
  for (subject in c(1:30)){
    for (activity in c("WALKING",
                       "WALKING_UPSTAIRS",
                       "WALKING_DOWNSTAIRS",
                       "SITTING",
                       "STANDING",
                       "LAYING")) {
      sub <- subset(data, data$SUBJECTS == subject &
                      data$ACTIVITIES == activity)
      v <- c(subject, activity, t(colMeans(sub[3:ncol(data)])))
      result <- rbind(result, v[1:ncol(data)])
    }
  }
  result <- result[-1,]
  colnames(result) <- get_colnames()
  result
}

get_colnames <- function() {
  
  c("SUBJECTS",
    "ACTIVITIES",
    "BodyAccX_Mean",
    "BodyAccY_Mean",
    "BodyAccZ_Mean",
    "BodyAccX_Std",
    "BodyAccY_Std",
    "BodyAccZ_Std",
    "GravityAccX_Mean",
    "GravityAccY_Mean",
    "GravityAccZ_Mean",
    "GravityAccX_Std",
    "GravityAccY_Std",
    "GravityAccZ_Std",
    "BodyAccJerkX_Mean",
    "BodyAccJerkY_Mean",
    "BodyAccJerkZ_Mean",
    "BodyAccJerkX_Std",
    "BodyAccJerkY_Std",
    "BodyAccJerkZ_Std",
    "BodyGyroX_Mean",
    "BodyGyroY_Mean",
    "BodyGyroZ_Mean",
    "BodyGyroX_Std",
    "BodyGyroY_Std",
    "BodyGyroZ_Std",
    "BodyGyroJerkX_Mean",
    "BodyGyroJerkY_Mean",
    "BodyGyroJerkZ_Mean",
    "BodyGyroJerkX_Std",
    "BodyGyroJerkY_Std",
    "BodyGyroJerkZ_Std",
    "BodyAccMag_Mean",
    "BodyAccMag_Std",
    "GravityAccMag_Mean",
    "GravityAccMag_Std",
    "BodyAccJerkMag_Mean",
    "BodyAccJerkMag_Std",
    "BodyGyroMag_Mean",
    "BodyGyroMag_Std",
    "BodyGyroJerkMag_Mean",
    "BodyGyroJerkMag_Std",
    "BodyAccX_Mean",
    "BodyAccY_Mean",
    "BodyAccZ_Mean",
    "BodyAccX_Std",
    "BodyAccY_Std",
    "BodyAccZ_Std",
    "BodyAccJerkX_Mean",
    "BodyAccJerkY_Mean",
    "BodyAccJerkZ_Mean",
    "BodyAccJerkX_Std",
    "BodyAccJerkY_Std",
    "BodyAccJerkZ_Std",
    "BodyGyroX_Mean",
    "BodyGyroY_Mean",
    "BodyGyroZ_Mean",
    "BodyGyroX_Std",
    "BodyGyroY_Std",
    "BodyGyroZ_Std",
    "BodyAccMag_Mean",
    "BodyAccMag_Std",
    "BodyAccJerkMag_Mean",
    "BodyAccJerkMag_Std",
    "BodyGyroMag_Mean",
    "BodyGyroMag_Std",
    "BodyGyroJerkMag_Mean",
    "BodyGyroJerkMag_Std")
}
