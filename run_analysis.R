#packages required to run this script correctly
require(plyr)

#downloads the zip file into a temporary directory
#then extracts all of the files into the temporary directory
temp <- tempdir()
tempDir <- paste(temp, "\\temp.zip", sep="")
zipURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(zipURL, tempDir)
unzip(tempDir, exdir=temp)

#reads in the 8 data frames to be combined, into separate objects
#creates a vector column names (labels) from the y_train/test data frames
subject_train <- read.table(paste(temp, "\\UCI HAR Dataset", "\\train", "\\subject_train.txt", sep=""))
x_train <- read.table(paste(temp, "\\UCI HAR Dataset", "\\train", "\\X_train.txt", sep=""))
y_train <- read.table(paste(temp, "\\UCI HAR Dataset", "\\train", "\\y_train.txt", sep=""))
train_labels <- y_train[,1]

subject_test <- read.table(paste(temp, "\\UCI HAR Dataset", "\\test", "\\subject_test.txt", sep=""))
x_test <- read.table(paste(temp, "\\UCI HAR Dataset", "\\test", "\\X_test.txt", sep=""))
y_test <- read.table(paste(temp, "\\UCI HAR Dataset", "\\test", "\\y_test.txt", sep=""))

activity_labels <- read.table(paste(temp, "\\UCI HAR Dataset", "\\activity_labels.txt", sep=""))
features <- read.table(paste(temp, "\\UCI HAR Dataset", "\\features.txt", sep=""))

#creates column names, and forms a data frame from the "test" data
colnames(x_test) <- features[,2]
colnames(subject_test) <- "subjectID"
colnames(y_test) <- "activityType"
test <- cbind(subject_test, y_test, x_test)

#creates column names, and forms a data frame from the "train" data
colnames(x_train) <- features[,2]
colnames(subject_train) <- "subjectID"
colnames(y_train) <- "activityType"
train <- cbind(subject_train, y_train, x_train)

#creates one big data set, that combines the data from both "train" and "test" sets
#arranges the rows based firstly on the subject id, and then secondly on activity
#converts the activity variable into a factor variable, which more clearly describes each activity
all_data <- rbind(test, train)
all_data <- arrange(all_data, subjectID, activityType)
all_data$activityType <- factor(x=all_data[,2], labels=activity_labels[,2])

#subsets only the columns dealing with data on calculating the mean or standard deviation
data_names <- names(all_data)
mean_sd_index <- c(1:2, (grep("mean", data_names, ignore.case=TRUE)), (grep("std", data_names, ignore.case=TRUE)))
mean_and_sd <- all_data[,mean_sd_index]

#tidies the variable names
temp_names <- names(mean_and_sd)
#removes the excess punctuation
temp_names <- gsub("-", "", temp_names)
temp_names <- gsub("\\(", "", temp_names)
temp_names <- gsub("\\)", "", temp_names)
temp_names <- gsub(",", "", temp_names)
#capitalises words for easier reading 
temp_names <- gsub("std", "Std", temp_names)
temp_names <- gsub("mean", "Mean", temp_names)
temp_names <- gsub("gravity", "Gravity", temp_names)
#renames the variables with the cleaned up column names
colnames(mean_and_sd) <- temp_names 

#creates a new data frame to store the new averages data
var_avg <- matrix(nrow=180, ncol=88)
var_avg <- data.frame(var_avg)
colnames(var_avg) <- temp_names
var_avg[,1] <- rep(1:30, each=6)
var_avg[,2] <- rep(activity_labels[,2], times=30)

#creates a loop that looks over each row the new data frame
#records the subject id and activity name in temporary variables
#then extracts all values from the "mean_and_sd" data set that corresponds to this id and activity name
#it calculates the mean from these values, and stores it in the new "var_avg" data frame
#it loops over every variable for that row, before moving onto the next row

for(x in 1:180) {

    temp_id <- var_avg[x,1]
    temp_activity <- var_avg[x, 2] 
    temp_extract <- mean_and_sd[(mean_and_sd$subjectID==temp_id & mean_and_sd$activityType==temp_activity), ]

        for(y in 3:88) {
            var_avg[x, y] <- mean(temp_extract[,y])

    }
}

#if you wish to write any of the data sets as a table, to your working directory, simply uncomment the relevant line of code.
#by default this has been disabled, so that unwanted files are not unexpectedly created within the users local computer
#
#write.table(all_data, "all_data.txt", row.names=FALSE)
#write.table(mean_and_sd, "mean_and_sd_data.txt", row.names=FALSE)
#write.table(var_avg, "average_variables_data.txt", row.names=FALSE)

