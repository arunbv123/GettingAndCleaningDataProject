library(plyr)


dataFile <- "uci_har_dataset.zip"

# Download the dataset from link

if (!file.exists(dataFile)){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl,dataFile)
}


#Unzip the dataset
if (!file.exists("UCI HAR Dataset")) { 
        unzip(dataFile) 
}

# 1. Merges the training and the test sets to create one data set.

        #read train data
        X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
        Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
        subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        
        #read test data
        X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
        Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
        subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        
        #merge x data
        X_data <- rbind(X_train,X_test)
        
        #merge y data
        Y_data <- rbind(Y_train,Y_test)
        
        #merge subject data
        subject_data <- rbind(subject_train,subject_test)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
        
        #read features list
        features <- read.table("./UCI HAR Dataset/features.txt")
        #extract only the measurements on the mean and standard deviation
        feature_extract <- grep("-(mean|std)\\(\\)", features$V2)
        X_data <- X_data[, feature_extract]
        names(X_data) <- features[feature_extract,2]
        
# 3. Uses descriptive activity names to name the activities in the data set
        
        #read activity list
        activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
        #Name the activities
        Y_data[, 1] <- activity_labels[Y_data[, 1], 2]
        names(Y_data) <- "activityName"
        
# 4. Appropriately labels the data set with descriptive variable names
        
        names(subject_data) <- "subjectId"
        #construct final dataset
        final_data <- cbind(subject_data,Y_data,X_data)
        #save dataset in text file
        write.table(final_data, "mean_std_data.txt", row.name=FALSE)
        
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        
        fn <- function(x) colMeans(x[, 3:68])
        #find mean of each variable for each activity
        tidy_data <- ddply(final_data, .(subjectId, activityName), fn )
        #save data in text file
        write.table(tidy_data, "tidy_data.txt", row.name=FALSE)
        
        
        


