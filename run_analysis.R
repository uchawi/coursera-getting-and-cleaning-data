#Assignment: Getting and Cleaning Data Course Project

library(plyr)
library(dplyr)
library(Hmisc) 

#read data
#Header
feature<-read.table("coursera/UCI HAR Dataset/features.txt",header=FALSE,  col.names=c('id', 'name'))
#activity names
labels <- read.table("coursera/UCI HAR Dataset/activity_labels.txt", header=FALSE, col.names=c('id', 'name'))
#XTRAIN  PC
Xtrain<-read.table("coursera/UCI HAR Dataset/train/X_train.txt",header=FALSE,col.names=feature[,2],sep="")
ytrain<-read.table("coursera/UCI HAR Dataset/train/y_train.txt",header=FALSE,col.names="Activity",sep="")
subject_train<-read.table("coursera/UCI HAR Dataset/train/subject_train.txt",col.names="Subject",header=FALSE,sep="")

#XTEST PC
Xtest<-read.table("coursera/UCI HAR Dataset/test/X_test.txt",header=FALSE,col.names=feature[,2],sep="")
ytest<-read.table("coursera/UCI HAR Dataset/test/y_test.txt",header=FALSE,col.names="Activity",sep="")
subject_test<-read.table("coursera/UCI HAR Dataset/test/subject_test.txt",header=FALSE,col.names="Subject",sep="")

#Merges the training and the test sets to create one data set.
subject_merged <- rbind(subject_train, subject_test)
names(subject_merged) <- "subject"
Xmerge <- rbind(Xtest, Xtrain)
Ymerge <- rbind(ytest, ytrain)

#Extracts only the measurements on the mean and standard deviation for each measurement.

feature_selected_columns <- grep('mean\\(\\)|std\\(\\)', feature$name)
filtered_Xmerge <- Xmerge[, feature_selected_columns]
names(filtered_Xmerge) <- feature[feature$id %in% feature_selected_columns, 2]


#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
Ymerge[, 1] = labels[Ymerge[, 1], 2]
names(Ymerge) <- "activity"


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
whole_dataset <- cbind(subject_merged, Ymerge, filtered_Xmerge)
measurements <- whole_dataset[, 3:dim(whole_dataset)[2]]
tidy_dataset <- aggregate(measurements, list(whole_dataset$subject, whole_dataset$activity), mean)
names(tidy_dataset)[1:2] <- c('subject', 'activity')
write.table(tidy_dataset, "coursera/UCI HAR Dataset/final_tidy_dataset.txt", row.names=FALSE)
