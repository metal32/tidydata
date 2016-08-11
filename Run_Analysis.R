run_analysis<-function(){

##Import the test data  
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")

##Import the training set
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")

## First task, combining the test set and training set
X_full<-rbind(X_test,X_train)
Y_full<-rbind(Y_test,Y_train)
subject_full<-rbind(subject_test, subject_train)

##Giving names to each of the column of X_full
features <- read.table("./UCI HAR Dataset/features.txt")
colnames(X_full)<-features[,2]

## Second task, Extracts only the measurements on the mean and 
## standard deviation for each measurement.
## using logical function grepl finding the columnes having partially mean or std written

rightcols<- grepl("mean()",colnames(X_full)) | grepl("std()",colnames(X_full))
X_mean_std<-X_full[,rightcols]

## Third task, uses descriptive activity names to name the activities in the data set
activities<-read.table("./UCI HAR Dataset/activity_labels.txt")
str(activities)
##before identifying in the set we have to map activites with the labels so 
##to predict the activities of sets correctly.
library(plyr)
str(Y_full)
Y_activity<-as.factor(Y_full[,1])
str(Y_activity)
Y_activity<-mapvalues(Y_activity,from = activities[,1],to=as.character(activities[,2]))

##Y_activity is a factor with 6 levels of activity with labels of x set, so now we can 
## combine Y_activity to X_mean_std

X_mean_std <- cbind(Y_activity, X_mean_std)

## Fourth task to give descriptive name to the activity column
## as we have already added the activity labels in the dataset, we can rename the column 
colnames(X_mean_std)[1] <- "activity"

##Fifth tast, we have to take average of all the variables for 
## each subject and activity.
##So first add subject set to X_mean_std so that we have the whole data.

X_mean_std <- cbind(subject_full, X_mean_std)
colnames(X_mean_std)[1] <- "subject"

##Now we have to reshape our data by using library(reshape2)
library(reshape2)
X_melt<-melt(X_mean_std,id.vars = c("subject","activity"))

##Reqired dataset
X_tidy <- dcast(X_melt, subject + activity ~..., mean)

return(X_tidy)
}

