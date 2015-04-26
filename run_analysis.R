library(dplyr)
library(data.table)
library(tidyr)
#getwd()
#I use backslash when setting working directory because I use Windows  
# setwd("C:\\Users\\Chao\\Documents")

#Step 1: download the file and put it in one folder

if(!file.exists("./data")) {
    dir.create("./data")
  
}

# note: I am using R on Windows. There is error warning when I have the argument
# of method = "curl". It works well when I remove it. However, if you are running 
# R on Mac, you may need to set method = "curl"

fileUrl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile= "./data/UCI HAR Dataset.zip")

# step 2: unzip the downloaded file
unzip(zipfile="./data/UCI HAR Dataset.zip",exdir="./data")


# step 3: read data from files


subtest<- read.table("./data/UCI HAR Dataset/test/subject_test.txt",header =FALSE)
subtrain<- read.table("./data/UCI HAR Dataset/train/subject_train.txt",header =FALSE)
xtest<- read.table("./data/UCI HAR Dataset/test/X_test.txt",header =FALSE)
xtrain<- read.table("./data/UCI HAR Dataset/train/X_train.txt",header =FALSE)
ytest<- read.table("./data/UCI HAR Dataset/test/y_test.txt",header =FALSE)
ytrain<- read.table("./data/UCI HAR Dataset/train/y_train.txt",header =FALSE)
feature<-read.table("./data/UCI HAR Dataset/features.txt",header =FALSE)
actlabels<-read.table("./data/UCI HAR Dataset/activity_labels.txt",header =FALSE)

subtest1<- tbl_df(subtest)
subtrain1<- tbl_df(subtrain)
xtest1<- tbl_df(xtest)
xtrain1<- tbl_df(xtrain)
ytest1<- tbl_df(ytest)
ytrain1<- tbl_df(ytrain)
feature1<-tbl_df(feature)
actlabels1<-tbl_df(actlabels)
rm("subtest")
rm("subtrain")
rm("xtest")
rm("xtrain")
rm("ytest")
rm("ytrain")
rm("feature")
# PART 1: Merges the training and the test sets to create one data set.

sub<-bind_rows(subtest1,subtrain1)
X<-bind_rows(xtest1,xtrain1)
Y<-bind_rows(ytest1,ytrain1)


sub
X
Y
#feature1

# according to the picture posted by Community TA David Hood

names(sub)<-c("subject")
names(Y)<-c("activity")
# note: here the V in v2 is capital.
names(X)<-feature1$V2

DATA<-cbind(X,sub,Y)
DATA1<-tbl_df(DATA)
DATA1
rm("DATA")

# PART 2: Extracts only the measurements on the mean and 
# standard deviation for each measurement. 
feature1

#?grep

subfeature<-feature1$V2[grep("mean\\(\\)|std\\(\\)", feature1$V2)]
subfeature



# selectedData <- select(DATA1,contains("mean|std"),contains("subject"),constains("activity"))

selectedNames <- c(as.character(subfeature),"subject","activity")
selectedData <-subset(DATA1,select= selectedNames)


str(selectedData)


#PART 3:  Uses descriptive activity names to name the activities in the data set

setnames(Y, "V1", "activityNum")
##enter name of activity into dataTable
selectedData <- merge(actlabels, selectedData , by="activityNum", all.x=TRUE)
selectedData$activityNum <- as.character(selectedData$activityNum)

## create dataTable with variable means sorted by subject and Activity

dataAggr<- aggregate(. ~ subject - activityNum, data = selectedData, mean) 
selectedData<- tbl_df(arrange(dataAggr,subject,activityNum))



#PART 4:  Appropriately labels the data set with descriptive variable names. 
#?gsub
#gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
#fixed = FALSE, useBytes = FALSE)
names(selectedData)<-gsub("^t", "time", names(selectedData))
names(selectedData)<-gsub("^f", "frequency", names(selectedData))
names(selectedData)<-gsub("Acc", "Accelerometer", names(selectedData))
names(selectedData)<-gsub("Gyro", "Gyroscope", names(selectedData))
names(selectedData)<-gsub("Mag", "Magnitude", names(selectedData))
names(selectedData)<-gsub("BodyBody", "Body", names(selectedData))

names(selectedData)


#PART 5:  From the data set in step 4, creates a 
#second, independent tidy data set with the average 
#of each variable for each activity and each subject.

library(plyr);
Data5<-aggregate(. ~subject + activity, selectedData, mean)
Data5<-Data5[order(Data5$subject,Data5$activity),]
write.table(Data5, file = "tidydata.txt",row.name=FALSE)

# After outputting the required data set, we can find it in the default
# working directory
