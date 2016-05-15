##Make a train dataset

#Import all the columns of the dataset
sub_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
x_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")

#Import colnames
features<-read.table("./UCI HAR Dataset/features.txt")

#Activity reference list
act_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")
colnames(act_labels)=c("activity","label")

#Concatenate the columns of the dataset and change the colnames
train<-data.frame()
train<-cbind(sub_train,y_train,x_train)
colnames(train)=c("subject","activity",as.character(features[,2]))

#Replace the activity with labels
labeled=merge(train, act_labels, by=intersect(names(train),names(act_labels)))
train[,2]=labeled$label
rm(labeled)

##Make a test dataset

#Import all the columns of the dataset
sub_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
x_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")

#Concatenate the columns of the dataset and change the colnames
test<-data.frame()
test<-cbind(sub_test,y_test,x_test)
colnames(test)=c("subject","activity",as.character(features[,2]))

#Replace the activity with labels
labeled=merge(test, act_labels, by=intersect(names(test),names(act_labels)))
test[,2]=labeled$label
rm(labeled)

##Merge train dataset with test dataset by subject
merged=rbind(train,test)
Total=merged[order(merged$subject,merged$activity),]

##Extracts only the measurements on the mean and standard deviation for each measurement.
MeanStd<-cbind(Total[,1:2],Total[,grepl("[Mm]ean|[Ss]td",colnames(Total))])

##Creates a second, independent tidy data set with the average of each variable for each activity and each subject
library(dplyr)
avg.group<-data.frame(MeanStd)
sum<-avg.group%>% group_by(subject,activity) %>% summarize_each(funs(mean))

##Export datasets
write.csv(Total,"train_test.csv")
write.csv(MeanStd,"MeanStd.csv")
write.csv(sum,"Group_Average.csv")
