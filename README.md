CourseProject
=============
## Merge data to create one data set, assuming the working directory is Desktop
## Read the names of variables and tranform it in characters
setwd("~/Desktop/UCI HAR Dataset")
fea <- read.table("features.txt")
features <- as.character(fea$V2)
## Read data from the train subgroup with correponding subjects and activities and add the column names
setwd("~/Desktop/UCI HAR Dataset/train")
train <- read.table("X_train.txt")
strain <- read.table("subject_train.txt")
atrain <- read.table("y_train.txt")
colnames(train) <- features
## Read data from the test subgroup with correponding subjects and activities and add the column names
setwd("~/Desktop/UCI HAR Dataset/test")
test <- read.table("X_test.txt")
stest <- read.table("subject_test.txt")
atest <- read.table("y_test.txt")
colnames(test) <- features
## Bind the two dataframes and add a column for the Subject variable and another for the Activity variable
## Add a descriptive name for these two columns
total <- rbind(train,test)
subject <- rbind(strain,stest)
activity <- rbind(atrain,atest)
total <- cbind(total,subject)
total <- cbind(total,activity)
names(total)[562] <- "Subject"
names(total)[563] <- "Activity"
## Extract the measurements on the mean and standard deviation 
select <- c(grep("mean",colnames(total)),grep("std",colnames(total)),562,563)
total <- total[,select]
## Use descriptive activity names to name the activities in the data set
total$Activity <- as.factor(total$Activity)
levels(total$Activity)[1:6] <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
## Appropriately labels the data set with descriptive variable names
## by substracting useless signs and lowering the letters
colnames(total) <- gsub("-","",colnames(total))
colnames(total) <- tolower(colnames(total))
colnames(total) <- sub("\\()","",colnames(total))
## creates a second, independent tidy data set with the average of each variable 
## for each activity and each subject
total2 <- aggregate(total[,1]~activity+subject,data=total,mean)
for (i in 2:79){
  tot <- aggregate(total[,i]~activity+subject,data=total,mean)
  total2 <- cbind(total2,tot[,3])
}
colnames(total2)[3:81] <- colnames(total)[1:79]
