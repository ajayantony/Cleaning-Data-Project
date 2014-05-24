#1. Create a R Script with the name run_analysis.R
# Check if datafile exists, if not download and unzip
#--------------------------------------------------------------------------------------
if(file.exists("./UCI HAR Dataset")==FALSE)
{
  link <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  print("Please wait, the file being downloaded is about ~60MB compressed")
  download.file(link,destfile="./Samsung.zip")
  unzip("Samsung.zip")
  unlink("Samsung.zip", recursive = T)
  print("Download Complete.")
}else {
  print("UCI HAR Dataset already exists, proceeding using this dataset")
}
#--------------------------------------------------------------------------------------

#2. First Task - Merging the training and the test sets to create one data set.
# a) Name the variables in file train/X_train.txt using features.txt, check length is same both files before naming.
#----------------------------------------------------------------------------------------

#Reading features.txt into vector
features <- read.table("./UCI HAR Dataset/features.txt",sep=" ")
features_v <- features[,2]

#Reading X_train.txt 
print("loading Training Measures,please wait")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt",sep="",stringsAsFactors=F)
print("Training Measures loaded")

#Checking if length of datasets match
if(length(features_v)!=length(X_train[1,]))
{
  print("features.txt file does not match X_train.txt, try running program again by deleting
        UCI HAR Dataset in your working directory")
  unlink("UCI HAR Dataset", recursive = T)
  stop()
}else {
#if matched assigning features as names.
  trainData <- X_train
  names(trainData) <- features_v
}

#Name variable as 'Subject' in subject_train.txt and 'ActivityID' in y_train.txt
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
names(subjectTrain) <- "Subject"

activityID <- read.table("./UCI HAR Dataset/train/y_train.txt")
names(activityID) <- "ActivityID"

#Merge the files in Training Set, cbind() - subject_train.txt,y_train.txt,X_train.txt
if(length(X_train[,1])==length(subjectTrain[,1]) && length(X_train[,1])==length(activityID[,1]))
{
  
  #comparing number of observations, if matched binding datasets
  print("Merging subject_train.txt,y_train.txt,X_train.txt")
  SampleType <- rep("Training",times=length(subjectTrain[,1]))
  trainDataM <- cbind(SampleType,subjectTrain,activityID,trainData)
  print("Merging complete")
}else {
  print("subject_train.txt,y_train.txt,X_train.txt do not match, try running program again by deleting
        UCI HAR Dataset in your working directory")
  unlink("UCI HAR Dataset", recursive = T)
  stop()
  }

# b) Merging Test files.
#----------------------------------------------------------------------------------------


#Reading X_test.txt 
print("loading Test Measures,please wait")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt",sep="",stringsAsFactors=F)
print("Test Measures loaded")

#Checking if length of datasets match
if(length(features_v)!=length(X_test[1,]))
{
  print("features.txt file does not match X_test.txt, try running program again by deleting
        UCI HAR Dataset in your working directory")
  unlink("UCI HAR Dataset", recursive = T)
  stop()
  }else {
  #if matched assigning features as names.
  testData <- X_test
  names(testData) <- features_v
}

#Name variable as 'Subject' in subject_test.txt and 'ActivityID' in y_test.txt
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
names(subjectTest) <- "Subject"

activityID <- read.table("./UCI HAR Dataset/test/y_test.txt")
names(activityID) <- "ActivityID"

#Merge the files in Test Set, cbind() - subject_test.txt,y_test.txt,X_test.txt
if(length(X_test[,1])==length(subjectTest[,1]) && length(X_test[,1])==length(activityID[,1]))
{
  
  #comparing number of observations, if matched binding datasets
  print("Merging subject_test.txt,y_test.txt,X_test.txt")
  SampleType <- rep("Test",times=length(subjectTest[,1]))
  testDataM <- cbind(SampleType,subjectTest,activityID,testData)
  print("Merging complete")
}else {
  print("subject_test.txt,y_test.txt,X_test.txt do not match, try running program again by deleting
        UCI HAR Dataset in your working directory")
  unlink("UCI HAR Dataset", recursive = T)
  stop()
}

print("Vertical Merging of Training and Test datasets")
mergedataB <- rbind(trainDataM,testDataM)
print("Training and Test datasets merged")

#--------------------------------------------------------------------------------------------
#3. Extracts only the measurements on the mean and standard deviation for each measurement. 
#Extract all measures with mean, std in its name along with SampleType,Subject and Activity ID 
#variable and create a data frame.

print("Extracting Mean and STD variables")
mnStdNames <- names(mergedataB)
mnStNamesB <- grep(c("MEAN|STD"),mnStdNames,ignore.case = T)
mergedataMS <- mergedataB[,mnStNamesB]
print("Extraction Complete")

#--------------------------------------------------------------------------------------------
#4. Uses descriptive activity names to name the activities in the data set
#Merge data frame from step 3, with file activity_labels.txt on 'ActivityID', 
#Name this new field 'Activity Name'

lenMS <- length(mergedataMS[1,])
activityLabelsDF <- read.table("./UCI HAR Dataset/activity_labels.txt",sep="")
names(activityLabelsDF) <- c("ActivityID","Activity")
tempDF <- mergedataB[,1:3]

#Joining Datasets
lbforeM <- length(tempDF[,1])
tempDFA <- merge(tempDF,activityLabelsDF,by.x="ActivityID",by.y="ActivityID")
laftM <- length(tempDFA[,1])

#Validating merge success
if(lbforeM == laftM)
{
  #binding datasets, the merged dataset has Activity Label
  print("Merging")
  mergedataA <- cbind(tempDFA,mergedataMS)
  print("Merge with activity_labels.txt successfull")
}else 
{
  print("Merge with activity_labels.txt failed, verify dataset")
  unlink("UCI HAR Dataset", recursive = T)
  stop()
}

#Writing Dataset
print("Writing Dataset Activity -TrainTest_MnStd_wAct.txt")
if(file.exists("./analysis_out")==TRUE)
{
  unlink("analysis_out", recursive = T)
  dir.create("./analysis_out")
}else 
{
  dir.create("./analysis_out")
}
write.table(mergedataA,"./analysis_out/TrainTest_MnStd_wAct.txt",sep=" ",quote=F,row.names = F)
print("Writing Complete")
#Verify written data
tmpCk <- read.table("./analysis_out/TrainTest_MnStd_wAct.txt",header=T)

if (length(tmpCk[,1]) == length(mergedataA[,1]) && length(tmpCk[1,]) == length(mergedataA[1,]))
{
  print("Dataset written to analysis_out/TrainTest_MnStd_wAct.txt successfully")
}else 
{
  print("Dataset writting failed. Re-Run program by deleting dataset directory - UCI HAR Dataset")
  unlink("UCI HAR Dataset", recursive = T)
  stop()
}
#--------------------------------------------------------------------------------------------
#5. Creates a second, independent tidy data set with the average of each variable 
#for each activity and each subject. 

#Creating factor for activity
factrs1 <- factor(mergedataA[,4])
factrs2 <- factor(mergedataA[,3])

library("plyr")
dft <- mergedataA[,3:length(mergedataA[1,])]
mergedataF <- ddply(.data=dft, .(Activity,Subject),colwise(mean))

#Writing Dataset
print("Writing Dataset Mean by Activity,Subject -TrainTest_Avg_ByActSub.txt")
write.table(mergedataF,"./analysis_out/TrainTest_Avg_ByActSub.txt",sep=" ",quote=F,row.names = F)
print("Writing Complete")
#Verify written data
tmpCk <- read.table("./analysis_out/TrainTest_Avg_ByActSub.txt",header=T)

if (length(tmpCk[,1]) == length(mergedataF[,1]) && length(tmpCk[1,]) == length(mergedataF[1,]))
{
  print("Dataset written to analysis_out/TrainTest_Avg_ByActSub.txt successfully")
}else 
{
  print("Dataset writting failed. Re-Run program by deleting dataset directory - UCI HAR Dataset")
  unlink("UCI HAR Dataset", recursive = T)
  stop()
}

