##### READING DATA from R Working directory

# STEP 1: Reading data from "UCI HAR Dataset"
#	1)activity_labels.txt
#	2)features.txt 

Features <- read.table("UCI HAR Dataset/features.txt")
Activities <- read.table("UCI HAR Dataset/activity_labels.txt")

# STEP 2: Reading TEST folder files
#	1) subject_test.txt 
#	2) X_test.txt 
#	3) Y_test.txt

TestST <- read.table("UCI HAR Dataset/test/subject_test.txt")
TestX  <- read.table("UCI HAR Dataset/test/X_test.txt")
TestY  <- read.table("UCI HAR Dataset/test/y_test.txt")

# STEP 3: Reading TRAIN folder files
#	1) subject_train.txt 
#	2) X_train.txt 
#	3) Y_train.txt

TrainST <- read.table("UCI HAR Dataset/train/subject_train.txt")
TrainX  <- read.table("UCI HAR Dataset/train/X_train.txt")
TrainY  <- read.table("UCI HAR Dataset/train/y_train.txt")

# STEP 4: ROW BIND simillar data from TEST and TRAIN folders
#	X_test with X_train
#	Y_test with Y_train
#	subject_test with subject_train

ST <- rbind(TestST,TrainST)
X  <- rbind(TestX,TrainX)
Y  <- rbind(TestY,TrainY)

# STEP 4: CHANGING COLUMN NAMES
#	Rename Columns of X with names from features.txt file		
#	Rename Column  of Y as "Activity_Name"
#	Rename Column  of ST as "Volunteer_ID"

names(X) <- Features[ ,2]
names(Y) <- "Activity_Name"
names(ST) <- "Volunteer_ID"

# STEP 5: Change ROW Values of Y data frame as mentioned in "activity_labels.txt"
#	1 - WALKING

#	2 - WALKING_UPSTAIRS

#	3 - WALKING_DOWNSTAIRS

#	4 - SITTING

#	5 - STANDING

#	6 - LAYING


Activity_Names <- unique(Y[ ,1])
len <- length(Activity_Names)
for(i in 1:len) { Y$Activity_Name[Y$Activity_Name == i] <- as.character(Activities[i,2]) }

# STEP 6: Filter the MEAN and STANDARD DEVIATION columns from X dataset
#	and create new data set "X_MeanAndStd_Cols"

MeanCols <- grep("mean()",names(X))
StdCols <- grep("std()",names(X))
MeanAndStdCols <- c(MeanCols,StdCols)
X_MeanAndStd_Cols <- X[ ,MeanAndStdCols]

# STEP 7: COLUMN BIND Subject, X and Y data sets to form MergedDF data set
#	Order it by Volunteer ID and Activity name columns

MergedDF <- cbind (ST,Y,X_MeanAndStd_Cols)
OrderedDF <- MergedDF[with(MergedDF,order(MergedDF$Volunteer_ID,MergedDF$Activity_Name)),]

# STEP 8: FIND MEAN of each variable for each activity(Activity_Name) and each subject (Volunteer_ID)
#	Write the data set to R workign directory as TidyDataSet.txt

TempDF <- cbind (X_MeanAndStd_Cols,ST,Y)
MeanDF <- aggregate(x=TempDF, by=list(Volunteer_ID=TempDF$Volunteer_ID ,Activity_Name=TempDF$Activity_Name), FUN=mean)
TidyDF <- MeanDF[ ,1:81]

write.table(TidyDF,"TidyDataSet.txt",row.name=FALSE)

