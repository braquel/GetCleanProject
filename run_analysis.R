#Tidy Up: Getting and Cleaning Data Final Project

      ##This R-code would generate tidy data set based on the findings recorded in the study- 
      ##"Human Activity Recognition Using Smartphones" (HAR) (see Code.md & README.TXT for details of the study,).
      ##The final output of this R-code is a data set which provides the average of each measurement\feature for each activity and subject from the study.
      
      ##Required Packages: library(read.table) ; library(reshape2)
      
      ##Data Source
      ##Extract data from the files found in https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
      
      ##Data Extraction 
      ##Use fread in library(read.table) package or just download the zip into the directory of project then extract the files accordingly.
      
      ##Environment Building: Import the findings of the study under the 'test' and 'train' folders. 
      ##Import only the .txt files named subject (volunteer), X (refers to features\measurement) & Y (refers to activity).
      
      ##Test data
      subject_train <- read.table("subject_train.txt")
      X_train <- read.table("X_train.txt")
      y_train <- read.table("y_train.txt")
      
      ##Train data
      subject_test <- read.table("subject_test.txt")
      X_test <- read.table("X_test.txt")
      y_test <- read.table("y_test.txt")
      
      ##Import features and activity labels to aid in correctly indetifying the elements of the dataset (see CodeBook.md for reference)
      
      featuresType <- read.table("features.txt")
      activityType <- read.table("activity_labels.txt")


##Part 1: Merge HAR Data Sets

      ###Step1: Identify and bind the variables into distinct data sets to form the subsets of the study findings
      
      subjectID<- rbind(subject_train, subject_test)
      activity<- rbind(y_train, y_test)
      features<-rbind(X_train,X_test)
      
      ####Run Dim() to check intended dimensions (obs and variables)
      
      ###Step 2: Name the columns of subset for better reference
      colnames(subjectID)<- "Subject"  ##Rename column of subject variable
      colnames(activity)<-  "Activity"  ##Rename column of activity variable
      colnames(features)<- t(featuresType[2]) ##Use the transpose function to make the features listed as column names for the observations
      
      ###Step 3: At this point, it would be easier to render descriptive activity names to the observations under the "Activity" variable\column
      
      activitytype2<- sapply(activityType, tolower) ##changes the activity types to lowercase
      activitytype2<- gsub("_", " ",activitytype2)  ##removes the underscore from the observations
      
      
      activity[,1] = activitytype2[activity[,1],2]   ##attribute the character 'name' with corresponding integer in "Activity" column


      ###Step 4: Completely merge the dataset by binding the three subsets 
      
      mergedHAR<- cbind(subjectID,activity,features)



##Part 2: Create Data Set with Mean and STD Features\Measurements Only

      ###Step 1: ##Identify and Extract the features columns that contain Mean and STD (Standard Deviation)
      
      MeanSTDColHAR <- grep(".*[Mm]ean.*|.*[Ss]td.*", names(mergedHAR)) ##Be mindful of the possible case discrepancies in column names
      
      #### There should 86 features\measurements columns that contain Mean and Std
      
      
      ###Step 2: Subset the merged HAR data sets with Mean and STD features
      ####Don't forget to include the Subject and Activity Column (1:2)
      
      HARMeanSTD<- mergedHAR[c(1:2,MeanSTDColsHAR)]
      
      ####test the dimension of the new data set
      
      dim(HARMeanSTD) ##This should show 10299 88 to include the Subject and Activity column
      
      
      ##Part 3: Provide a more reader-friendly column names for features
      
      ###Refer to Codebook.MD for details of the acronyms
      ###perform gsub to replace\substitute certain characthers\strings with more descriptive terms
      
      names(HARMeanSTD)<-gsub("-[Mm]ean()", "Mean", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("-[Ss]td()", "STD", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("^t", "Time", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("^f", "Frequency", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("tBody", "TimeBody", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("-[Ff]req()", "Frequency", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("[M]ean[Ff]req()", "MeanFrequency", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("angle", "Angle", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("gravity", "Gravity", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("Acc", "Accelerometer", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("Gyro", "Gyroscope", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("BodyBody", "Body", names(HARMeanSTD))
      names(HARMeanSTD)<-gsub("Mag", "Magnitude", names(HARMeanSTD))
      
      ####Test the changes
      names(HARMeanSTD)
      
      ####Arrange the data set by arranging the observations based on Subject. Use sort or Load dply and use arrange function 
      arrange(HARMeanSTD, Subject)


##Part 4: Extract and generate a tidy data set containing the average of each variable
      ###This data set is the final output of the exercise
      
      ###Load reshape2 package and arrange the dataset by Subject in preparation of creating a new data set
      
      library(reshape2)
      
      
      ###Step 1: Prepare the elements required in reshaping data by grouping the feature names to inidicate the varaibales to measure 
      
      
      identifiers<- names(HARMeanSTD[1:2])
      
      #### Columns 1 and 2 refers to "Subject" and "Activity" which will be used as identifiers
      
      
      ###Step 2: Set the identifiers and then melt the data set
      
      meltHARMeanSTD<- melt(HARMeanSTD, id= identifiers) 
      
      
      ####Step 3: Apply the dcast function and calculate the mean of each variable(features) by Subject and Activity
      GetTidyData<- dcast(meltHARMeanSTD, Subject+Activity ~variable, mean) ####The output is wide form
      
      
      #### Step 4: Create a file containing the independent data set 
      
      write.csv(GetTidyData, "GetTidyData.csv", row.names= FALSE) 
      
      ####If you plan to print the document, remove the row name argument for better reference