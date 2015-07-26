# Introduction
The data set used for this project is from the UCI Machine Learning Repository (Human Activity Recognition Using Smartphones Data Set). A detailed description is at : [http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#]

# Data Set Information ( as mentioned in the UCI Machine Learning repository website )
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

# Variable/Column Header Information 
1.  subject : Column named "subject" was appended to the test and train data sets from "subject_test.txt" and "subject_train.txt" respectively . Refers to the identifier of the subject who carried out the experiment.
2.  activity : Column named "activity" was appended to the test and train data sets . Maps the activity id to it's corresponding name . Created from "y_test.txt" and "y_train.txt" for the test and train data sets. Could be one of the following values :
       * WALKING
       * WALKING_UPSTAIRS
       * WALKING_DOWNSTAIRS
       * SITTING
       * STANDING
       * LAYING
3.  561 column headers specifying time and frequency domain variables for the merged test and train data sets . Information about these variables can be found in the "features.txt" and "features_info.txt" files downloaded from the UCI repository . 

# Data Set Transformations
The following transformations were performed on the data sets downloaded from the UCI repository:
  *  "subject" and "activity" were column binded to the test and train data sets
  *  "activity ids" were mapped to their corresponding activity names
  *   the training and the test data sets with the subject and activity columns appended were merged into one data set using rbind .
  *   mean and standard deviation measurements were extracted from the merged test and train data sets for the column headers specifying the time and frequency domain variables . 
  *   a new tidy data set with the average of each variable for each activity and each subject was created by melting and casting the pre processed and merged test and train data sets . 
