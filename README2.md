run_analysis.R
Version 0.9
========================================================
Cameron W. Ferguson
Coursera-Get and Clean Data
Feb 2nd to Mar 1st class.
========================================================
This script was written to complete the course project for Getting and Cleaning Data,
a John Hopkins Sponsered Coursera Course.The instructions for this assignment where as follows:
 
1)Merges the training and the test sets to create one data set.
2)Extracts only the measurements on the mean and standard deviation for each measurement. 
3)Uses descriptive activity names to name the activities in the data set
4)Appropriately labels the data set with descriptive variable names. 
5)From the data set in step 4, creates a second, independent tidy data set with 
  the average of each variable for each activity and each subject.

In order for this script to work the User must have the following data set:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

These were originally obtained from:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

These data were collected from accelerometers from the Samsung Galaxy S smartphone. To find out
more about the raw data, visit the above listed website or view the README that accomanies the 
raw data download. 

======================================================
Script Description:

** if you wish to specify a name for the outlet file, create a variable:
	file_name <- "your chosen file name.txt"

Below is a step by step description of the script. First however a quick summary:

The script starts by loading relevant files and libraries, and binding related files together. 
Along the way the script constantly removes unneeded files/values/ and variables to limit memory use.
Next it determines which measearments are needed, and creates a column list, which it uses to 
extract only the relevant measurements and their associated names. Finally it adds all the 
data files together, all with descriptive names for both the Activities and the measurements.
It take this chopped data and calculates the Mean of each measurement for every activity and subject:
ie For Walking, Subject 2, it calculates the mean of all 66 measurements that related to mean or Standard Deviation.
It writes the output of this to a .txt file called by default "clean_tidy" or if you specify a file_name
that name.txt

# This script does the following things:

# 1) Checks for existance of UCI HAR Dataset folder in the current WD. 
# 2) Loads required packages. See Code Book and Read Me for more info.
# 3) Reads in all the data containing files from the Dataset. See Code book for info.
# 4) Binds together the test and train data:
	# a) binds together the measurement data
	# b) binds together the activity data
	# c) binds together the subject data
# 5) removes used files
# 6) finds the features that contain mean() or std(), and makes a sorted list of the column numbers
# 7) Uses that list to extract just those variables from the measurement data
# 8) Makes a list of variable names by using the column numbers list again.
# 9) Adds column names to the remaining four data/label files:
	# a) selc_measurement_data: column names now represent the measurements
	# b) activity_data: simiply labeled with "Activity" to enable merging later
	# c) subject_data: Labeled with "Subject_ID", as it had no name previously
	# d) activity_labels: Labeled with "Activity" to enable merge, and "Activity_Name" for description
# 10) Binds the three data files together
# 11) removes unnecessary files/values
# 12) Merges the activity_labels file with the full_data file to give descriptive activity names
# 13) Removes everything but the now Clean, Intermediate File.
# 14) Converts Clean_Intermediate to a tbl_df file to enable DPLYR package functions
# 15) removes old clean_intermediate DF
# 16) Removes redundant Activity column using select(-Activity), which just contains numbers not names. 
# 17) groups CI_tbl_df by Activity_Name and Subject_ID
# 18) Removes the ungrouped CI_tbl_df
# 19) Runs the mean function on each variable for each activity and each subject
# 20) Removes the grouped_data
# 21) writes out the new Tidy data set with either write.table(file = "x", row.names=FALSE)or
	write.table(file = "clean_tidy.txt", row.names=FALSE)