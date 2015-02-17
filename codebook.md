---
title: "CodeBook"
author: "Cameron Ferguson"
date: "Tuesday, February 17, 2015"
output: html_document
---

#run_analysis Codebook

This is the codebook for the run_analysis script. The following document will describe the raw data, all the elements that the script uses, as well as the final data.

View the ReadMe for information on the flow and logic of the script, as well as specific liscense. 

---

###Raw Data

The Raw data is available at: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

For this script to work, the data must be unzipped in the current working directory.

For information on this Raw data, please see the readme contained within it.

---

###Script Variables

The Script Variables are broken down into the steps laid out in the Readme, and deliminated within the script via commented numbers.

<b>file_name</b> This is an optional variable allowing the user to name their file.

<b>feature_measurements_test</b>: is the raw X_test.txt data read into R using read.table <br>
<b>activity_test</b>: is the raw y_test.txt data read into R using read.table 
<br>
<b>subject_test</b>: is the raw subject_test.txt data read into R using read.table 
<br>
<b>feature_measurements_train</b>: is the raw X_train.txt data read into R using read.table <br>
<b>activity_train</b>: is the raw y_train.txt data read into R using read.table 
<br>
<b>subject_train</b>: is the raw subject_train.txt data read into R using read.table
<br>
<b>feature_list</b>: is the raw features.txt data read into R using read.table 
<br>
<b>activity_labels</b>: is the raw activity_labels.txt data read into R using read.table
<br><br>
<b> feature_measurement_data</b>: is the two measurement data files merged using rbind
<br>
<b> activity_data </b>: is the two activity data files merged using rbind
<br>
<b> subject_data </b>: is the two subject data files merged using rbind
<br><br>
<b>mean_col_numbers</b>: is a list of numbers representing the column numbers found using grep searching for the phrase mean() in the 2nd column of the feature_list data file.
<br>
<b>std_col_numbers</b>: is a list of numbers representing the column numbers found using grep searching for the phrase std() in the 2nd column of the feature_list data file.
<br>
<b>column_numbers</b>: is the SORTED combination of the mean_col_numbers and std_col_numbers lists. It must be sorted inorder for the next step to pull the columns out of the data files in order. If this doesn't happen, then the names will be added incorrectly.
<br><br>
<b>selc_measurement_data</b>: is a data.frame consisting of just the columns we have selected for using the column_numbers list, ie just the columns that contain measurements of the mean() or std(). It contains all rows however.
<br><br>
<b>column_names</b>: is a character list containing the names of the measurements we have selected using the column_numbers file, ie just the names that contain mean() and std()
<br><br>
Next four data files get column names using the colnames function: <br>
        1)selc_measurement_data get column names as determined by the column_names list<br>
        2)activity_data gets the column name Activity<br>
        3)subject_data gets the column name Subject_ID<br>
        4)activity_labels gets the column names Activity and Activity Name<br>
<br><br>
<b>full_data</b>: is a data.frame containing a cbind of subject_data, activity_data, and selc_measurement_data. 
<br><br>
<b>Clean_Intermediate</b>: contains a merge of full_data and activity_labels by the Activity column. This in effect adds activity names to the observations.
<br><br>
<b>CI_tbl_df</b>: is the Clean_Intermediate data.frame converted into a tbl_df for use within the dplyr package.
<br><br>
Next we remove the Activity column, as it just containes numbers, which we have replaced in previous steps with the activity labels column.
<br><br>
<b>grouped_data</b>: is the CI_tbl_df grouped by Activity Name and then Subject ID
<br><br>
<b>clean_tidy</b>: is our final data.frame. To get this we run the mean function on every integer containing column within our groups, so for each Activity and Subject. This gives us a mean value for each measurement (which again are just the mean and std deviation measurements) for each subject ID at every activity. In essense, you could pull out subject ID #30 and get the mean for all his measurements for each of his activities. 
<br>
This data.frame contains 68 variables, and 180 observations. The first two variables are the Activity Names and then the Subject IDs. The final 66 are mean values of the measurements of mean and standard deviation taken from the raw data. As there are 6 different Activities and 30 different subjects we end up with 180 observations, one for each subject doing each activity. <br><br>
This data.frame is then written to a file, either named by the user, as denoted by the file_name variable, or named clean_tidy.txt.
