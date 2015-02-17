## Course Project

# first step, is to make a cohert test and train data frame, currently they are split

# x_test contains the data set, y_test contains the activity #, 
# referenced in the activity labels file
# subject_test contains the ID for which subject performed the activity

## the below commands read in the data, 
## after checking for the UCI HAR dataset in the current WD

if (file.exists("UCI HAR Dataset") != TRUE){
        stop("UCI HAR Dataset folder is not in working directory")
}
library(data.table)
library(dplyr)


feature_measurements_test <- read.table("UCI HAR Dataset/test/X_test.txt")
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
feature_measurements_train <- read.table("UCI HAR Dataset/train/X_train.txt")
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
feature_list <- read.table("UCI HAR Dataset/features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

## Next it combines the x files, y files, and subject files together

feature_measurement_data <- rbind(feature_measurements_test, feature_measurements_train)
activity_data <- rbind(activity_test, activity_train)
subject_data <- rbind(subject_test, subject_train)

# then to keep things clean we eliminate the original, unmerged files
rm(subject_test, feature_measurements_test, activity_test, subject_train, feature_measurements_train, activity_train )


# next we take just the mean and standard deviation data
# I decided that only means of raw data would be considered, ie it says mean at the end.
# to find the correct columns I just looked at the raw data, and made a note
# of anything ending in mean or std. The following are the column numbers
mean_col_numbers <- grep("mean()", feature_list[,2], fixed=TRUE)
std_col_numbers <- grep("std()", feature_list[,2], fixed=TRUE)
column_numbers <- sort(c(mean_col_numbers, std_col_numbers))
selc_measurement_data <- feature_measurement_data[,column_numbers]
rm(feature_measurement_data)

############# this shouldn't be here but ive figuered it out

## to label the variables, I'll make a column_names vector containing the same
## names as the original data. I'll also add the subject and the activity column name

column_names <- as.character(feature_list[column_numbers, 2])
colnames(selc_measurement_data) <- column_names
colnames(activity_data) <- "Activity"
colnames(subject_data) <- "Subject_ID"
colnames(activity_labels) <- c("Activity", "Activity_Name")
full_data <- cbind(subject_data, activity_data, selc_measurement_data)
rm(subject_data, activity_data, selc_measurement_data, feature_list)
rm(column_numbers, mean_col_numbers, std_col_numbers, column_names)

## Now that we have a data set combined with all data, we shall now add a column
## that will tell us what each observation is, ie it will convert the activity number
## to an activity name. This had to be done last, as the merge command reorders the data


Clean_Intermediate <- merge(activity_labels, full_data, by="Activity")
rm(full_data, activity_labels)

## here is the end of step 4. Now we need to create our TIDY data set. but first lets
## remove all the crap.

CI_tbl_df <- tbl_df(Clean_Intermediate)
CI_tbl_df <- select(CI_tbl_df, -Activity)
rm(Clean_Intermediate)
grouped_data <- group_by(CI_tbl_df, Activity_Name, Subject_ID)
rm(CI_tbl_df)
summarise_each(grouped_data, funs(mean)) -> clean_tidy
rm(grouped_data)

write.table(clean_tidy, "clean_tidy.txt", row.names=FALSE)

### add grep to your script : ie grep("mean", colnames(data))
## add linking to make it easier to read
## add fread to speed it up
