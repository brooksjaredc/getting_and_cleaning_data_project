# This reads in the data, labels the columns, factors the activity labels, and merges the data in the first section

######################################################################################################################

# Read in the feature names

feature.names <- read.table("UCI HAR Dataset/features.txt")[2]

# Read in test and train features and name columns using feature.names

features.test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = feature.names[,1])
features.train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = feature.names[,1])

# Row bind features.test and features.train

features <- rbind(features.test, features.train)

# Read in activity labels and test and train activity columns

activity.labels <- read.table("UCI HAR Dataset/activity_labels.txt")
y.test <- read.table("UCI HAR Dataset/test/y_test.txt")
y.train <- read.table("UCI HAR Dataset/train/y_train.txt")

# Row bind y.test and y.train

activities <- rbind(y.test, y.train)

# Convert activities numbers into factors that describe the activity

activities.factor <- factor(activities[,1], labels = activity.labels[,2])

# Column bind activities.factors to the features

features.activites <- cbind(activities.factor, features)

# Read in subject_test.txt and subject_train.txt

subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt")

# Row bind subject.test and subject.train, then Column bind result to features.activities

subject <- rbind(subject.test, subject.train)
subject <- factor(subject[,1])
full.data <- cbind(subject, features.activites)
names(full.data) <- gsub("\\.\\.\\.","\\.",names(full.data))
names(full.data) <- gsub("\\.\\.","",names(full.data))

######################################################################################################################

# Now we have our full, labeled, and factored data set.
# In the next section we extract only the measurements on the mean and standard deviation for each measurement

######################################################################################################################

# Create boolean vector of columns that have "mean." or "std." in their names

means.and.stds <- grepl("mean\\.|std\\.",names(full.data))

# Change first two values of this vector to TRUE to keep subject and activity columns

means.and.stds[1:2] <- TRUE

# Create new table by subsetting the columns of full.data with means.and.stds

avg.data <- full.data[,means.and.stds]

######################################################################################################################

# Our data now only includes the measurements on the means and standard deviations
# In the next section we group by activites and subject, summarize to find the means of column for each activity
# and subject. Finally, we save this dataset to a file.

######################################################################################################################


# Group by activities and subject, then use summarize_each to find the mean of all of the columns in avg.data

second.dataset <- avg.data %>% group_by(activities.factor, subject) %>% summarize_each(funs(mean))

# Save the second dataset to file

write.table(second.dataset, file = "second_dataset.txt", row.names = FALSE)

######################################################################################################################

# That's it!

######################################################################################################################

