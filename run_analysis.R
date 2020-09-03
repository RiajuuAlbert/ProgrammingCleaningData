# Getting and Cleaning Data Programming Assignment  Coursera
# Albert Setiawan

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load Packages and get the Data
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

# Load activity labels + features
actLabels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
ft <- fread(file.path(path, "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "ftNames"))
ftWant <- grep("(mean|std)\\(\\)", ft[, ftNames])
measure <- ft[ftWant, ft]
measure <- gsub('[()]', '', measure)

# Load train datasets
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, ftWant, with = FALSE]
data.table::setnames(train, colnames(train), measurements)
trainAct <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainSub <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
train <- cbind(trainSub, trainAct, train)

# Load test datasets
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, ftWant, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
testAct <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
testSub <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))
test <- cbind(testSub, testAct, test)

# merge datasets
combi <- rbind(train, test)

# Convert classLabels to activityName basically. More explicit. 
combi[["Activity"]] <- factor(combi[, Activity]
                                 , levels = activityLabels[["classLabels"]]
                                 , labels = activityLabels[["activityName"]])

combi[["SubjectNum"]] <- as.factor(combi[, SubjectNum])
combi <- reshape2::melt(data = combi, id = c("SubjectNum", "Activity"))
combi <- reshape2::dcast(data = combi, SubjectNum + Activity ~ variable, fun.aggregate = mean)

data.table::fwrite(x = combi, file = "tidyData.txt", quote = FALSE)