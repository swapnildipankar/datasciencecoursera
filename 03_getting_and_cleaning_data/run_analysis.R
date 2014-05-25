x.train.set           <- read.table("UCI HAR Dataset/train/X_train.txt")
x.test.set            <- read.table("UCI HAR Dataset/test/X_test.txt")
x.combined.set        <- rbind(x.train.set, x.test.set)
subject.train.set     <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject.test.set      <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject.combined.set  <- rbind(subject.train.set, subject.test.set)
y.train.set           <- read.table("UCI HAR Dataset/train/y_train.txt")
y.test.set            <- read.table("UCI HAR Dataset/test/y_test.txt")
y.combined.set        <- rbind(y.train.set, y.test.set)

features.set              <- read.table("UCI HAR Dataset/features.txt")
desired.features.indices  <- grep("-mean\\(\\)|-std\\(\\)", features.set[, 2])
x.combined.set            <- x.combined.set[, desired.features.indices]
names(x.combined.set)     <- features.set[desired.features.indices, 2]
names(x.combined.set)     <- sub("\\(\\)", "", names(x.combined.set))
names(x.combined.set)     <- gsub("-", ".", names(x.combined.set))

activitiy.labels          <- read.table("UCI HAR Dataset/activity_labels.txt")
activitiy.labels[, 2]     <- sub("_", " ", activitiy.labels[, 2])
y.combined.set[, 1]       <- activitiy.labels[y.combined.set[, 1], 2]
names(y.combined.set)     <- "activity.name"

names(subject.combined.set)   <- "subject.id"
merged.data.set               <- cbind(subject.combined.set, y.combined.set, x.combined.set)
write.table(merged.data.set, "merged_output.txt")

subject.ids     <- c(1:30)
num.subjects    <- 30
num.activities  <- 6
num.columns     <- dim(merged.data.set)[2]
result.set      <- merged.data.set[1:(num.subjects * num.activities), ]

row.index = 1
for (subject.id in subject.ids) {
  for (activity in 1:num.activities) {
    result.set[row.index, 1] = subject.ids[subject.id]
    result.set[row.index, 2] = activities[activity, 2]
    temporary.row <- merged.data.set[merged.data.set$subject.id == subject.id & merged.data.set$activity.name == activitiy.labels[activity, 2], ]
    result.set[row.index, 3:num.columns] <- colMeans(temporary.row[, 3:num.columns])
    row.index = row.index + 1
  }
}
write.table(result.set, "computed_mean_data_set.txt")
