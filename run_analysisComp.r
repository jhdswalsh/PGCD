#
# rm(list = ls()) # Clear workspace
# Source of data for this project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# This R script does the following:

# (1) Bind train & test sets
# The cbind function concats datasets column wise, i.e., new variables/features. 
# The rbind function appends datasets row wises, i.e., additional observations of the same variables/features.
 
library(data.table) 

# X = actual data for computation
x_f_a    <- file.path("train/X_train.txt")
x_dt_a   <- data.table(read.table(x_f_a))
x_f_b    <- file.path("test/X_test.txt")
x_dt_b   <- data.table(read.table(x_f_b))
X_dt_ab  <- rbind(x_dt_a, x_dt_b)
#utils::View(X_dt_ab)

# Y = Activity labels
y_f_a    <- file.path("train/y_train.txt")
y_dt_a   <- data.table(read.table(y_f_a))
y_f_b    <- file.path("test/y_test.txt")
y_dt_b   <- data.table(read.table(y_f_b))
Y_dt_ab  <- rbind(y_dt_a, y_dt_b)
setnames(Y_dt_ab, "V1", "actNumber")
#View(Y_dt_ab)

# Z = Subject labels
z_f_a    <- file.path("train/subject_train.txt")
z_dt_a   <- data.table(read.table(z_f_a))
z_f_b    <- file.path("test/subject_test.txt")
z_dt_b   <- data.table(read.table(z_f_b))
Z_dt_ab  <- rbind(z_dt_a, z_dt_b)
setnames(Z_dt_ab, "V1", "subject")
#View(Z_dt_ab)


XYZ_dt_ab <- cbind(X_dt_ab, Y_dt_ab, Z_dt_ab) # A single Complete dataset of  
setkey(XYZ_dt_ab, subject, actNumber) # 
# (Cols 1:561 X data (features), Col 562 Y data (Activty), Col 563 Z data (Subject))
# (Rows 1:7352 training data, rows 7353:10299 testing data)
#utils::View(XYZ_dt_ab)


# (2) Find precisely mean and std dev indices for each variable/feature.

fts <- fread(file.path("features.txt"))
setnames(fts, names(fts), c("ftNumber", "ftName"))
fts <- fts[grepl("mean\\(\\)|std\\(\\)", ftName)]
fts$indx <- fts[, paste0("V", ftNumber)]
filter <- c(fts$indx, key(XYZ_dt_ab)) # keep these cols (acts & subjs)
XYZ_dt_ab <- XYZ_dt_ab[, filter, with=FALSE]


# (3) & (4) Label the descriptive activity within the dataset.
actNames <- fread(file.path("activity_labels.txt"))
setnames(actNames, names(actNames), c("actNumber", "actName"))
XYZ_dt_ab <- merge(XYZ_dt_ab, actNames, by="actNumber", all.x=TRUE)
setkey(XYZ_dt_ab, subject, actNumber, actName)
XYZ_dt_ab <- data.table(melt(XYZ_dt_ab, key(XYZ_dt_ab), variable.name="indx"))
XYZ_dt_ab <- merge(XYZ_dt_ab, fts[, list(ftNumber, indx, ftName)], by="indx", all.x=TRUE)
XYZ_dt_ab$act <- factor(XYZ_dt_ab$actName)
XYZ_dt_ab$ft <- factor(XYZ_dt_ab$ftName)

SLP <- function (regex) {
  grepl(regex, XYZ_dt_ab$ft)
}
# One type
XYZ_dt_ab$ftJerk <- factor(SLP("Jerk"), labels = c(NA, "Jerk"))
XYZ_dt_ab$ftMag <- factor(SLP("Mag"), labels = c(NA, "Magnitude"))

# Two types 
i <- 2
j <- matrix(seq(1, i), nrow = i)
k <- matrix(c(SLP("^t"), SLP("^f")), ncol = nrow(j))
XYZ_dt_ab$ftDom <- factor(k %*% j, labels = c("Time", "Freq"))
k <- matrix(c(SLP("Acc"), SLP("Gyro")), ncol = nrow(j))
XYZ_dt_ab$ftIns <- factor(k %*% j, labels = c("Accelerometer", "Gyroscope"))
k <- matrix(c(SLP("BodyAcc"), SLP("GravityAcc")), ncol = nrow(j))
XYZ_dt_ab$ftAcc <- factor(k %*% j, labels = c(NA, "Body", "Gravity"))
k <- matrix(c(SLP("mean()"), SLP("std()")), ncol = nrow(j))
XYZ_dt_ab$ftVar <- factor(k %*% j, labels = c("Mean", "SD"))

# Three types
i <- 3
j <- matrix(seq(1, i), nrow = i)
k <- matrix(c(SLP("-X"), SLP("-Y"), SLP("-Z")), ncol = nrow(j))
XYZ_dt_ab$ftAxis <- factor(k %*% j, labels = c(NA, "X", "Y", "Z"))

cp1 <- nrow(XYZ_dt_ab[, .N, by=c("ft")])
cp2 <- nrow(XYZ_dt_ab[, .N, by=c("ftDom", "ftAcc", "ftIns", "ftJerk", "ftMag", "ftVar", "ftAxis")])
cp1 == cp2 # check point

# (5) Final tidy dataset: mean of each variable with activity and subject (including feature details). 

setkey(XYZ_dt_ab, subject, act, ftDom, ftIns, ftVar, ftAxis, ftAcc, ftJerk, ftMag)
Tidy_Data <- XYZ_dt_ab[, list(count = .N, mean_val = mean(value)), by = key(XYZ_dt_ab)]



f <- file.path("Tidy_Data.txt")
write.table(Tidy_Data, f, quote = FALSE, sep = "\t", row.names = FALSE)
