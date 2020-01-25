# Title     : TODO
# Objective : TODO
# Created by: kapol
# Created on: 18.01.2020
# http://archive.ics.uci.edu/ml/machine-learning-databases/acute/diagnosis.data

# print("Hello world")

test_rows_number <- 20
learning_rows_number <- 100


mydata <- read.table("diagnosis.csv", header=FALSE, sep=";", dec = ",", quote = "")
colnames(mydata) <- c("temp", "nausea", "lumbar_pain", "urine_pushing", "micturition_pain", "burning", "dec_inflamation", "dec_nephritis")
str(mydata)
number_of_rows <- nrow(mydata)

if(test_rows_number + learning_rows_number != number_of_rows) {
  stop("invalid split of rows")
}


print(paste0("Number of rows: ", nrow(mydata)));


