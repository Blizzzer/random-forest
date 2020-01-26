# Title     : TODO
# Objective : TODO
# Created by: kapol
# Created on: 18.01.2020
# http://archive.ics.uci.edu/ml/machine-learning-databases/acute/diagnosis.data

training_percent <- 0.75
seed <- 234

mydata <- read.table("diagnosis.csv", header=FALSE, sep=";", dec = ",", quote = "")
colnames(mydata) <- c("temp", "nausea", "lumbar_pain", "urine_pushing", "micturition_pain", "burning", "dec_inflamation", "dec_nephritis")
mydata$dec_inflamation = ifelse(mydata$dec_inflamation == "yes", 1, 0)
mydata$dec_nephritis = ifelse(mydata$dec_nephritis == "yes", 1, 0)
str(mydata)

train_size <- floor(0.75 * nrow(mydata))
set.seed(seed)

train_ind <- sample(seq_len(nrow(mydata)), size = train_size)

train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

targ <- "dec_inflamation"
preds <- c("temp", "nausea", "lumbar_pain", "urine_pushing", "micturition_pain", "burning")

dtree <- rpart::rpart(formula = train[,targ] ~ ., data = train[,preds])

rpart.plot::rpart.plot(dtree)

predictions <- predict(dtree, test)
predictions <- ifelse(predictions >= 0.5, 1, 0)
print(predictions)
predictions <- as.numeric(as.matrix(predictions))

reference <- test$dec_inflamation


table <- table(factor(reference, levels=min(predictions):max(predictions)),
      factor(predictions, levels=min(predictions):max(predictions)))

print(table)