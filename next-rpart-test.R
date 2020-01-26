# Title     : TODO
# Objective : TODO
# Created by: kapol
# Created on: 26.01.2020

# using ggplot2 data diamonds - https://ggplot2.tidyverse.org/reference/diamonds.html
# we will modify data to be better for classification problem

set.seed(20)

data <- as.data.frame(ggplot2::diamonds)
data$over_4000 <- sapply(data$price, FUN = function(v) ifelse(v > 4000, 1, 0))


train_size <- floor(0.85 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = train_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]


targ <- "over_4000"
preds <- c("carat", "depth", "table", "cut", "x", "y", "z", "clarity", "color")
dtree <- rpart::rpart(formula = train[,targ] ~ ., data = train[,preds])

rpart.plot::rpart.plot(dtree)

predictions <- predict(dtree, test)
predictions <- ifelse(predictions >= 0.5, 1, 0)
predictions <- as.numeric(as.matrix(predictions))

reference <- test$over_4000

caret::confusionMatrix(data = factor(predictions, levels=min(predictions):max(predictions)),
                       reference = factor(reference, levels=min(predictions):max(predictions)))

