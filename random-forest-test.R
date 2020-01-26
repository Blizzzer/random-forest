# Title     : TODO
# Objective : TODO
# Created by: kapol
# Created on: 26.01.2020

set.seed(324453)

data <- as.data.frame(ggplot2::diamonds)
data$is_at_least_premium <- sapply(data$cut, FUN = function(v) ifelse(v == "Premium" || v == "Ideal", 1, 0))

n_trees <- 100
train_size <- floor(0.85 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = train_size)

train <- data[train_ind,]
test <- data[-train_ind,]


targ <- "is_at_least_premium"
possible_preds <- c("carat", "depth", "table", "price", "x", "y", "z", "clarity", "color")
possible_methods <- c("anova", "poisson")
combinations <- combn(possible_preds, 2, simplify = FALSE)

pred <- vector(length = nrow(test))
for (i in seq_len(n_trees)) {
  possible_predstmp <- possible_preds
  pred1 <- possible_preds[sample(seq_len(length(possible_preds)), size = 1)]
  possible_predstmp <- possible_predstmp[ possible_predstmp != pred1]
  pred2 <- possible_predstmp[sample(seq_len(length(possible_predstmp)), size = 1)]

  met <- sample(possible_methods, size = 1)

  rtree <- rpart::rpart(formula = train[,targ] ~ ., data = train[,c(pred1, pred2)], method = met)

  pred_tmp <- predict(rtree, test)
  pred <- pred + pred_tmp
}

pred <- pred / n_trees

pred <- ifelse(pred >= 0.5, 1, 0)

reference <- test$is_at_least_premium

caret::confusionMatrix(data = factor(pred, levels=min(pred):max(pred)),
                       reference = factor(reference, levels=min(pred):max(pred)))
