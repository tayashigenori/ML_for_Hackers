
setwd("./GitHub/ML_for_Hackers/12_model_comparison/")

library('glmnet')
library('ggplot2')
library('e1071') # for svm
library('class') # for knn

# load document term matrix
load('data/dtm.RData')

# split data into training and test data
set.seed(1)

training.indices <- sort(sample(1:nrow(dtm), round(0.5 * nrow(dtm))))
test.indices <- which(! 1:nrow(dtm) %in% training.indices)
train.x <- dtm[training.indices, 3:ncol(dtm)]
train.y <- dtm[training.indices, 1]
test.x <- dtm[test.indices, 3:ncol(dtm)]
test.y <- dtm[test.indices, 1]

rm(dtm)

##############################################
# 1. logistic regression with regularization #
##############################################
regularized.logit.fit <- glmnet(train.x, train.y, family = c('binomial'))

# cross-validation (simplified)
lambdas <- regularized.logit.fit$lambda
performance <- data.frame()

for (lambda in lambdas)
{
  predictions <- predict(regularized.logit.fit, test.x, s = lambda)
  predictions <- as.numeric(predictions > 0)
  mse <- mean(predictions != test.y)

  performance <- rbind(performance, data.frame(Lambda = lambda, MSE = mse))
}

ggplot(performance, aes(x = Lambda, y = MSE)) +
  geom_point() +
  scale_x_log10()

# choose best lambda
best.lambda <- with(performance, max(Lambda[ which(MSE == min(MSE)) ] ))

# compute Mean Square Error for the best lambda
mse <- with(subset(performance, Lambda == best.lambda), MSE)
mse
## check output. only 6%!


#############################
# 2. SVM with linear kernel #
#############################

linear.svm.fit <- svm(train.x, train.y, kernel = 'linear')

# compute Mean Square Error
predictions <- predict(linear.svm.fit, test.x)
predictions <- as.numeric(predictions > 0)
mse <- mean(predictions != test.y)
mse
## check output. 12%
## more errors than logistic regression!!

###############################
# 3. SVM with Gaussian kernel #
###############################

radial.svm.fit <- svm(train.x, train.y, kernel = 'radial')

predictions <- predict(radial.svm.fit, test.x)
predictions <- as.numeric(predictions > 0)
mse <- mean(predictions != test.y)
mse
## check output. 14%
## even more errors than SVM with linear kernel

##########################
# 4. k Nearest Neighbour #
##########################

library('class')
knn.fit <- knn(train.x, test.x, train.y, k = 50)

# compute Mean Square Error
predictions <- as.numeric(as.character(knn.fit))
mse <- mean(predictions != test.y)
mse
## check output. 14%
## as erroneous as SVM

# test with varying k
performance <- data.frame()

for (k in seq(5, 50, by = 5))
{
  knn.fit <- knn(train.x, test.x, train.y, k = k)

  predictions <- as.numeric(as.character(knn.fit))
  mse <- mean(predictions != test.y)

  performance <- rbind(performance, data.frame(K = k, MSE = mse))
}

# choose best k
best.k <- with(performance, K[ which(MSE == min(MSE)) ] )
best.mse <- with(subset(performance, K == best.k), MSE)
best.mse
## checkout output. 9%
## less errors than SVM, but more errors than logistic regression

# logistic regression > k NN > SVM (linear kernel > Gaussian kernel)

# (1) test various algorithms
# (2) good algorithm varies depending on the problem
# (3) try hard to adjust hyper-parameters
