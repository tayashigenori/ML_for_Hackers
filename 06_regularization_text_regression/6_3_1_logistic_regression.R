
setwd("./Github/ML_for_Hackers/06_regularization/")

library('ggplot2')
library('tm')
library('glmnet')
library('boot')

# read csv and create Document Term Matrix
ranks <- read.csv('data/oreilly.csv', stringsAsFactors = FALSE)

documents <- data.frame(Text = ranks$Long.Desc.)
row.names(documents) <- 1:nrow(documents)

corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

dtm <- DocumentTermMatrix(corpus)

# learn on training data and cross-validate on test data
# with varying lambda, 50 iterations
x <- as.matrix(dtm)

y <- rep(c(1, 0), each = 50)

# logistic regression
regularized.fit <- glmnet(x, y, family = 'binomial')

# compare
regularized.fit <- glmnet(x, y) # family = 'gaussian'
regularized.fit <- glmnet(x, y, family = 'gaussian')
regularized.fit <- glmnet(x, y, family = 'binomial')

# predict
predict(regularized.fit, newx = x, s = 0.001)

## check the output

# make them 0/1
ifelse(predict(regularized.fit, newx = x, s = 0.001) > 0, 1, 0)

# make them 0/1
inv.logit(predict(regularized.fit, newx = x, s = 0.001))

# learn on training data and cross-validate on test set
# with varying lambda, 250 iterations

set.seed(1)
performance <- data.frame()

for (i in 1:250)
{
  indices <- sample(1:100, 80)

  training.x <- x[indices, ]
  training.y <- y[indices]

  test.x <- x[-indices, ]
  test.y <- y[-indices]

  for (lambda in c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.5, 0.1))
  {
    glm.fit <- glmnet(training.x, training.y, family = 'binomial')        # (1)
    predicted.y <- ifelse(predict(glm.fit, test.x, s = lambda) > 0, 1, 0) # (2)
    error.rate <- mean(predicted.y != test.y)

    performance <- rbind(performance,
                         data.frame(Lambda = lambda,
                                    Iteration = i,
                                    ErrorRate = error.rate))
  }
}

# plot
ggplot(performance, aes(x = Lambda, y = ErrorRate)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point') +
  scale_x_log10()
