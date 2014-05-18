
library(ggplot2)

# generate sin data
set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

# split data into training set and test set
n <- length(x)

indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

# define rmse (Root Mean Squared Error)
rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

# check performance for training and test data
# degree = 1 to 12
performance <- data.frame()

for (d in 1:12)
{
  poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)

  performance <- rbind(performance,
                        data.frame(Degree = d,
                                   Data = 'Training',
                                   RMSE = rmse(training.y, predict(poly.fit))))

  performance <- rbind(performance,
                        data.frame(Degree = d,
                                   Data = 'Test',
                                   RMSE = rmse(test.y, predict(poly.fit,
                                                               newdata = test.df))))
}

ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()
