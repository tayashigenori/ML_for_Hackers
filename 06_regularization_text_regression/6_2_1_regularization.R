
library('glmnet')

# define complexity
lm.fit <- lm(y ~ x)
model.complexity <- sum(coef(lm.fit) ^ 2)

# define complexities (L1 & L2 norm)
lm.fit <- lm(y ~ x)
l2.model.complexity <- sum(coef(lm.fit) ^ 2)
l1.model.complexity <- sum(abs(coef(lm.fit)))

# example: generate sin data

set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

x <- as.matrix(cbind(x, rev(x))) # x <- matrix(x)
glmnet(x, y)

## check outputs
## Def: number of non-zero coefficients
## %Def: R^2
## Lambda: penalty for complex models
## R^2 decreases as lambda descreases

# check the relation between RMSE (Rooted Mean Squared Error) and lambda
# split data into training and test sets
set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

n <- length(x)

indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

df <- data.frame(X = x, Y = y)
training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

# define rmse
rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

glmnet.fit <- with(training.df, glmnet(poly(X, degree = 10), Y))

lambdas <- glmnet.fit$lambda

performance <- data.frame()

# loop on lambda
for (lambda in lambdas)
{
  performance <- rbind(performance,
                       data.frame(Lambda = lambda,
                                  RMSE = rmse(test.y,
                                              with(test.df,
                                                   predict(glmnet.fit,
                                                           poly(X, degree = 10),
                                                           s = lambda)))))
}

# plot
ggplot(performance, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line()

# check the best model (= model with the best lambda)
best.lambda <- with(performance, Lambda[which(RMSE == min(RMSE))])

blmnet.fit <- with(df, glmnet(poly(X, degree = 10), Y))

coef(glmnet.fit, s = best.lambda)

