
library(ggplot2)

set.seed(1)

# plot sin
x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

df <- data.frame(X = x, Y = y)
ggplot(df, aes(x = X, y = Y)) +
  geom_point()

# linear regression
summary(lm(Y ~ X, data = df))

## check R-squared

# plot linear regression
df <- data.frame(X = x, Y = y)
ggplot(df, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# nonlinear regression X ^ 2, X ^ 3
df <- transform(df, X2 = X ^ 2)
df <- transform(df, X3 = X ^ 3)

summary(lm(Y ~ X + X2 + X3, data = df))

## check R-squared

# nonlinear regression X ^ 4 to 14
df <- transform(df, X4 = X ^ 4)
df <- transform(df, X5 = X ^ 5)
df <- transform(df, X6 = X ^ 6)
df <- transform(df, X7 = X ^ 7)
df <- transform(df, X8 = X ^ 8)
df <- transform(df, X9 = X ^ 9)
df <- transform(df, X10 = X ^ 10)
df <- transform(df, X11 = X ^ 11)
df <- transform(df, X12 = X ^ 12)
df <- transform(df, X13 = X ^ 13)
df <- transform(df, X14 = X ^ 14)

summary(lm(Y ~ X + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14,
           data = df))

## check "singularity", check some of coefficients are "NA"

# degree parameter

summary(lm(Y ~ poly(X, degree = 14), data = df))

## check all coefficients are defined

# degree = 1, 3, 5, 25
plot_sin <- function (d) {
  poly.fit <- lm(Y ~ poly(X, degree = d), data = df)
  df <- transform(df, PredictedY = predict(poly.fit))

  ggplot(df, aes(x = X, y = PredictedY)) +
    geom_point() +
    geom_line()
}
plot_sin(d = 1)
plot_sin(d = 3)
plot_sin(d = 5)
plot_sin(d = 25)

