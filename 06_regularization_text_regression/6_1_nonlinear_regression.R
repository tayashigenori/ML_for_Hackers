
library(ggplot2)

set.seed(1)

# plot non-linear function
x <- seq(-10, 10, by = 0.01)
y <- 1 - x ^ 2 + rnorm(length(x), 0, 5)
df1 <- data.frame(X = x, Y = y)
ggplot(df1, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(se = FALSE)

# linear regression
x <- seq(-10, 10, by = 0.01)
y <- 1 - x ^ 2 + rnorm(length(x), 0, 5)
df2 = data.frame(X = x, Y = y)
ggplot(df2, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

## check regression line

# non-linear regression
x.squared <- x ^ 2
df3 = data.frame(XSquared = x.squared, Y = y)
ggplot(df3, aes(x = XSquared, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

## check regression line

# compute R^2
summary(lm(y ~ x))$r.squared

summary(lm(y ~ x.squared))$r.squared

## check the gain: 0% to 97%
