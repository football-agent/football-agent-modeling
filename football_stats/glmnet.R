library(glmnet)

data = read.csv('df.csv', encoding = 'utf8')
# data = data[data$GK == 1,]

shuffled_data= data[sample(1:nrow(data)), ]
df <- shuffled_data[, -which(names(shuffled_data) %in% c("X", "player", "squad"))]
df <- df %>%
  relocate(value_eur)

# 
# data1 = sort(sample(nrow(df), nrow(df)*.7))
# #creating training data set by selecting the output row values
# train<-df[data1,]
# #creating test data set by not selecting the output row values
# test<-df[-data1,]
# 

df$value_eur = log(df$value_eur)

d <- data.frame(df)
m <- as.matrix(d)

set.seed(1234)
indices <- sample(1:nrow(df), size = 0.8 * nrow(df))
train <- m[indices,]
test <- m[-indices,]

X <-  train[, 2:length(df)]
y = train[, 1]

cvfit = cv.glmnet(X, y, type.measure = "mse", nfolds = 20)
plot(cvfit)


yhat <- predict(cvfit, newx = test[, 2:length(df)], s = "lambda.min")
y = test[, 1]


postResample(yhat, y)
r <- exp(y) - exp(yhat)
plot(r, ylab = "residuals")

plot(exp(y),
     exp(yhat),
     xlab = "actual",
     ylab = "predicted")
abline(lm(exp(yhat) ~ exp(y)))
abline(coef = c(0,1))

