library(tidyverse)
library(tidymodels)

data = read.csv('df.csv', encoding = 'utf8')
data <- data[, -which(names(data) %in% c("X", "season", "player", "squad"))]

plot((data$on_xg_against), log(data$value_eur))

plot(sqrt(data$minutes), log(data$value_eur))
hist((data$games)^(1/3))

hist(log(data$pass_targets))

model <- glm(value_eur ~ best_age + I((pass_targets)) + sqrt(gca_passes_live) + I(sqrt(data$minutes)) 
             , 
             data = data, family = Gamma("log"))
summary(model)


model <- glm(value_eur ~ ., 
             data = data, family = Gamma("log"))
summary(model)


