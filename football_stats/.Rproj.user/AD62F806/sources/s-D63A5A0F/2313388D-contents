library(tidyverse)
library(tidymodels)
library(xgboost)
library(caret)
library(DiagrammeR)

data = read.csv('df.csv', encoding = 'utf8')
# data = data[data$GK == 1,]

shuffled_data= data[sample(1:nrow(data)), ]
df <- shuffled_data[, -which(names(shuffled_data) %in% c("X", "player", "squad"))]
df <- df %>%
  relocate(value_eur)
# df <- df[df$value_eur < 2e07,]

###################################################################################################################
# for (col in colnames(df)) {
#   if (col %in% c("id", "season", "squad", "league", "player", "squad", "contract_signing", "elite_team")){
#     print("skip")
#   }else if(col %in% c("xg_net", "npxg_net", "xa_net")){
#     print("skip because log 0")
#   }else if(col %in% c("plus_minus_per90", "FW", "DF", "GK", "MF", "wc_cards_red", 
#                       "xg_plus_minus_wowy", "xg_plus_minus_per90", "xg_plus_minus", 
#                       "plus_minus", "plus_minus_wowy"
#                       )){
#     print("skip because not significant")
#   } else{
#   print(col)
#   simple.fit = glm(formula = as.formula(paste0("value_eur~",col, "")), 
#                    data = data, family = Gamma("log"))
#   print(summary(simple.fit))
# }
# # plot(data[col],
# #      log(data$value_eur)
# # )
# # abline(model)
# }

not_signif_features = c()
for (col in colnames(df)) {
  if (col %in% c("id", "season", "squad", "league", "player", "squad", "contract_signing", "elite_team")){
    print("skip")
  }else if(col %in% c("xg_net", "npxg_net", "xa_net")){
    print("skip because log 0")
  }else{
    simple.fit = glm(formula = as.formula(paste0("value_eur~",col, "")), 
                     data = data, family = gaussian("log"))
    d <- data.frame(summary(simple.fit)$coef[summary(simple.fit)$coef[,4] <= .05, 4])[2,1]
    if (is.na(d) | d > 0.01){
      not_signif_features <- c(not_signif_features, col)
    }else{
      print(summary(simple.fit))
    }
    
    
  }
}

not_signif_features <- not_signif_features[not_signif_features != "value_eur"]
df <- df[, -which(names(df) %in% not_signif_features)]

basic.fit = glm(value_eur ~ ., 
             data = train, family = gaussian("log"))

print(summary(basic.fit))
# 
# data1 = sort(sample(nrow(df), nrow(df)*.7))
# #creating training data set by selecting the output row values
# train<-df[data1,]
# #creating test data set by not selecting the output row values
# test<-df[-data1,]
# 
# y <-  test$value_eur
# yhat <- predict(basic.fit, test)
# print(postResample(y, yhat))
# 
# plot(y,
#      yhat,
#      xlab = "actual",
#      ylab = "predicted",
#      main = title)
# abline(lm(yhat ~ y))
# abline(coef = c(0,1))
# 
# 



################################################################################

get_aic <- function(model){
  # summary(model)$adj.r.squared
  return(summary(model)$aic)
}

transform_log = list()


for (col in colnames(df)) {
  if (col == "value_eur"){next}
  if (col == "rich_team"){next}
  if (col == "best_age"){next}
  if (col == "players_dribbled_past"){next}
  if (col == "dribbles_completed_diff"){next}
  if (col == "npxg_net"){next}
  if (col == "xg_net"){next}
  if (col == "xa_net"){next}
  if (col == "FW"){next}
  
  
  log.aic = Inf
  sqrt.aic = Inf
  simple.aic = Inf
  
  tryCatch({
    sqrt.fit = glm(formula = as.formula(paste0("value_eur~sqrt(abs(",col, "))")), 
                   data = df, family = gaussian("log"))
    sqrt.aic = get_aic(sqrt.fit)
    
  }, error=function(e){
    print(col)
  })
  tryCatch({
    log.fit = glm(formula = as.formula(paste0("value_eur~log(abs(",col, ") + 1)")), 
                  data = df, family = gaussian("log"))
    log.aic = get_aic(log.fit)
    
  }, error=function(e){
  })
  tryCatch({
    simple.fit = glm(formula = as.formula(paste0("value_eur~",col, "")), 
                     data = df, family = gaussian("log"))
    simple.aic = get_aic(simple.fit)
    
  }, error=function(e){
    # simple.aic = Inf
  })
  
  
  
  aics <- list(
    sqrt = sqrt.aic,
    log = log.aic,
    simple = simple.aic
  )
  
  name = names(which.min(unlist(aics)))  
  if (name == "log"){
    df[col] = log(df[col]+1)
    transform_log[[col]] = "log"  
  }else if(name == "sqrt"){
    df[col] = sqrt(df[col])
    transform_log[[col]] = "sqrt"  
    
  }else if(name == "simple"){
    transform_log[[col]] = "simple"  
  }
}



data1 = sort(sample(nrow(df), nrow(df)*.7))
#creating training data set by selecting the output row values
train<-df[data1,]
#creating test data set by not selecting the output row values
test<-df[-data1,]

transformed.fit = glm(value_eur ~ ., 
                      data = train, family = gaussian("log"))

print(summary(transformed.fit))


y <-  test$value_eur
yhat <- exp(predict(transformed.fit, test[, -which(names(test) == "value_eur")]))

y <- y[yhat < 1e8]
yhat <- yhat[yhat < 1e8]

print(postResample(y, yhat))

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted")
# abline(lm(yhat ~ y))
abline(coef = c(0,1))
