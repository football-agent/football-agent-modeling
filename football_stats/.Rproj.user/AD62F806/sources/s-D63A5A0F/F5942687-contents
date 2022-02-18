library(tidyverse)
library(tidymodels)

data = read.csv('data.csv', encoding = 'utf8')
data = data[data$DF == 1,]
# data <- data %>% 
#   mutate(
#     elite_team = case_when(
#       squad %in% c("Real Madrid CF", "FC Barcelona") ~ 1,
#       !squad %in% c("Real Madrid CF", "FC Barcelona") ~ 0
#     ))






shuffled_data= data[sample(1:nrow(data)), ]
df <- shuffled_data[, -which(names(shuffled_data) %in% c("X", "player", "squad", "log_value_eur", "team_std", "team_avg "))]
df <- df %>%
  relocate(value_eur)
df <- df[df$value_eur < 6e07,]

###################################################################################################################
# 
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
team_dummy_features = c()
for (col in colnames(df)) {
  if (startsWith(col, "Team_")) {
    team_dummy_features <- c(team_dummy_features, col)
    
  }else if (col %in% c("id", "squad", "league", "player", "squad", "contract_signing", "elite_team")){
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
                 data = df, family = gaussian("log"))

print(summary(basic.fit))


################################################################################
get_aic <- function(model){
  return(summary(model)$aic)
}

transform_log = list()


for (col in colnames(df)) {
  if (col %in% team_dummy_features){next}
  if (col == "value_eur"){next}
  if (col == "rich_team"){next}
  if (col == "best_age"){next}
  if (col == "players_dribbled_past"){next}
  if (col == "dribbles_completed_diff"){next}
  if (col == "npxg_net"){next}
  if (col == "xg_net"){next}
  if (col == "xa_net"){next}
  
  
  log.aic = Inf
  sqrt.aic = Inf
  simple.aic = Inf
  
  tryCatch({
    sqrt.fit = glm(formula = as.formula(paste0("value_eur~sqrt(",col, ")")), 
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
df <- replace_na(df)
transformed.fit = glm(value_eur ~ ., 
                data = df, family = gaussian("log"))

print(summary(transformed.fit))





# 
# print(names(df))
# 
# 
# # plot((df$), log(data$value_eur))
# 
# plot(sqrt(data$goals), log(data$value_eur))
# 
# hist(log(data$goals))
# 
# model <- glm(value_eur ~ best_age + I((pass_targets)) + sqrt(gca_passes_live) + I(sqrt(data$minutes)) 
#              , 
#              data = data, family = Gamma("log"))
# summary(model)
# 
# 
# model <- glm(value_eur ~ ., 
#              data = data, family = Gamma("log"))
# summary(model)re
library(tidyverse)
library(tidymodels)
library(xgboost)
library(caret)
library(DiagrammeR)



###################################################################################################################
# sample <- sample.split(df$X, SplitRatio = 0.8)
# train_data <- subset(df, sample == TRUE)
# test_data <- subset(df, sample == FALSE)

df$value_eur = log(df$value_eur)

d <- data.frame(df)
m <- as.matrix(d)

set.seed(1234)
indices <- sample(1:nrow(df), size = 0.8 * nrow(df))
train <- m[indices,]
test <- m[-indices,]

####################################################################################
# hyper_grid <- expand.grid(max_depth = seq(2, 10, 4),
#                           eta = seq(.1, .9, .4),
#                           gamma = seq(.1, .9, .4),
#                           subsample = seq(.1, .9, .4),
#                           colsample_bytree = seq(.1, .9, .4)
# )
# hyper_grid <- expand.grid(max_depth = seq(2,6,2),
#                           eta = seq(.1, .3, .1),
#                           gamma = seq(0.3, 0.9, 0.2),
#                           subsample = seq(0.5, 0.9, 0.2),
#                           colsample_bytree = seq(0.3, 0.9, 0.2)
# )

hyper_grid <- expand.grid(eta = c(5e-3, 1e-3, 1e-4),
                          lambda = seq(0.3, 0.9, 0.2),
                          lambda_bias = seq(0.5, 0.9, 0.2),
                          alpha = seq(0.3, 0.9, 0.2)
)

xgb_train_rmse <- NULL
xgb_test_rmse <- NULL
best_iterations <- NULL

for (j in 1:nrow(hyper_grid)) {
  set.seed(1314)
  
  # m_xgb_untuned <- xgb.cv(
  #   booster = "gblinear",
  #   data = train[, 2:length(df)],
  #   label = train[, 1],
  #   nrounds = 1000,
  #   objective = "reg:squarederror",
  #   early_stopping_rounds = 30,
  #   nfold = 5,
  #   max_depth = hyper_grid$max_depth[j],
  #   eta = hyper_grid$eta[j],
  #   gamma = hyper_grid$gamma[j],
  #   subsample = hyper_grid$subsample[j],
  #   colsample_bytree = hyper_grid$colsample_bytree[j],
  #   print_every_n = 50,
  #   nthread = 8
  #   
  # )
  m_xgb_untuned <- xgb.cv(
    booster = "gblinear",
    data = train[, 2:length(df)],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 30,
    nfold = 5,
    eta = hyper_grid$eta[j],
    lambda = hyper_grid$lambda[j],
    lambda_bias = hyper_grid$lambda_bias[j],
    alpha = hyper_grid$alpha[j],
    
    print_every_n = 50,
    nthread = 8

  )

  
  # xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_gamma_nloglik_mean[m_xgb_untuned$best_iteration]
  # xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_gamma_nloglik_mean[m_xgb_untuned$best_iteration]
  xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  best_iterations[j] <- m_xgb_untuned$best_iteration
  
  cat(j, "\n")
}

#ideal hyperparamters
hyper_grid[which.min(xgb_test_rmse), ]
best_iterations[which.min(xgb_test_rmse)]

# max_depth eta gamma subsample colsample_bytree
# 82         2 0.1   0.5       0.9              0.3
# best_iterations[which.min(xgb_test_rmse)]
# 468


# hyper_grid <- expand.grid(max_depth = seq(hyper_grid[which.min(xgb_test_rmse), ]$max_depth),
#                           eta = seq(.1, .7, .2),
#                           gamma = seq(0.9, 0.9, 0.2),
#                           subsample = seq(0.5, 0.9, 0.2),
#                           colsample_bytree = seq(0.8, 0.95, 0.05)
# )
# 
# xgb_train_rmse <- NULL
# xgb_test_rmse <- NULL
# 
# for (j in 1:nrow(hyper_grid)) {
#   m_xgb_untuned <- xgb.cv(
#     data = train[, 2:length(df)],
#     label = train[, 1],
#     nrounds = 100,
#     objective = "reg:gamma",
#     early_stopping_rounds = NULL,
#     nfold = 5,
#     max_depth = hyper_grid$max_depth[j],
#     eta = hyper_grid$eta[j],
#     gamma = hyper_grid$gamma[j],
#     subsample = hyper_grid$subsample[j],
#     colsample_bytree = hyper_grid$colsample_bytree[j],
#     print_every_n = 50,
#     nthread = 8
#     
#   )
#   
#   xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_gamma_nloglik_mean[m_xgb_untuned$best_iteration]
#   xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_gamma_nloglik_mean[m_xgb_untuned$best_iteration]
#   # xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
#   # xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
#   
#   cat(j, "\n")
# }
# 
# #ideal hyperparamters
# hyper_grid[which.min(xgb_test_rmse), ]

#######################################################################################
m1_xgb <-
  xgboost(
    data = train[, 2:length(df)],
    label = train[, 1],
    nrounds = best_iterations[which.min(xgb_test_rmse)]+20,
    objective = "reg:squarederror",
    early_stopping_rounds = NULL,
    max_depth =  hyper_grid[which.min(xgb_test_rmse), ]$max_depth,
    eta = hyper_grid[which.min(xgb_test_rmse), ]$eta,
    gamma = hyper_grid[which.min(xgb_test_rmse), ]$gamma,
    subsample = hyper_grid[which.min(xgb_test_rmse), ]$subsample,
    colsample_bytree = hyper_grid[which.min(xgb_test_rmse), ]$colsample_bytree,
    print_every_n = 10
  )


pred_xgb <- predict(m1_xgb, test[, 2:length(df)])

yhat <- exp(pred_xgb)
y <- exp(test[, 1])

postResample(yhat, y)


# r <- y - yhat
# plot(r, ylab = "residuals")

plot(y,
     yhat,
     xlab = "actual",
     ylab = "predicted")
abline(lm(yhat ~ y))
abline(coef = c(0,1))

#plot first 3 trees of model
# xgb.plot.tree(model = m1_xgb, trees = 0:2)

importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix[1:10,], xlab = "Feature Importance")



