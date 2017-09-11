nu_classification <- function(df,prop=0.05, file_name, C, nu_list = prop, gamma_list,test= F,
                              cluster = F, metric = "recall"){
  if(cluster == F){
    training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
    testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
    # Scaling the datasets [0,1]
    training <- scale_df(training)
    testing <- scale_df(testing)
    
    # removing variables with null variance
    null_var <- nearZeroVar(x = training[,-ncol(training)])
    if(length(null_var)>0)training <- training[,-null_var]
    if(length(null_var)>0)testing <- testing[,-null_var]
  }
  else{
    training <- df
  }

  ## TRAINING
  first <- T
  for(cost in C){
    for(gam in gamma_list){
      for(nu in nu_list){
        model <- svm(as.factor(training[,ncol(training)])~., data = training, type = "nu-classification",
                     kernel = "radial", nu = nu, gamma= gam, cost = cost, cross=10)
        cm <- confusionMatrix(predict(model,training), training[,ncol(training)],
                              positive = "1")
        if(first == T){
          best_pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                         round(cm$table[2,2],2), cost, gam, nu, round(cm$byClass[4],2))
          first <- F
        }
        else{
          pred2 <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                         round(cm$table[2,2],2), cost, gam, nu, round(cm$byClass[4],2))
          best_pred <- best_prediction(best_pred, pred2, metric = metric)
        }
      }
    }
  }
  if(test == T){
    cm <- confusionMatrix(ifelse(predict(model,testing) == T,1,0), testing[,ncol(testing)],
                          positive = "1")
    params_test <- c(round(cm$table[1,1],2), round(cm$table[1,2],2),round(cm$table[2,1],2),
                     round(cm$table[2,2],2), round(cm$byClass[4],2))
  }
  else params_test <- c()
  list(best_pred, params_test)
}