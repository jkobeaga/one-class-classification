weight_classification <- function(df,prop=0.05, file_name, C, gamma, weight_normal, weight_anomaly,
                                  test = F, cluster = T, metric = "recall"){
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
    
    training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
    testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))
  }
  else{
    training <- df
  }
  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  
  ## TRAINING
  first <- T
  for(cost in C){
    for(sig in gamma){
      for(p0 in weight_normal){
        for(p1 in weight_anomaly){
          model <- svm(formulae, data = training, kernel = "sigmoid", gamma = sig, cost = cost,
                       class.weights = c("No" = p0, "Yes" = p1))
          
          cm <- confusionMatrix(predict(model,training[which(complete.cases(training)),]),
                                training[which(complete.cases(training)),ncol(training)], positive = "Yes")
          if(first == T){
            best_pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                           round(cm$table[2,2],2), cost, sig, round(cm$byClass[4],2), p0, p1,
                           model$nSV[1], model$nSV[2])
            first <- F
          }
          else{
            pred2 <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                       round(cm$table[2,2],2), cost, sig, round(cm$byClass[4],2), p0, p1, 
                       model$nSV[1], model$nSV[2])
            best_pred <- best_prediction(best_pred, pred2, metric = metric)
          }
        }
      }
    }
  }
  if(test == T){
    cm <- confusionMatrix(predict(model,testing[which(complete.cases(testing)),]),
                          testing[which(complete.cases(testing)),ncol(testing)], positive = "Yes")
    
    params_test <- c(round(cm$table[1,1],2), round(cm$table[1,2],2),round(cm$table[2,1],2),
                     round(cm$table[2,2],2), round(cm$byClass[4],2))
  }
  else params_test <- c()
  list(best_pred, params_test)
}