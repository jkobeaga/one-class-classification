cluster_weight <- function(df,prop=0.05, file_name, C, gamma, weight_normal, weight_anomaly,
                           metric = "recall"){
  # Load training and testing
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  testing <- scale_df(testing)
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[, -ncol(training)])
  if(length(null_var)>0){
    training <- training[,-null_var]
    testing <- testing[,-null_var]
  }
  
  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  training <- cluster_assign(training, file_name)
  
  # Predict the corresponing cluster for each observation of test using KNN
  testing <- predict_cluster(training, testing)
  predictions <- c()
  predictions_test <- c()
  
  
  # TRAINING
  for(clus in unique(training$cluster)){
    # Take observations for each cluster
    train_clust <- training[which(training$cluster == clus), -ncol(training)]
    train_clust[,ncol(train_clust)] <- ifelse(train_clust[,ncol(train_clust)]==0,"No","Yes")
    
    test_clust <- testing[which(testing$cluster == clus), -ncol(testing)]
    test_clust[,ncol(test_clust)] <- ifelse(test_clust[,ncol(test_clust)]==0,"No","Yes")
    
    # If all the observation from test are from the same class assign that class to all observation 
    # from test
    train_clust[,ncol(train_clust)] <- as.factor(as.character(train_clust[,ncol(train_clust)]))
    if(length(unique(train_clust[, ncol(train_clust)]))==1){
      predictions_in <- as.character(train_clust[, ncol(train_clust)])
      predictions <- append(predictions, predictions_in)
      predictions_test <- c(predictions_test,
                            rep(train_clust[1, ncol(train_clust)], dim(test_clust)[1]))
    }
    else if(dim(train_clust)[1]<10){
      warning("This cluster has less than 10 observations")
      print(length(unique(train_clust[, ncol(train_clust)]))==1)
      print(table(train_clust[,ncol(train_clust)]))
      print(length(unique(train_clust[, ncol(train_clust)]))==1)
      if(table(train_clust[,ncol(train_clust)])[1]>table(train_clust[,ncol(train_clust)])[2]){
        predictions <- c(predictions, rep("No",dim(train_clust)[1]))
        predictions_test <- c(predictions_test, rep("No",dim(test_clust)[1]))
      }
      else{
        predictions <- c(predictions, rep("Yes",dim(train_clust)[1]))
        predictions_test <- c(predictions_test, rep("Yes",dim(test_clust)[1]))
      }
    }
    else{
      train_test <- weight_classification(train_clust, file_name, C = C, gamma = gamma,
                                      weight_normal = weight_normal, weight_anomaly = weight_anomaly,
                                      cluster = T, metric = metric)
      params <- train_test[[1]]
      model <- svm(formulae, data = train_clust, kernel = "radial", gamma= params[6],
                   cost = params[5], class.weights = c("No" = params[8], "Yes" = params[9]),
                   cross=10)
      in_clus_pred <- as.character(predict(model,train_clust))
      predictions <- c(predictions, in_clus_pred)
      #  Test prediction
      in_clus_pred_test <- predict(model,test_clust)
      predictions_test <- append(predictions_test,in_clus_pred_test)
    }
  }
  predictions <- as.factor(predictions)
  training[,ncol(training)-1] <- as.factor(as.character(ifelse(training[,ncol(training)-1]==0,"No","Yes")))
  cm <- confusionMatrix(predictions, training[,ncol(training)-1], positive = "Yes")
  pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
            round(cm$table[2,2],2), round(cm$byClass[4],2))
  predictions_test <- ifelse(predictions_test == 2, "Yes", "No")
  testing[,ncol(testing)-1] <- as.factor(ifelse(testing[,ncol(testing)-1]==0,"No","Yes"))
  cm <- confusionMatrix(predictions_test, testing[,ncol(testing)-1], positive = "Yes")
  pred_test <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                 round(cm$table[2,2],2), round(cm$byClass[4],2))
  # print(pred)
  # cat("\n")
  list(pred, pred_test)
  
}
# cat("file,nu,TN,FP,FN,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_cluster_svdd.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  C=seq(0.5,3,0.5)
  gamma_list=seq(0.1,1,0.3)
  weight_normal <- c(0.5,1)
  weight_anomaly <- seq(2,20,2)
  cluster_weight(datasets[[i]], file_name = datasets_names[i], C = C, gamma = gamma_list,
                 weight_normal = weight_normal, weight_anomaly = weight_anomaly)
  
}