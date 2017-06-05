cluster_nu <- function(df,prop=0.05, file_name, C, nu_list = prop, gamma_list){
  # Load training and testing
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  testing <- scale_df(testing)
  cat("Number of anomalies: ",length(which(testing[,ncol(testing)] == 1)))
  
  
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
  # nu_list <- seq(0.01,0.2,0.02)
  # gamma_list <- seq(0.1,0.6,0.05)
  for(clus in unique(training$cluster)){
    # Take observations for each cluster
    train_clust <- training[which(training$cluster == clus), -ncol(training)]
    test_clust <- testing[which(testing$cluster == clus), -ncol(testing)]
    
    # If all the observation from test are from the same class assign that class to all observation 
    # from test
    train_clust[,ncol(train_clust)] <- as.factor(train_clust[,ncol(train_clust)])
    test_clust[,ncol(test_clust)] <- as.factor(test_clust[,ncol(test_clust)])
    proportion <- round(length(which(train_clust[,ncol(train_clust)]=="1"))/dim(train_clust)[1],3)
    if(length(unique(train_clust[, ncol(train_clust)]))==1 | proportion<0.05){
      predictions <- c(predictions, rep(0, dim(train_clust)[1]))
      predictions_test <- c(predictions_test,
                            rep(train_clust[1, ncol(train_clust)], dim(test_clust)[1]))
      
    }
    else if(dim(train_clust)[1]<10){
      warning("This cluster has less than 10 observations")
      if(table(train_clust[,ncol(train_clust)])[1]>table(train_clust[,ncol(train_clust)])[2]){
        predictions <- c(predictions, rep(0,dim(train_clust)[1]))
        predictions_test <- c(predictions_test, rep(0,dim(test_clust)[1]))
      }
      else{
        predictions <- c(predictions, rep(1,dim(train_clust)[1]))
        predictions_test <- c(predictions_test, rep(1,dim(test_clust)[1]))
      }
    }
    else{
      # cat(proportion,length(which(train_clust[,ncol(train_clust)]=="1")),dim(train_clust)[1], "\n")
      train_test <- nu_classification(train_clust, file_name, C = C, nu_list = proportion, gamma_list = gamma_list,
                                    cluster = T)
      params_train <- train_test[[1]]
      model <- svm(formulae, data = train_clust, type = "nu-classification",
                   kernel = "radial", nu = proportion, gamma= params_train[6], cost = params_train[5],
                   cross=10)
      # Prediction in training test
      in_clus_pred <- predict(model,train_clust)
      predictions <- c(predictions, in_clus_pred)
      # Prediction in testing set
      # in_clus_pred_test <- ifelse(predict(model,test_clust)==T, 1, 0)
      # print(predict(model,test_clust))
      # cat("\nasdsadadad\n")
      in_clus_pred_test <- predict(model,test_clust)
      predictions_test <- append(predictions_test, in_clus_pred_test)
      # print(predictions_test)
      # print(table(predictions))
      # cat("ASDAD\n")
    }
  }
  # print(table(predictions))
  predictions <- ifelse(predictions=="2",1,0)
  cm <- confusionMatrix(predictions, training[,ncol(training)-1], positive = "1")
  pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
            round(cm$table[2,2],2), round(cm$byClass[4],2))
  # print(testing[,ncol(testing)-1])
  # print(predictions_test)
  predictions_test <- ifelse(predictions_test=="2",1,0)
  cm <- confusionMatrix(predictions_test, testing[,ncol(testing)-1], positive = "1")
  pred_test <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                 round(cm$table[2,2],2), round(cm$byClass[4],2))
  list(pred,pred_test)
}
# cat("file,nu,TN,FP,FN,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_cluster_svdd.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  # nu_list <- seq(0.01,0.2,0.02)
  gamma_list <- seq(0.1,5,0.3)
  C <- c(seq(0.01,3,0.2))
  cluster_nu(datasets[[i]], file_name = datasets_names[i], C = C, nu_list = prop, gamma_list = gamma_list)
  
}