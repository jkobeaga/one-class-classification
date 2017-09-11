cluster_weight <- function(df,prop=0.05, file_name){
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
  # testing <- predict_cluster(training, testing)
  predictions <- c()
  
  # TRAINING
  for(clus in unique(training$cluster)){
    # Take observations for each cluster
    train_clust <- training[which(training$cluster == clus), -ncol(training)]
    train_clust[,ncol(train_clust)] <- ifelse(train_clust[,ncol(train_clust)]==0,"No","Yes")
    
    test_clust <- testing[which(testing$cluster == clus), -ncol(testing)]
    
    # If all the observation from test are from the same class assign that class to all observation 
    # from test
    train_clust[,ncol(train_clust)] <- as.factor(as.character(train_clust[,ncol(train_clust)]))
    if(length(unique(train_clust[, ncol(train_clust)]))==1){
      
      train_clust[,ncol(train_clust)] <- as.factor(ifelse(train_clust[,ncol(train_clust)]==0,"No","Yes"))
      predictions_in <- as.character(train_clust[, ncol(train_clust)])
      predictions <- append(predictions, predictions_in)
    }
    else if(dim(train_clust)[1]<10){
      warning("This cluster has less than 10 observations")
      if(table(train_clust[,ncol(train_clust)])[1]>table(train_clust[,ncol(train_clust)])[2]){
        predictions <- c(predictions, rep("No",dim(train_clust)[1]))
      }
      else{
        predictions <- c(predictions, rep("Yes",dim(train_clust)[1]))
        
      }
    }
    else{
      params <- weight_classification(train_clust, file_name, C = C, gamma = gamma,
                                      weight_normal = weight_normal, weight_anomaly = weight_anomaly,
                                      cluster = T)
      model<- train(formulae, data = train_clust,
                    tuneLength=5,method = "glm", family="binomial",
                    preProcess = c("center","scale"), na.action = na.omit)
      in_clus_pred <- as.character(predict(model,train_clust))
      predictions <- c(predictions, in_clus_pred)
    }
  }
  predictions <- as.factor(predictions)
  training[,ncol(training)-1] <- as.factor(as.character(ifelse(training[,ncol(training)-1]==0,"No","Yes")))
  cm <- confusionMatrix(predictions, training[,ncol(training)-1], positive = "Yes")
  pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
            round(cm$table[2,2],2), round(cm$byClass[4],2))
  pred
}