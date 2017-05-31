cluster_smote <- function(df,prop=0.05, file_name, C, gamma_list){
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
  training_1 <- data.frame(matrix(ncol = ncol(training), nrow = 0))
  for(clus in unique(training$cluster)){
    # Take observations for each cluster
    train_clust <- training[which(training$cluster == clus), -ncol(training)]
    test_clust <- testing[which(testing$cluster == clus), -ncol(testing)]
    # If all the observation from test are from the same class assign that class to all observation 
    # from test
    train_clust[,ncol(train_clust)] <- as.factor(train_clust[,ncol(train_clust)])
    
    if(length(unique(train_clust[, ncol(train_clust)]))==1){
      train_clust[,ncol(train_clust)] <- as.factor(ifelse(train_clust[,ncol(train_clust)]==0,"No","Yes"))
      training_1 <- rbind(training_1, train_clust)
      predictions_in <- as.character(train_clust[, ncol(train_clust)])
      predictions <- append(predictions, predictions_in)

    }
    else if(dim(train_clust)[1]<10){
      warning("This cluster has less than 10 observations")
      train_clust[,ncol(train_clust)] <- as.factor(ifelse(train_clust[,ncol(train_clust)]==0,"No","Yes"))
      training_1 <- rbind(training_1, train_clust)
      if(table(train_clust[,ncol(train_clust)])>table(train_clust[,ncol(train_clust)])){
        predictions <- c(predictions, rep(0,dim(train_clust)[1]))
      }
      else{
        predictions <- c(predictions, rep(1,dim(train_clust)[1]))
        
      }
    }
    else{
      cat("Before Smote Cluster:", dim(train_clust)[1],length(which(train_clust[,ncol(train_clust)]==1)),"\n")
      train_clust <- SMOTE(formulae, data = train_clust, perc.over = 3000, perc.under = 100)
      train_clust[,ncol(train_clust)] <- as.factor(ifelse(train_clust[,ncol(train_clust)]==0,"No","Yes"))
      cat("After Smote Cluster:", dim(train_clust)[1],length(which(train_clust[,ncol(train_clust)]=="No")),"\n")
      proportion <- round(length(which(train_clust[,ncol(train_clust)]=="1"))/dim(train_clust)[1],2)
      params <- smote_classification(train_clust, file_name, cost_list = C, gamma_list = gamma_list,
                                    cluster = T)
      model <- svm(formulae, data = train_clust,
                   kernel = "radial", gamma= params[6], cost = params[5], cross=10)

      # store the smote data in a data frame for the final prediction
      training_1 <- rbind(training_1, train_clust[which(complete.cases(train_clust)),])
      in_clus_pred <- as.character(predict(model,train_clust[which(complete.cases(train_clust)),]))
      predictions <- c(predictions, in_clus_pred)
    }
  }
  predictions <- as.factor(predictions)
  cm <- confusionMatrix(predictions, training_1[,ncol(training_1)], positive = "Yes")
  pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
            round(cm$table[2,2],2), round(cm$byClass[4],2))
  # print(pred)
  # cat("\n")
  pred
}
# cat("file,nu,TN,FP,FN,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_cluster_svdd.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  gamma_list <- seq(0.1,0.6,0.05)
  C <- c(seq(0.01,0.2,0.02))
  cluster_smote(datasets[[i]], file_name = datasets_names[i], C = C, gamma_list = gamma_list)
  
}