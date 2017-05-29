cluster_svdd <- function(df,prop=0.05, file_name, C, nu_list = prop, gamma_list){
  # Load training and testing
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  testing <- scale_df(testing)
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[, -ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  
  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  training <- cluster_assign(training, file_name)
  # Predict the corresponing cluster for each observation of test using KNN
  testing <- predict_cluster(training, testing) 
  predictions <- c()
  
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
    # print(unique(train_clust[, ncol(train_clust)]))
    # cat("\n")
    # print(summary(train_clust))
    ##################### Up to here it's OK ##################
    if(length(unique(train_clust[, ncol(train_clust)]))==0){
      # cat("CCCCCCCCCCCCCCCC")
      predictions <- c(predictions,
                       rep(unique(train_clust[, ncol(train_clust)]), dim(train_clust)[1]))
      cat("\n")
      # print(predictions)
      cat("\n")
    }
    else{
      # print(train_clust)
      # cat("\noeeoeoeoeoeoeoe\n")
      # print(which(training$cluster==0))
      # cat("ASDASDASDSADAD")
      proportion <- round(length(which(train_clust[,ncol(train_clust)]=="1"))/dim(train_clust)[1],2)
      # cat("\n",length(which(train_clust[,ncol(train_clust)]=="1")),dim(train_clust)[1])
      # cat("\n",length(which(train_clust[,ncol(train_clust)]=="0")),dim(train_clust)[1])
      params <- svdd_classification(train_clust, file_name, C, nu_list = 0.01, gamma_list,
                                          cluster = T)
      cat("EEEEEEEEEEEEEEEE")
      model <- svm(formulae, data = training, type = "one-classification",
                   kernel = "radial", nu = params[4], gamma= params[6], cost = params[5], cross=10)
      in_clus_pred <- predict(model,train_clust)
      predictions <- c(predictions, in_clus_pred)
      # predikziñoak gehitzu traineko datueri falta da eitzie 
    }
  }
  # print(predictions)
  cm <- confusionMatrix(train_clust[,ncol(train_clust)], predictions, positive = "1")
  cat(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
    round(cm$table[2,2],2), cost, gam, nu, round(cm$byClass[4],2), "\n")
  }
datasets_names <- c("blood_trans", "breast", "ecoli", "fertility", "haberman", "liver", "ionosphere",
                    "mammo", "parkinson", "biodegrad", "seeds")# skin
# cat("file,nu,TN,FP,FN,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_svdd.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  nu_list <- seq(0.01,0.2,0.02)
  gamma_list <- seq(0.1,0.6,0.05)
  C <- c(seq(0.01,0.2,0.02))
  cluster_svdd(datasets[[i]], file_name = datasets_names[i], C, nu_list = prop, gamma_list)
  
}