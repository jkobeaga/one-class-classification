svdd_classification <- function(df,prop=0.05, file_name){
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
  nu_list <- seq(0.01,0.2,0.02)
  gamma_list <- seq(0.1,0.6,0.05)
  
  for(clus in unique(training$cluster)){
    # Take observations for each cluster
    train_clust <- training[which(training$cluster == clus), -ncol(training)]
    test_clust <- testing[which(testing$cluster == clus), -ncol(testing)]
    
    # If all the observation from test are from the same class assign that class to all observation 
    # from test
    if(length(unique(train_clust[, ncol(train_clust)]))==1){
      predictions <- c(predictions,
                       rep(unique(train_clust[, ncol(train_clust)]), dim(train_clust)[1]))
    }
    else{
      first <- T
      for(gam in gamma_list){
        for(nu in nu_list){
          # cat(dim(training)[2], summary(training[,10]))
          model <- svm(formulae, data = train_clust[,-c(ncol(train_clust)-1,ncol(train_clust))],
                       type = "one-classification", kernel = "radial", nu = nu, gamma= gam, cross=10)
          # cat(predict(model,testing))
          cm <- confusionMatrix(ifelse(predict(model,training) == T,1,0), training[,ncol(training)-2],
                                positive = "1")
          if(first){
            pred1 <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                       round(cm$table[2,2],2), gam, nu)
            first <- F
          }
          else{
            pred2 <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                       round(cm$table[2,2],2), gam, nu)
            pred1 <- best_prediction(pred1, pred2)
          }
        }
      }
    }
    cat(file_name, gam, nu, pred[1], pred[2], pred[3], pred[4],"\n",
        file = "results/results_svdd.txt", append = T, sep = ",")
  cm
}

datasets_names <- c("blood_trans", "breast", "ecoli", "fertility", "haberman", "liver", "ionosphere",
                    "mammo", "parkinson", "biodegrad", "seeds")# skin
cat("file,nu,TN,FP,FN,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_svdd.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  svdd_classification(datasets[[i]], file_name = datasets_names[i])
  
}