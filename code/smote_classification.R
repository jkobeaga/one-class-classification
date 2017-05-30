smote_classification <- function(df,prop=0.05, file_name, C, sigma){
  # Dividimos en train y test (70-30)
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  # testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  training[, ncol(training)] <- as.factor(training[, ncol(training)])
  # testing[, ncol(testing)] <- as.factor(testing[, ncol(testing)])
  
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  # testing <- scale_df(testing)
  
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  
  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  cat("Before Smote:", dim(training),length(which(training[,ncol(training)]==1)),"\n")
  training <- SMOTE(formulae, data = training, perc.over = 3000, perc.under = 100)
  training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
  # testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))
  cat("After Smote:", dim(training),length(which(training[,ncol(training)]=="No")),"\n")
  
  
  ## TRAINING
  sigma_list <- seq(0.1, 1, 0.1)
  cost_list <- seq(0.5,3,0.5)
  first <- T
  
  for(sigma in sigma_list){
    for(cost in cost_list){
      model <- svm(training[,ncol(training)]~., data = training,
                   kernel = "radial", cost = cost, gamma= sigma, cross=10)
      # cat("\nPREDICT ",length(which(!is.na(training))),length(which(is.na(training))))
      cm <- confusionMatrix(predict(model,training[which(complete.cases(training)),]),
                            training[which(complete.cases(training)),ncol(training)], positive = "Yes")
      if(first == T){
        best_pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                       round(cm$table[2,2],2), cost, sigma, round(cm$byClass[4],2), model$nSV[1],
                       model$nSV[2])
        first <- F
      }
      else{
        pred2 <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                   round(cm$table[2,2],2), cost, sigma, round(cm$byClass[4],2), model$nSV[1],
                   model$nSV[2])
        best_pred <- best_prediction(best_pred, pred2)
      }
    }
  }
  # cat(file_name, best_pred[5], best_pred[6], best_pred[8], best_pred[9], best_pred[1], best_pred[2],
  #     best_pred[3], best_pred[4],
  #     best_pred[7], "\n",file = "results/results_SMOTE.txt", append = T, sep = ",")
  # cm
  best_pred
}

cat("file,cost,sigma,nSV_0,nSV_1,TN,FN,FP,TP,Kappa,\n", file = "results/results_SMOTE.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  smote_classification(datasets[[i]], file_name = datasets_names[i])
  
}