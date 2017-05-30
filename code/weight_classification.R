weight_classification <- function(df,prop=0.05, file_name, C, sigma, weight_normal, weight_anomaly){
  # Dividimos en train y test (70-30)
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  # testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  # testing <- scale_df(testing)
  
  target <- colnames(training)[ncol(training)]
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  
  formulae <- formula(paste(target, "~."))
  training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
  # testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))
  
  ## TRAINING
  C=seq(0.5,3,0.5)
  sigma=seq(0.1,1,0.3)
  weight_normal <- c(0.5,1)
  weight_anomaly <- seq(2,20,2)
  first <- T
  for(cost in C){
    for(sig in sigma){
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
            best_pred <- best_prediction(best_pred, pred2)
            # cat("P0: ", best_pred[8], "P1: ", best_pred[9], "\n")
            
            
          }
        }
      }
    }
  }
  # cat(file_name, best_pred[8], best_pred[9], best_pred[6], best_pred[5], best_pred[10], best_pred[11],
  #     best_pred[1], best_pred[2], best_pred[3], best_pred[4], best_pred[7],
  #     "\n",file = "results/results_weights.txt", append = T, sep = ",")
  # cm
  best_pred
}

cat("file,P0,P1,cost,sigma,nSV_0,nSV_1,TN,FN,FP,TP,Kappa,\n", file = "results/results_weights.txt",
    append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  weight_classification(datasets[[i]], file_name = datasets_names[i])
  
}

