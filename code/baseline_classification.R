baseline_classification <- function(df,prop=0.05, file_name){
  # Dividimos en train y test (70-30)
  # index <- createDataPartition(df[,dim(df)[2]], list = FALSE, p = 0.7)
  # training <- df[index,]
  # testing <- df[-index,]
  # training <- data_split(training, prop = prop)
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  # testing <- scale_df(testing)
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  
  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
  # testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))
  
  ## TRAINING
  C=seq(0.5,3,0.5)
  sigma=seq(0.1,1,0.3)
  weight_normal <- c(0.5,1)
  weight_anomaly <- seq(2,20,2)
  for(c in C){
    for(sig in sigma){
      cat(file_name,round(cm$table[1,1],2),round(cm$table[1,2],2),round(cm$table[2,1],2),
          round(cm$table[2,2],2), round(cm$byClass[6],2), round(cm$byClass[4],2),
          round(cm$overall[2],2),"\n",  file = "results/results_baseline.txt", append = T, sep = ",")
      # cat(dim(training)[2], summary(training[,10]))
      model <- svm(formulae, data = training, kernel = "sigmoid", gamma = sig, cost = c)
      # cat(predict(model,testing))
      cm <- confusionMatrix(predict(model,training), training[,dim(training)[2]], positive = "Yes")
      if(first == T){
        best_pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                       round(cm$table[2,2],2), c, sig, round(cm$byClass[4],2))
        first <- F
      }
      else{
        pred2 <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                   round(cm$table[2,2],2), c, sig, round(cm$byClass[4],2))
        best_pred <- best_prediction(best_pred, pred2)
      }
    }
  }
  cat(file_name, best_pred[6], best_pred[5], best_pred[1], best_pred[2], best_pred[3], best_pred[4],
      best_pred[7], "\n",file = "results/results_baseline.txt", append = T, sep = ",")
  
  
  cm
}

datasets_names <- c("blood_trans", "breast", "ecoli", "fertility", "haberman", "liver", "ionosphere",
                    "mammo", "parkinson", "biodegrad", "seeds")# skin
cat("file,cost,sigma,TN,FN,FP,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_baseline.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  baseline_classification(datasets[[i]], file_name = datasets_names[i])
  
}

