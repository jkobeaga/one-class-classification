weight_classification <- function(df,prop=0.05, file_name){
  # Dividimos en train y test (70-30)
  index <- createDataPartition(df[,dim(df)[2]], list = FALSE, p = 0.7)
  training <- df[index,]
  testing <- df[-index,]
  training <- data_split(training, prop = prop)
  target <- colnames(training)[ncol(training)]
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  
  formulae <- formula(paste(target, "~."))
  training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
  testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))
  
  ## TRAINING
  C=seq(0.5,3,0.5)
  sigma=seq(0.1,1,0.3)
  weight_normal <- c(0.5,1)
  weight_anomaly <- seq(2,20,2)
  for(c in C){
    for(sig in sigma){for(p0 in weight_normal){
      for(p1 in weight_anomaly){
        
        model <- svm(formulae, data = training, kernel = "sigmoid", gamma = sig, cost = c,
                     class.weights = c("No" = p0, "Yes" = p1))
        cm <- confusionMatrix(predict(model,testing), testing[,dim(testing)[2]], positive = "Yes")
        cat(file_name, p0,p1,round(cm$table[1,1],2),round(cm$table[1,2],2),round(cm$table[2,1],2),
            round(cm$table[2,2],2), round(cm$byClass[6],2), round(cm$byClass[4],2),
            round(cm$overall[2],2),"\n",  file = "results/results_weights.txt", append = T, sep = ",")
        
        
      }
    }
      
      
    }
  }
  cm
}

datasets_names <- c("blood_trans", "breast", "ecoli", "fertility", "haberman", "liver", "ionosphere",
                    "mammo", "parkinson", "biodegrad", "seeds")# skin
cat("file,P0,P1,TN,FP,FN,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_weights.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  weight_classification(datasets[[i]], file_name = datasets_names[i])
  
}
