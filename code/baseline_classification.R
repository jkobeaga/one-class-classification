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
  testing <- scale_df(testing)
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  
  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
  testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))
  
  ## TRAINING
  C=seq(0.5,3,0.5)
  sigma=seq(0.1,1,0.3)
  weight_normal <- c(0.5,1)
  weight_anomaly <- seq(2,20,2)
  for(c in C){
    for(sig in sigma){
        model <- svm(formulae, data = training, kernel = "sigmoid", gamma = sig, cost = c)
        cm <- confusionMatrix(predict(model,testing), testing[,dim(testing)[2]], positive = "Yes")
        cat(file_name,round(cm$table[1,1],2),round(cm$table[1,2],2),round(cm$table[2,1],2),
            round(cm$table[2,2],2), round(cm$byClass[6],2), round(cm$byClass[4],2),
            round(cm$overall[2],2),"\n",  file = "results/results_baseline.txt", append = T, sep = ",")
    }
  }
  cm
}

datasets_names <- c("blood_trans", "breast", "ecoli", "fertility", "haberman", "liver", "ionosphere",
                    "mammo", "parkinson", "biodegrad", "seeds")# skin
cat("file,TN,FP,FN,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_baseline.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  baseline_classification(datasets[[i]], file_name = datasets_names[i])
  
}

