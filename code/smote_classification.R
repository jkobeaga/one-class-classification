smote_classification <- function(df,prop=0.05, file_name){
  # Dividimos en train y test (70-30)
  # index <- createDataPartition(df[,dim(df)[2]], list = FALSE, p = 0.7)
  # training <- df[index,]
  # testing <- df[-index,]
  # training <- data_split(training, prop = prop)
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  training[, ncol(training)] <- as.factor(training[, ncol(training)])
  testing[, ncol(testing)] <- as.factor(testing[, ncol(testing)])
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  
  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  cat("Before Smote:", dim(training),length(which(training[,ncol(training)]==1)),"\n")
  training <- SMOTE(formulae, data = training, perc.over = 3000, perc.under = 100)
  training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
  testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))

  ## TRAINING
  cat("After Smote:", dim(training),length(which(training[,ncol(training)]=="Yes")),"\n")
  CV_Folds <- createMultiFolds(training[,ncol(training)], k=10, times = 1)
  train_control <- trainControl(method = "cv", index = CV_Folds, classProbs=T, allowParallel = T)
  grid_svm <- expand.grid(sigma=c(seq(0.1,1),0.3),C=seq(0.5,3,0.5))
  model<- train(formulae, data = training, 
                   tuneLength=5,method = "svmRadial",#trControl=train_control,
                   preProcess = c("center","scale"),tuneGrid = grid_svm, na.action = na.omit)
  cm <- confusionMatrix(predict(model,testing), testing[,dim(testing)[2]], positive = "Yes")
  cat(file_name, round(cm$table[1,1],2),round(cm$table[1,2],2),round(cm$table[2,1],2),
      round(cm$table[2,2],2), round(cm$byClass[6],2), round(cm$byClass[4],2),
      round(cm$overall[2],2),"\n",  file = "results/results_SMOTE.txt", append = T, sep = ",")
  cm
}

datasets_names <- c("blood_trans", "breast", "ecoli", "fertility", "haberman", "liver", "ionosphere",
                    "mammo", "parkinson", "biodegrad", "seeds")# skin
cat("file,TN,FP,FN,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_SMOTE.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  smote_classification(datasets[[i]], file_name = datasets_names[i])
  
}