LR_classification <- function(df,prop=0.05, file_name){
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
  
  # Taking 10% of positive observations
  n_pos <- which(training[,ncol(training)]=="1")
  training <- training[c(n_pos, sample(which(training[,ncol(training)]=="0"),length(n_pos)*10)),]
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]

  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
  testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))

  # ## TRAINING
  model<- train(formulae, data = training,
                tuneLength=5,method = "glm", family="binomial",#trControl=train_control,
                preProcess = c("center","scale"), na.action = na.omit)
  # Prediction before correction
  
  cm <- confusionMatrix(predict(model,testing), testing[,dim(testing)[2]], positive = "Yes")
  cat(file_name,"F", round(cm$table[1,1],2),round(cm$table[1,2],2),round(cm$table[2,1],2),
      round(cm$table[2,2],2), round(cm$byClass[6],2), round(cm$byClass[4],2),
      round(cm$overall[2],2),"\n",  file = "results/results_LR.txt", append = T, sep = ",")
  
  # Prediction after correction
  model$finalModel$coefficients[1] <- model$finalModel$coefficients[1]-log(((1-0.1)*0.1)*(1-prop)*prop)
  cm <- confusionMatrix(predict(model,testing), testing[,dim(testing)[2]], positive = "Yes")
  cat(file_name,"T", round(cm$table[1,1],2),round(cm$table[1,2],2),round(cm$table[2,1],2),
      round(cm$table[2,2],2), round(cm$byClass[6],2), round(cm$byClass[4],2),
      round(cm$overall[2],2),"\n",  file = "results/results_LR.txt", append = T, sep = ",")
  cm
}

datasets_names <- c("blood_trans", "breast", "ecoli", "fertility", "haberman", "liver", "ionosphere",
                    "mammo", "parkinson", "biodegrad", "seeds")# skin
cat("file,Corrected,TN,FP,FN,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_LR.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  LR_classification(datasets[[i]], file_name = datasets_names[i])
  
}