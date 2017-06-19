LR_classification <- function(df,prop=0.05, file_name){
  # Dividimos en train y test (70-30)
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  # testing <- scale_df(testing)
  
  # Taking 10% of positive observations
  n_pos <- which(training[,ncol(training)]=="1")
  # n_pos_test <- which(testing[,ncol(testing)]=="1")
  training <- training[c(n_pos, sample(which(training[,ncol(training)]=="0"),length(n_pos)*10)),]
  # testing <- testing[c(n_pos_test, sample(which(testing[,ncol(testing)]=="0"),length(n_pos_test)*10)),]
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  if(length(null_var)>0)testing <- testing[,-null_var]

  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
  testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))

  # ## TRAINING
  model<- train(formulae, data = training,
                tuneLength=5,method = "glm", family="binomial",#trControl=train_control,
                preProcess = c("center","scale"), na.action = na.omit)
  # Prediction before correction
    # Training
  cm <- confusionMatrix(predict(model,training), training[,dim(training)[2]], positive = "Yes")
  pred <- c(cm$table[1,1], cm$table[1,2], cm$table[2,1], cm$table[2,2], round(cm$overall[2],2))
    # Testing
  cm <- confusionMatrix(predict(model,testing), testing[,dim(testing)[2]], positive = "Yes")
  pred <- c(pred,cm$table[1,1], cm$table[1,2], cm$table[2,1], cm$table[2,2], round(cm$overall[2],2))
  
  # Prediction after correction
  model$finalModel$coefficients[1] <- model$finalModel$coefficients[1]-log(((1-0.1)*0.1)*(1-prop)*prop)
    # Training
  cm <- confusionMatrix(predict(model,training), training[,dim(training)[2]], positive = "Yes")
  pred <- c(pred, cm$table[1,1], cm$table[1,2], cm$table[2,1], cm$table[2,2], round(cm$overall[2],2))
    # Testing
  cm <- confusionMatrix(predict(model,testing), testing[,dim(testing)[2]], positive = "Yes")
  pred <- c(pred, cm$table[1,1], cm$table[1,2], cm$table[2,1], cm$table[2,2], round(cm$overall[2],2))
  pred
}

cat("file,Corrected,TN,FN,FP,TP,Kappa,\n", file = "results/results_LR.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  LR_classification(datasets[[i]], file_name = datasets_names[i])
  
}