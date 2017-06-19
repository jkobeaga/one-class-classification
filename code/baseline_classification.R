baseline_classification <- function(df,prop=0.05, file_name, C, sigma, test = F){
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  testing <- scale_df(testing)
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  if(length(null_var)>0)testing <- testing[,-null_var]
  
  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
  testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))
  
  ## TRAINING

  model <- best.svm(formulae, data = training, kernel = "sigmoid", gamma = sigma, cost = C, cross = 10)
  cm <- confusionMatrix(predict(model,training), training[,dim(training)[2]], positive = "Yes")
  params <- c(model$cost, model$gamma, round(cm$table[1,1],2),
              round(cm$table[1,2],2),round(cm$table[2,1],2), round(cm$table[2,2],2),
              round(cm$byClass[4],2))
  if(test == T){
    cm <- confusionMatrix(predict(model,testing), testing[,dim(testing)[2]], positive = "Yes")
    params_test <- c(round(cm$table[1,1],2), round(cm$table[1,2],2),round(cm$table[2,1],2),
                     round(cm$table[2,2],2), round(cm$byClass[4],2))
  }
  else params_test <- c()
  list(params, params_test)
}

