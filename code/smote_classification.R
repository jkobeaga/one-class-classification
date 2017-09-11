smote_classification <- function(df,prop=0.05, file_name, cost_list, gamma_list,
                                 prop_majority = c(50,60,65), test = F, cluster = F,
                                 metric = "recall"){
  if(cluster == F){
    training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
    testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
    

    # removing variables with null variance
    training[,ncol(training)] <- as.factor(training[,ncol(training)])
    testing[,ncol(testing)] <- as.factor(testing[,ncol(testing)])
    null_var <- nearZeroVar(x = training[,-ncol(training)])
    if(length(null_var)>0)training <- training[,-null_var]
    if(length(null_var)>0)testing <- testing[,-null_var]
    
    target <- colnames(training)[ncol(training)]
    formulae <- formula(paste(target, "~."))
    cat("Before Smote:", dim(training),length(which(training[,ncol(training)]==0)),
        length(which(training[,ncol(training)]==1)),"\n")
    
    # 65-35  by doing downsampling and oversampling
    if(prop_majority == 65)training <- SMOTE(formulae, data = training, perc.over = 1500,
                                             per.under = 200)
    # 60-40 by doing downsampling and oversampling
    if(prop_majority == 60)training <- SMOTE(formulae, data = training, perc.over = 800,
                                            perc.under = 163)
    # 50-50 by doing downsampling and oversampling
    if(prop_majority == 50)training <- SMOTE(formulae, data = training, perc.over = 900,
                                            perc.under = 113)
    
    training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
    testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))
    cat("After Smote:", dim(training),length(which(training[,ncol(training)]=="No")),
        length(which(training[,ncol(training)]=="Yes")),"\n")
    cat("After Smote percentage:", length(which(training[,ncol(training)]=="No"))/dim(training)[1],"\n")
    
    # Scaling the datasets [0,1]
    training <- scale_df(training)
    testing <- scale_df(testing)
  }
  else{
    training <- df
    testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  }
  
  
  ## TRAINING

  first <- T

  for(gamma in gamma_list){
    for(cost in cost_list){
      model <- svm(training[,ncol(training)]~., data = training,
                   kernel = "radial", cost = cost, gamma= gamma, cross=10)
      cm <- confusionMatrix(predict(model,training[which(complete.cases(training)),]),
                            training[which(complete.cases(training)),ncol(training)], positive = "Yes")
      if(first == T){
        best_pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                       round(cm$table[2,2],2), cost, gamma, round(cm$byClass[4],2), model$nSV[1],
                       model$nSV[2])
        first <- F
      }
      else{
        pred2 <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                   round(cm$table[2,2],2), cost, gamma, round(cm$byClass[4],2), model$nSV[1],
                   model$nSV[2])
        best_pred <- best_prediction(best_pred, pred2, metric = metric)
      }
    }
  }
  if(test == T){
    cm <- confusionMatrix(predict(model,testing[which(complete.cases(testing)),]),
                          testing[which(complete.cases(testing)),ncol(testing)], positive = "Yes")
    
    params_test <- c(round(cm$table[1,1],2), round(cm$table[1,2],2),round(cm$table[2,1],2),
                     round(cm$table[2,2],2), round(cm$byClass[4],2))
  }
  else params_test <- c()
  list(best_pred, params_test)
}