smote_classification <- function(df,prop=0.05, file_name, cost_list, gamma_list,
                                 prop_majority = c(50,60,65), test = F, cluster = F){
  if(cluster == F){
    training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
    testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
    
    # Scaling the datasets [0,1]
    training <- scale_df(training)
    testing <- scale_df(testing)
    
    
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
    # print(dim(training))
    # 70-30  without removing the majority class observations.
    # if(prop_majority == 70)training <- SMOTE(formulae, data = training, perc.over = 1000,
    #                                          per.under = 1)
    if(prop_majority == 65)training <- SMOTE(formulae, data = training, perc.over = 1500,
                                             per.under = 200)
    # 60-40 without removing the majority class observations.
    if(prop_majority == 60)training <- SMOTE(formulae, data = training, perc.over = 800,
                                            perc.under = 163)
    # 50-50 without removing the majority class observations.
    if(prop_majority == 50)training <- SMOTE(formulae, data = training, perc.over = 900,
                                            perc.under = 113)
    training[,ncol(training)] <- as.factor(ifelse(training[,ncol(training)]==0,"No","Yes"))
    testing[,ncol(testing)] <- as.factor(ifelse(testing[,ncol(testing)]==0,"No","Yes"))
    cat("After Smote:", dim(training),length(which(training[,ncol(training)]=="No")),
        length(which(training[,ncol(training)]=="Yes")),"\n")
    cat("After Smote percentage:", length(which(training[,ncol(training)]=="No"))/dim(training)[1],"\n")
    
    # print(dim(training))
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
      # cat("\nPREDICT ",length(which(!is.na(training))),length(which(is.na(training))))
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
        best_pred <- best_prediction(best_pred, pred2)
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
  # cat(file_name, best_pred[5], best_pred[6], best_pred[8], best_pred[9], best_pred[1], best_pred[2],
  #     best_pred[3], best_pred[4],
  #     best_pred[7], "\n",file = "results/results_SMOTE.txt", append = T, sep = ",")
  # cm
  list(best_pred, params_test)
}

# cat("file,cost,gamma,nSV_0,nSV_1,TN,FN,FP,TP,Kappa,\n", file = "results/results_SMOTE.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  gamma_list <- seq(0.1, 1, 0.1)
  cost_list <- seq(0.5,3,0.5)
  cat("\nDATASETTTTTT: ", datasets_names[i], "\n")
  smote_classification(datasets[[i]], file_name = datasets_names[i], cost_list = cost_list,
                       gamma_list = gamma_list, cluster = F, prop_majority = 65)
  
}