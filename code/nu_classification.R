nu_classification <- function(df,prop=0.05, file_name, C, nu_list = prop, gamma_list, cluster = T){
  if(cluster == F){
    training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
    # testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
    
  }
  else{
    training <- df
  }
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  # testing <- scale_df(testing)
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  
  
  ## TRAINING
  # C <- c(seq(0.01,0.2,0.02))
  # nu_list <- prop
  # gamma_list <- seq(0.1,0.6,0.05)
  first <- T
  for(cost in C){
    for(gam in gamma_list){
      for(nu in nu_list){
        # cat(dim(training)[2], summary(training[,10]))
        model <- svm(as.factor(training[,ncol(training)])~., data = training, type = "nu-classification",
                     kernel = "radial", nu = nu, gamma= gam, cost = cost, cross=10)      # cat(predict(model,testing))
        cm <- confusionMatrix(ifelse(predict(model,training) == T,1,0), training[,ncol(training)],
                              positive = "1")
        if(first == T){
          best_pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                         round(cm$table[2,2],2), cost, gam, nu, round(cm$byClass[4],2))
          first <- F
        }
        else{
          pred2 <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                         round(cm$table[2,2],2), cost, gam, nu, round(cm$byClass[4],2))
          best_pred <- best_prediction(best_pred, pred2)
        }
      }
    }
  }
  # cat(file_name, best_pred[5], best_pred[6], best_pred[7], best_pred[9], best_pred[10],
  #     best_pred[1], best_pred[2], best_pred[3], best_pred[4],best_pred[8],
  #     "\n",file = "results/results_Scholkopf.txt", append = T, sep = ",")
  # cm
  best_pred
}

cat("file,cost,gamma,nu,nSV_0,nSV_2,TN,FN,FP,TP,Kappa,\n", file = "results/results_Scholkopf.txt",
    append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  nu_classification(datasets[[i]], file_name = datasets_names[i])
  
}