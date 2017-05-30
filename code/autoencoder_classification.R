autoencoder_classification <- function(df,prop=0.05, file_name){
  # Dividimos en train y test (70-30)
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  # testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  # testing <- scale_df(testing)
  
  # Removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  
  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  
  
  ## Initializing H2O (After starting H2O, you can use and view every operation done over H2O in the
  ## Web UI at http://localhost:54321)
  training.hex <- as.h2o(training)
  # testing.hex <- as.h2o(testing)
  
  
  
  
  ## TRAINING
  if(dim(training)[2]<10){
    l1_neurons <- seq(2,dim(training)[2],3)
    l2_neurons <- seq(2,dim(training)[2],3)
    l3_neurons <- seq(2,dim(training)[2],3)
  }
  else if(dim(training)[2]>=10 & dim(training)[2]<20){
    l1_neurons <- seq(10,dim(training)[2],3)
    l2_neurons <- seq(10,dim(training)[2],3)
    l3_neurons <- seq(10,dim(training)[2],3)
  }
  else{
    l1_neurons <- seq(20,dim(training)[2],5)
    l2_neurons <- seq(20,dim(training)[2],5)
    l3_neurons <- seq(20,dim(training)[2],5)
  }
  for(l1 in l1_neurons){
    for(l2 in l2_neurons){
      for(l3 in l3_neurons){
        training_mdl <- h2o.deeplearning(x = 1:10, training_frame = training.hex, autoencoder = TRUE,
                                         hidden = c(l1, l2, l3), epochs = 100,seed=1)
        # Fix the treshold of train
        errors_train <- as.data.frame(h2o.anomaly(training_mdl, training.hex, per_feature = FALSE))
        errors_train <- errors_train[order(errors_train[,1]),]
        treshold <- errors_train[round(dim(training.hex)[1]*(1-prop))]
        
        # Using the treshold to determine if an observation is normal
        result_test <- as.data.frame(h2o.anomaly(training_mdl, training.hex, per_feature = FALSE))
        pred_class <- factor(ifelse(result_test[,1] > treshold,1,0), levels = c("0","1"))
        cm <- confusionMatrix(pred_class, training[,ncol(training)],
                              positive = "1")
        cat(file_name, l1, l2, l3, round(cm$table[1,1],2),round(cm$table[1,2],2),round(cm$table[2,1],2),
            round(cm$table[2,2],2), round(cm$byClass[6],2), round(cm$byClass[4],2),
            round(cm$overall[2],2),"\n",  file = "results/results_autoencoder.txt", append = T, sep = ",")
      }
    }
  }
  cm
}

cat("file, l1, l2, l3,TN,FN,FP,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_autoencoder.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  autoencoder_classification(datasets[[i]], file_name = datasets_names[i])
  
}