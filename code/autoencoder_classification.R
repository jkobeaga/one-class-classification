autoencoder_classification <- function(df,prop=0.05, file_name, l1_neurons, l2_neurons, l3_neurons,
                                       test = F, metric = "recall"){
  # Train-Test (70-30)
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  testing <- scale_df(testing)
  
  # Removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  if(length(null_var)>0)testing <- testing[,-null_var]
  
  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  
  
  ## Initializing H2O (After starting H2O, you can use and view every operation done over H2O in the
  ## Web UI at http://localhost:54321)
  training.hex <- as.h2o(training)
  testing.hex <- as.h2o(testing)
  
  
  
  
  ## TRAINING
  if(test == F){
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
  }

  first <- T
  for(l1 in l1_neurons){
    for(l2 in l2_neurons){
      for(l3 in l3_neurons){
        # With h2o.deeplearning we create an autoencoder model to detect anomalies
        training_mdl <- h2o.deeplearning(x = 1:10, training_frame = training.hex, autoencoder = TRUE,
                                         hidden = c(l1, l2, l3), epochs = 100,seed=1)
        # h20.anomaly calculates the reconstruction error for each feature, i.e, using the object created
        # with the autoencoder we reconstruct the variables and calculate the error. Then we have to 
        # fix a treshold to determine when is an observation classified as an anomaly. To fix the 
        # treshold we use the rate of normal/anomaly rate of the training dataset.
        errors_train <- as.data.frame(h2o.anomaly(training_mdl, training.hex, per_feature = FALSE))
        # Fix the treshold of train
        errors_train_ord <- errors_train[order(errors_train[,1]),]
        treshold <- errors_train_ord[round(dim(training.hex)[1]*(1-prop))]
        
        # Using the treshold to determine if an observation is normal
        result_train <- errors_train
        pred_class <- factor(ifelse(result_train[,1] > treshold,1,0), levels = c("0","1"))
        cm <- confusionMatrix(pred_class, training[,ncol(training)],
                              positive = "1")
        if(first == T){
          best_pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                     round(cm$table[2,2],2), l1, l2, l3, round(cm$byClass[4],2))
          first <- F
        }
        else{
          pred2 <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                     round(cm$table[2,2],2), l1, l2, l3, round(cm$byClass[4],2))
          best_pred <- best_prediction(best_pred, pred2, metric = metric)
        }
      }
    }
  }
  if(test == T){
    result_test <- as.data.frame(h2o.anomaly(training_mdl, testing.hex, per_feature = FALSE))
    pred_class <- factor(ifelse(result_test[,1] > treshold,1,0), levels = c("0","1"))
    cm <- confusionMatrix(pred_class, testing[,ncol(testing)],positive = "1")
    params_test <- c(round(cm$table[1,1],2), round(cm$table[1,2],2),round(cm$table[2,1],2),
                     round(cm$table[2,2],2), round(cm$byClass[4],2))
  }
  else params_test <- c()
  list(best_pred, params_test)
}