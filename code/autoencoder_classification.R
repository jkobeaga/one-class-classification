autoencoder_classification <- function(df,prop=0.05, file_name){
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
  
  
  ## Initializing H2O (After starting H2O, you can use and view every operation done over H2O in the
  ## Web UI at http://localhost:54321)
  h2o.init()
  training.hex <- as.h2o(training)
  testing.hex <- as.h2o(testing)
  
  
  
  
  ## TRAINING
  l1_neurons <- c(4,5)
  l2_neurons <- c(4,5)
  l3_neurons <- c(4,5)
  # if(dim(training)[2]<10){
  #   l1_neurons <- seq(1,dim(training)[2],3)
  #   l2_neurons <- seq(1,dim(training)[2],3)
  #   l3_neurons <- seq(1,dim(training)[2],3)
  # }
  # if(dim(training)[2]>=10 & dim(training)[2]<20){
  #   l1_neurons <- seq(10,dim(training)[2],3)
  #   l2_neurons <- seq(10,dim(training)[2],3)
  #   l3_neurons <- seq(10,dim(training)[2],3)
  # }
  # else{
  #   l1_neurons <- seq(20,dim(training)[2],5)
  #   l2_neurons <- seq(20,dim(training)[2],5)
  #   l3_neurons <- seq(20,dim(training)[2],5)
  # }
  for(l1 in l1_neurons){
    for(l2 in l2_neurons){
      for(l3 in l3_neurons){
        training_mdl <- h2o.deeplearning(x = 1:10, training_frame = training.hex, autoencoder = TRUE,
                                         hidden = c(l1, l2, l3), epochs = 10,seed=1)
        # Fix the treshold of train
        errors_train <- as.data.frame(h2o.anomaly(training_mdl, training.hex, per_feature = FALSE))
        errors_train <- errors_train[order(errors_train[,1]),]
        # cat(errors_train[419,])
        treshold <- errors_train[round(dim(training.hex)[1]*(1-prop))]
        
        # Using the treshold to determine if an observation is normal
        result_test <- as.data.frame(h2o.anomaly(training_mdl, testing.hex, per_feature = FALSE))
        pred_class <- as.factor(ifelse(result_test[,1] > treshold,1,0))
        cm <- confusionMatrix(pred_class, testing[,ncol(testing)-1],
                              positive = "1")
        cat(file_name, l1, l2, l3, round(cm$table[1,1],2),round(cm$table[1,2],2),round(cm$table[2,1],2),
            round(cm$table[2,2],2), round(cm$byClass[6],2), round(cm$byClass[4],2),
            round(cm$overall[2],2),"\n",  file = "results/results_autoencoder.txt", append = T, sep = ",")
        
        
        
      }
    }
  }
  cm
}

datasets_names <- c("blood_trans", "breast", "ecoli", "fertility", "haberman", "liver", "ionosphere",
                    "mammo", "parkinson", "biodegrad", "seeds")# skin
cat("file, l1, l2, l3,TN,FP,FN,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_autoencoder.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  autoencoder_classification(datasets[[i]], file_name = datasets_names[i])
  
}