svdd_classification <- function(df,prop=0.05, file_name){
  training <- read.csv(file = paste("./uci_datasets/", file_name, "/", "training.txt", sep = ""))
  testing <- read.csv(file = paste("./uci_datasets/", file_name, "/", "testing.txt", sep = ""))
  # Scaling the datasets [0,1]
  training <- scale_df(training)
  # testing <- scale_df(testing)
  
  # removing variables with null variance
  null_var <- nearZeroVar(x = training[,-ncol(training)])
  if(length(null_var)>0)training <- training[,-null_var]
  
  target <- colnames(training)[ncol(training)]
  formulae <- formula(paste(target, "~."))
  
  
  ## TRAINING
  nu_list <- seq(0.01,0.05,0.01)
  gamma_list <- seq(0.1,0.6,0.05)
  first <- T
  
  for(gam in gamma_list){
    for(nu in nu_list){
      # cat(dim(training)[2], summary(training[,10]))
      model <- svm(formulae, data = training, type = "one-classification",
                   kernel = "radial", nu = nu, gamma= gam, cross=10)
      # cat(predict(model,testing))
      cm <- confusionMatrix(ifelse(predict(model,training) == T,1,0), training[,ncol(training)],
                            positive = "1")
      if(first == T){
        best_pred <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                   round(cm$table[2,2],2), gam, nu, round(cm$byClass[4],2))
        first <- F
      }
      else{
        pred2 <- c(round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
                   round(cm$table[2,2],2), gam, nu, round(cm$byClass[4],2))
        best_pred <- best_prediction(best_pred, pred2)
      }
    }
  }
  # cat(file_name, best_pred[6], best_pred[5], best_pred[1], best_pred[2], best_pred[3], best_pred[4],
  #     best_pred[7], "\n",file = "results/results_svdd.txt", append = T, sep = ",")
  best_pred
}

datasets_names <- c("blood_trans", "breast", "ecoli", "fertility", "haberman", "liver", "ionosphere",
              "mammo", "parkinson", "biodegrad", "seeds")# skin
cat("file, gam, nu, TN,FN,FP,TP,Kappa,\n", file = "results/results_svdd.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  svdd_classification(datasets[[i]], file_name = datasets_names[i])
  
}