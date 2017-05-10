svdd_classification <- function(df,prop=0.05, file_name){
  # Dividimos en train y test (70-30)
  index <- createDataPartition(df[,dim(df)[2]], list = FALSE, p = 0.7)
  training <- df[index,]
  testing <- df[-index,]
  training <- data_split(training, prop = prop)
  
  ## TRAINING
  nu_list <- seq(0.01,0.2,0.02)
  gamma_list <- seq(0.1,0.6,0.05)
  
  for(gam in gamma_list){
    for(nu in nu_list){
      # cat(dim(training)[2], summary(training[,10]))
      model <- svm(as.factor(training[,ncol(training)])~., data = training, type = "one-classification",
                   kernel = "radial", nu = nu, gamma= gam, cross=10)
      # cat(predict(model,testing))
      cm <- confusionMatrix(ifelse(predict(model,testing)==T,1,0), testing[,ncol(testing)])
      cat(file_name, nu, round(cm$table[1,1],2),round(cm$table[1,2],2),round(cm$table[2,1],2),
          round(cm$table[2,2],2), round(cm$byClass[4],2), round(cm$byClass[6],2),
          round(cm$overall[2],2),"\n",  file = "results/results_svdd.txt", append = T, sep = ",")
    }
  }
  cm
}

datasets_names <- c("blood_trans", "breast", "ecoli", "fertility", "haberman", "liver", "ionosphere",
              "mammo", "parkinson", "biodegrad", "seeds")# skin
cat("file,nu,TN,FP,FN,FP,Recall,Neg_pred,Kappa,\n", file = "results/results_svdd.txt", append = F)
for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  svdd_classification(datasets[[i]], file_name = datasets_names[i])
  
}