# To scale data into [0,1]
scale_df <- function(df){
  num_col <- c()
  for(i in 1:dim(df)[2]){
    if(length(unique(df[,i])) > 9){
      num_col <- c(num_col,i)
    }
  }
  if(length(num_col==1)){
    df[,num_col] <-   predict(preProcess(as.data.frame(df[,num_col]), method = c("range")),
                              as.data.frame(df[,num_col]))
  }
  if(length(num_col)>1){
    df[,num_col] <- predict(preProcess(df[,num_col], method = c("range")),df[,num_col])
  }
  df
}


# Assign cluster to each observation
cluster_assign <- function(df,name){
  results <- read.table(file = "./results_training/clusters.txt", sep = ",")
  num_clusters <- as.numeric(results[which(results[,1] == name),2])+1
  km <- kmeans(df[,-ncol(df)], centers = num_clusters, iter.max = 20, nstart = 25)
  df$cluster <- as.factor(km$cluster)
  df <- df[order(df$cluster),]
  # df$id <- seq(1,dim(df)[1])
  df
}

# Predicting the cluster for each observation from test
predict_cluster <- function(train, test){
  # The training set has one more column than the testing set. This column refers to the cluster.
  cat("\nDim train:")
  print(dim(train[, -c(ncol(train),ncol(train)-1)]))
  cat("\nDim Test")
  print(dim(test[,-ncol(test)]))
  test$cluster <- knn(train[, -c(ncol(train),ncol(train)-1)], test[,-ncol(test)],
                      cl = train[,ncol(train)], k = 10)
  test
}


# Get the best prediction between two, by default considered metric is recall
best_prediction <- function(pred1, pred2, recall = T, F1 = F, accuracy = F){
  pred1 <- as.numeric(pred1)
  pred2 <- as.numeric(pred2)
  if(recall == T){
    recall_1 <- pred1[4]/(pred1[2] + pred1[4])
    recall_2 <- pred2[4]/(pred2[2] + pred2[4])
    if(recall_1>=recall_2) best_pred <- pred1
    else best_pred <- pred2
  }
  else if(F1 == T){
    precision_1 <- pred1[4]/(pred1[4]+pred1[3])
    recall_1 <- pred1[4]/(pred1[2] + pred1[4])
    precision_2 <- pred2[4]/(pred2[4]+pred2[3])
    recall_2 <- pred2[4]/(pred2[2] + pred2[4])
    
    F1_1 <- 2*precision_1*recall_1/(precision_1+recall_1)
    F1_2 <- 2*precision_2*recall_2/(precision_2+recall_2)
    if(F1_1>=F1_2) best_pred <- pred1
    else best_pred <- pred2
  }
  else{
    accuracy_1 <- (pred1[1] + pred1[4])/(sum(pred1))
    accuracy_2 <- (pred2[1] + pred2[4])/(sum(pred2))
    if(accuracy_1 >= accuracy_2) best_pred <- pred1
    else best_pred <- pred2
  }
  best_pred
}

# Calculate metrics and add to results
add_metric <- function(df){
  tn <- df[, match("TN", colnames(df))]
  fn <- df[, match("FN", colnames(df))]
  fp <- df[, match("FP", colnames(df))]
  tp <- df[, match("TP", colnames(df))]
  df$recall <- tp/(tp+fn)
  df$precision <- tp/(tp+fp)
  df$F1 <- 2*df$recall*df$precision/(df$recall+df$precision)
  df
  
}


# Write results for each technique in a file. When clustering is required it also test the model.
# prop_majority is the proportion of the majority class in SMOTE
write_result_train <- function(datasets, names, baseline = F, svdd = F, one_class = F,  smote = F,
                         weights = F, logistic = F, autoencoder = F, all = F, cluster = F,
                         prop_majority = 50, first){
  if(baseline == T){
    C=c(seq(0.01,0.2,0.05),seq(0.3,1,0.1))
    sigma=seq(0.1,1,0.3)
    if(first == T)cat("file,cost,sigma,TN,FN,FP,TP,Kappa,\n",
                     file = "results_training/results_baseline.txt", append = F)
    for(i in 1:length(datasets)){
      train_test <- baseline_classification(datasets[[i]], file_name = datasets_names[i],
                                          C = C, sigma =sigma) 
      bl_model <- train_test[[1]]
      cat(datasets_names[i], bl_model[1], bl_model[2], bl_model[3], bl_model[4], bl_model[5],
          bl_model[6], bl_model[7], "\n",
          file = "results_training//results_baseline.txt", append = T, sep = ",")
      # cat(bl_model[8], bl_model[9],"\n")
    } 
  }
  if(svdd == T){
    if(cluster == T){
      if(first == T){
        cat("file,TN,FP,FN,TP,Kappa,\n", file = "results_training/results_cluster_svdd.txt",append = F)
        cat("file,TN,FP,FN,TP,Kappa,\n", file = "results_testing/results_cluster_svdd.txt",append = F)
        }
      # nu_list <- seq(0.01,0.2,0.02)
      gamma_list <- seq(0.1,0.6,0.05)
      C <- c(seq(0.01,0.2,0.02))
      for(i in 1:length(datasets)){
        train_test <- cluster_svdd(datasets[[i]], file_name = datasets_names[i], C = C,
                                         nu_list = prop, gamma_list = gamma_list)
        clust_svdd_model <- train_test[[1]]
        cat(datasets_names[i], clust_svdd_model[1], clust_svdd_model[2], clust_svdd_model[3],
          clust_svdd_model[4], clust_svdd_model[5], "\n",
          file = "results_training/results_cluster_svdd.txt",
          append = T, sep = ",")
        clust_svdd_model_test <- train_test[[2]]
        cat(datasets_names[i], clust_svdd_model_test[1], clust_svdd_model_test[2],
            clust_svdd_model_test[3], clust_svdd_model_test[4], clust_svdd_model_test[5], "\n",
            file = "results_testing/results_cluster_svdd.txt",
          append = T, sep = ",")
        }

    }
    else{
      C <- c(seq(0.01,0.2,0.02))
      gamma_list <- seq(0.1,0.6,0.06)
      if(first == T)cat("file,cost,sigma,nu,TN,FN,FP,TP,Kappa,\n",
                        file = "results_training//results_svdd.txt",
                        append = F)
      for(i in 1:length(datasets)){
        train_test <- svdd_classification(datasets[[i]], file_name = datasets_names[i], C = C,
                                          gamma_list = gamma_list, cluster = F)
        svdd_model <- train_test[[1]]
        cat(datasets_names[i], svdd_model[5], svdd_model[6], svdd_model[7], svdd_model[1],
            svdd_model[2], svdd_model[3], svdd_model[4], svdd_model[8], "\n",
            file = "results_training//results_svdd.txt",
            append = T, sep = ",")
      }
    }
  }
  if(one_class == T){
    if(cluster == T){
      if(first == T){
        cat("file,TN,FP,FN,TP,Kappa,\n", file = "results_training/results_cluster_Scholkopf.txt",
            append = F)
        cat("file,TN,FP,FN,TP,Kappa,\n", file = "results_testing/results_cluster_Scholkopf.txt",
            append = F)
      }
      # nu_list <- seq(0.01,0.2,0.02)
      gamma_list <- seq(0.1,5,0.3)
      C <- c(seq(0.01,3,0.2))
      for(i in 1:length(datasets)){
        train_test <- cluster_nu(datasets[[i]], file_name = datasets_names[i], C = C,
                                         nu_list = prop, gamma_list = gamma_list)
        clust_svdd_model <- train_test[[1]]
        cat(datasets_names[i], clust_svdd_model[1], clust_svdd_model[2], clust_svdd_model[3],
            clust_svdd_model[4], clust_svdd_model[5], "\n",file = "results_training/results_cluster_Scholkopf.txt",
            append = T, sep = ",")
        clust_svdd_model_test <- train_test[[2]]
        cat("\nNUMBER OF ANOMALIES: ", sum(clust_svdd_model_test[2],clust_svdd_model_test[4]),"\n")
        cat(datasets_names[i], clust_svdd_model_test[1], clust_svdd_model_test[2],
            clust_svdd_model_test[3], clust_svdd_model_test[4], clust_svdd_model_test[5], "\n",
            file = "results_testing/results_cluster_Scholkopf.txt",
            append = T, sep = ",")
        }
    }
    else{
      C <- c(seq(0.01,0.2,0.02))
      gamma_list <- seq(0.1,0.6,0.06)
      if(first == T)cat("file,cost,gamma,nu,TN,FN,FP,TP,Kappa,\n",
                        file = "results_training/results_Scholkopf.txt", append = F)
      for(i in 1:length(datasets)){
        train_test <- nu_classification(datasets[[i]], file_name = datasets_names[i], C = C,
                                        gamma_list = gamma_list, cluster = F)
        oc_model <- train_test[[1]]
        cat(datasets_names[i], oc_model[5], oc_model[6], oc_model[7],oc_model[1], oc_model[2],
            oc_model[3], oc_model[4],oc_model[8], "\n",file = "results_training/results_Scholkopf.txt",
            append = T, sep = ",")
      }
    }
  }
  if(smote == T){
    if(cluster == T){
      # gamma_list <- seq(0.1,0.6,0.05)
      # C <- c(seq(0.01,0.2,0.02))
      gamma_list <- seq(0.6,1,0.05)
      C <- c(seq(0.2,0.6,0.02))
      location <- paste( "results_training/results_cluster_SMOTE_", prop_majority, ".txt", sep="")
      location_test <- paste( "results_testing/results_cluster_SMOTE_", prop_majority, ".txt", sep="")
      if(first == T){
        cat("file,TN,FN,FP,TP,Kappa,\n",file = location, append = F)
        cat("file,TN,FN,FP,TP,Kappa,\n",file = location_test, append = F)
      }
      for(i in 1:length(datasets)){
        train_test <- cluster_smote(datasets[[i]], file_name = datasets_names[i], C = C,
                                             gamma_list = gamma_list, prop_majority = prop_majority)
        cluster_smote_model <- train_test[[1]]
        cat(datasets_names[i],cluster_smote_model[1], cluster_smote_model[2], cluster_smote_model[3],
            cluster_smote_model[4], cluster_smote_model[5],
            "\n",file = location, append = T, sep = ",")
        cluster_smote_model_test <- train_test[[2]]
        cat(datasets_names[i],cluster_smote_model_test[1], cluster_smote_model_test[2],
            cluster_smote_model_test[3], cluster_smote_model_test[4], cluster_smote_model_test[5],
            "\n",file = location_test, append = T, sep = ",")
      }
    }
    
    else{
      sigma_list <- seq(0.1, 1, 0.1)
      cost_list <- seq(0.5,3,0.5)
      location <- paste( "results_training/results_SMOTE_", prop_majority, ".txt", sep="")
      if(first == T)cat("file,cost,sigma,nSV_0,nSV_1,TN,FN,FP,TP,Kappa,\n",
                        file = location, append = F)
      for(i in 1:length(datasets)){
        train_test <- smote_classification(datasets[[i]], file_name = datasets_names[i],
                                            cost_list = cost_list, gamma_list = sigma_list,
                                            prop_majority = prop_majority, cluster = F)
        smote_model <- train_test[[1]]
        cat(datasets_names[i], smote_model[5], smote_model[6], smote_model[8], smote_model[9],
            smote_model[1], smote_model[2], smote_model[3], smote_model[4], smote_model[7],
            "\n",file = location, append = T, sep = ",")
      }
    }
  }
  if(weights == T){
    C=seq(0.5,3,0.5)
    gamma_list=seq(0.1,1,0.3)
    weight_normal <- c(0.5,1)
    weight_anomaly <- seq(2,20,2)
    if(cluster == T){
      if(first == T){
        cat("file,TN,FN,FP,TP,Kappa,\n", file = "results_training/results_cluster_weights.txt",
            append = F)
        cat("file,TN,FN,FP,TP,Kappa,\n", file = "results_testing/results_cluster_weights.txt",
            append = F)
      }
      for(i in 1:length(datasets)){
        train_test <- cluster_weight(datasets[[i]], file_name = datasets_names[i], C = C,
                                               gamma = gamma_list, weight_normal = weight_normal,
                                               weight_anomaly = weight_anomaly)
        # cluster_weight_model <- cluster_weight(datasets[[i]], file_name = datasets_names[i], C = C,
        #                                      gamma_list = gamma_list)
        cluster_weight_model <- train_test[[1]]
        cat(datasets_names[i],cluster_weight_model[1], cluster_weight_model[2],
            cluster_weight_model[3], cluster_weight_model[4], cluster_weight_model[5],
            "\n",file = "results_training/results_cluster_weights.txt", append = T, sep = ",")
        cluster_weight_model_test <- train_test[[2]]
        cat(datasets_names[i],cluster_weight_model_test[1], cluster_weight_model_test[2],
            cluster_weight_model_test[3], cluster_weight_model_test[4], cluster_weight_model_test[5],
            "\n",file = "results_testing/results_cluster_weights.txt", append = T, sep = ",")
      }
    }
    else{
      if(first == T)cat("file,P0,P1,cost,sigma,nSV_0,nSV_1,TN,FN,FP,TP,Kappa,\n",
                        file = "results_training/results_weights.txt", append = F)
      for(i in 1:length(datasets)){
        train_test <- weight_classification(datasets[[i]], file_name = datasets_names[i], C = C,
                                              gamma = gamma_list, weight_normal = weight_normal,
                                              weight_anomaly = weight_anomaly, cluster = F)
        weight_model <- train_test[[1]]
        cat(datasets_names[i], weight_model[8], weight_model[9], weight_model[6], weight_model[5],
            weight_model[10], weight_model[11], weight_model[1], weight_model[2], weight_model[3],
            weight_model[4], weight_model[7], "\n",file = "results_training/results_weights.txt",
            append = T, sep = ",")
      }
    }
  }
  if(logistic == T){
    if(first == T){
      cat("file,Corrected,TN,FN,FP,TP,Kappa,\n", file = "results_training/results_LR.txt",
          append = F)
      cat("file,Corrected,TN,FN,FP,TP,Kappa,\n", file = "results_testing/results_LR.txt",
          append = F)
    }
    for(i in 1:length(datasets)){
      lr_model <- LR_classification(datasets[[i]], file_name = datasets_names[i])
      # Write results of training
      cat(datasets_names[i],"F", lr_model[1], lr_model[2], lr_model[3], lr_model[4], lr_model[5],
          "\n",  file = "results_training/results_LR.txt", append = T, sep = ",")
      cat(datasets_names[i],"T", lr_model[11], lr_model[12], lr_model[13], lr_model[14], lr_model[15],
          "\n",  file = "results_training/results_LR.txt", append = T, sep = ",")
      # Write results of testing
      cat(datasets_names[i],"F", lr_model[6], lr_model[7], lr_model[8], lr_model[9], lr_model[10],
          "\n",  file = "results_testing/results_LR.txt", append = T, sep = ",")
      cat(datasets_names[i],"T", lr_model[16], lr_model[17], lr_model[18], lr_model[19], lr_model[20],
          "\n",  file = "results_testing/results_LR.txt", append = T, sep = ",")
      
    }
  }
}


for(i in 1:10){
  for(j in 1:length(datasets)){
    # cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
    create_train_test(datasets[[j]], folder = datasets_names[j])
  }
  cat("iteration: ",i, "\n")
  fold1 <- ifelse(i == 1, T, F)
  write_result_train(datasets, datasets_names, baseline = T, first = fold1)
  cat("baseline\n")
  write_result_train(datasets, datasets_names, svdd = T, first = fold1)
  cat("svdd\n")
  write_result_train(datasets, datasets_names, one_class = T, first = fold1)
  cat("one-class\n")
  write_result_train(datasets, datasets_names, smote = T, first = fold1, prop_majority = 50)
  cat("smote 70\n")
  write_result_train(datasets, datasets_names, smote = T, first = fold1, prop_majority = 60)
  cat("smote 50\n")
  write_result_train(datasets, datasets_names, smote = T, first = fold1, prop_majority = 65)
  cat("smote 50\n")
  write_result_train(datasets, datasets_names, weights = T, first = fold1)
  cat("weights\n")
  write_result_train(datasets, datasets_names, logistic = T, first = fold1)
  cat("logistic\n")
  write_result_train(datasets, datasets_names, svdd = T, cluster = T, first = fold1)
  cat("cluster svdd\n")
  write_result_train(datasets, datasets_names, one_class = T, cluster = T, first = fold1)
  cat("cluster one-class\n")
  write_result_train(datasets, datasets_names, smote = T, cluster = T, first = fold1,
  prop_majority = 50)
  cat("cluster smote 70\n")
  write_result_train(datasets, datasets_names, smote = T, cluster = T, first = fold1,
                     prop_majority = 60)
  cat("cluster smote 60\n")
  write_result_train(datasets, datasets_names, smote = T, cluster = T, first = fold1,
                     prop_majority = 65)
  cat("cluster smote 50\n")
    write_result_train(datasets, datasets_names, weights = T, cluster = T, first = fold1)
  cat("cluster\n")
}


write_result_test <-  function(datasets, names, baseline = F, svdd = F, one_class = F,  smote = F,
                                weights = F, logistic = F, autoencoder = F, all = F, cluster = F,
                                prop_majority = 50, first){
  if(baseline == T){
    params <- read.csv("results_training/parameters/baseline.txt", sep = ",")
    if(first == T)cat("file,TN,FN,FP,TP,Kappa,\n",
                      file = "results_testing/results_baseline.txt", append = F)
    for(i in 1:length(datasets)){
      cost_list <- params[which(params$file == names[i]),match("cost", colnames(params))]
      sigma_list <- params[which(params$file == names[i]),match("sigma", colnames(params))]
      train_test <- baseline_classification(datasets[[i]], file_name = datasets_names[i],
                                          C = cost_list, sigma =sigma_list, test = T) 
      bl_model <- train_test[[2]]
      cat(datasets_names[i], bl_model[1], bl_model[2], bl_model[3],bl_model[4], bl_model[5], "\n",
          file = "results_testing/results_baseline.txt", append = T, sep = ",")
      # cat(bl_model[8], bl_model[9],"\n")
    } 
  }
  if(svdd == T){
    params <- read.csv("results_training/parameters/svdd.txt", sep = ",")
    if(first == T)cat("file,TN,FN,FP,TP,Kappa,\n", file = "results_testing/results_svdd.txt",
                      append = F)
    for(i in 1:length(datasets)){
      cost_list <- params[which(params$file == names[i]),match("cost", colnames(params))]
      sigma_list <- params[which(params$file == names[i]),match("sigma", colnames(params))]
      train_test <- svdd_classification(datasets[[i]], file_name = datasets_names[i], C = cost_list,
                                        gamma_list = sigma_list, test = T, cluster = F)
      svdd_model <- train_test[[2]]
      cat(datasets_names[i], svdd_model[1], svdd_model[2], svdd_model[3], svdd_model[4],
          svdd_model[5], "\n",file = "results_testing/results_svdd.txt", append = T, sep = ",")
    }
  }
  if(one_class == T){
    params <- read.csv("results_training/parameters/oneclass.txt", sep = ",")
    if(first == T)cat("file,TN,FN,FP,TP,Kappa,\n",
                      file = "results_testing/results_Scholkopf.txt", append = F)
    for(i in 1:length(datasets)){
      cost_list <- params[which(params$file == names[i]),match("cost", colnames(params))]
      sigma_list <- params[which(params$file == names[i]),match("sigma", colnames(params))]
      train_test <- nu_classification(datasets[[i]], file_name = datasets_names[i], C = cost_list,
                                    gamma_list = sigma_list, test = T, cluster = F)
      oc_model <- train_test[[2]]
      cat(datasets_names[i],oc_model[1], oc_model[2], oc_model[3], oc_model[4],oc_model[5],
          "\n",file = "results_testing/results_Scholkopf.txt", append = T, sep = ",")
    }
  }
  if(smote == T){
    location_params <- paste("results_training/parameters/smote_", prop_majority, ".txt", sep = "")
    params <- read.csv(location_params, sep = ",")
    location <- paste( "results_testing/results_SMOTE_", prop_majority, ".txt", sep="")
    if(first == T)cat("file,cost,sigma,nSV_0,nSV_1,TN,FN,FP,TP,Kappa,\n",
                      file = location, append = F)
    for(i in 1:length(datasets)){
      cost_list <- params[which(params$file == names[i]),match("cost", colnames(params))]
      sigma_list <- params[which(params$file == names[i]),match("sigma", colnames(params))]
      train_test <- smote_classification(datasets[[i]], file_name = datasets_names[i],
                                          cost_list = cost_list, gamma_list = sigma_list,
                                          prop_majority = prop_majority, test = T, cluster = F)
      smote_model <- train_test[[2]]
      cat(datasets_names[i], smote_model[1], smote_model[2], smote_model[3],
          smote_model[4], smote_model[5], "\n",file = location, append = T, sep = ",")
    }
  }
  if(weights == T){
    C=seq(0.5,3,0.5)
    gamma_list=seq(0.1,1,0.3)
    weight_normal <- c(0.5,1)
    weight_anomaly <- seq(2,20,2)
    params <- read.csv("results_training/parameters/weights.txt", sep = ",")
    if(first == T)cat("file,TN,FN,FP,TP,Kappa,\n",
                      file = "results_testing/results_weights.txt", append = F)
    for(i in 1:length(datasets)){
      cost_list <- params[which(params$file == names[i]),match("cost", colnames(params))]
      sigma_list <- params[which(params$file == names[i]),match("sigma", colnames(params))]
      weight_normal <- params[which(params$file == names[i]),match("P0", colnames(params))]
      weight_anomaly <- params[which(params$file == names[i]),match("P1", colnames(params))]
      weight_model <- weight_classification(datasets[[i]], file_name = datasets_names[i], C = C,
                                            gamma = gamma_list, weight_normal = weight_normal,
                                            weight_anomaly = weight_anomaly, cluster = F)
      cat(datasets_names[i],weight_model[1], weight_model[2], weight_model[3],
          weight_model[4], weight_model[5], "\n",file = "results_testing/results_weights.txt", append = T,
          sep = ",")
    }
  }
}

write_result_test(datasets, datasets_names, baseline = T, first = fold1)
write_result_test(datasets, datasets_names, svdd = T, first = fold1)
write_result_test(datasets, datasets_names, one_class = T, first = fold1)
write_result_test(datasets, datasets_names, smote = T, first = fold1, prop_majority = 50)
write_result_test(datasets, datasets_names, smote = T, first = fold1, prop_majority = 60)
write_result_test(datasets, datasets_names, smote = T, first = fold1, prop_majority = 65)
write_result_test(datasets, datasets_names, weights = T, first = fold1, prop_majority = 65)
