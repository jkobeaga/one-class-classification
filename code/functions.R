# To scale data into [0,1]
scale_df <- function(df){
  num_col <- c()
  for(i in 1:dim(df)[2]){
    if(length(unique(df[,i])) > 9){
      num_col <- c(num_col,i)
    }
  }
  if(length(num_col==1)){
    df[,num_col] <-   predict(preProcess(as.data.frame(df[,num_col]), method = c("range")),as.data.frame(df[,num_col]))
  }
  if(length(num_col)>1){
    df[,num_col] <- predict(preProcess(df[,num_col], method = c("range")),df[,num_col])
  }
  df
}

# Create metadata for FRaC
frac_metadata <- function(datas, names){
  for(j in 1:length(datas)){
    df <- datas[[j]]
    values <- unique(df[,1])
    if(length(values)<8 | is.integer(df[,1])){
      if(length(values)>1){
        input <- paste(1, "\tnominal\t",paste(sort(values),collapse = ","), sep = "")
        write <- TRUE
      }
      
    }
    else{
      input <- paste(1, "\tcontinuous\t", round(min(df[,1]),2), ",", round(max(df[,1]),2),
                     sep = "")
      write <- TRUE
    }
    if(write == TRUE){
      write.table(input, file = paste("./uci_datasets/",names[j],"/FRaC/metadata", sep = ""),
                  quote =  F, row.names = F, col.names = F)  
      write <- FALSE
    }
    
    for(i in 2:(ncol(df)-1)){
      values <- unique(df[,i])
      # cat(names[j]," Variable: ", i," Valores: ", values, "\n")
      if(length(values)<8 | is.integer(df[,i])){
        if(length(values)>1){
          write <- TRUE
          input <- paste(i, "\tnominal\t", paste(sort(values),collapse = ","), sep = "")
        }
      }
      else{
        write <- TRUE
        input <- paste(i, "\tcontinuous\t", round(min(df[,i]),2), ",", round(max(df[,i]),2),
                       sep = "")
      }
      if(write == TRUE){
        write.table(input, file = paste("./uci_datasets/",names[j],"/FRaC/metadata", sep = ""),
                    quote =  F, row.names = F, col.names = F, append = T)
        write <- FALSE
      }
      
    }
  }
}


# Assign cluster to each observation
cluster_assign <- function(df,name){
  results <- read.table(file = "./results/clusters.txt", sep = ",")
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


# Write results for each technique in a file
write_result <- function(datasets, names, baseline = F, svdd = F, one_class = F,  smote = F,
                         weights = F, logistic = F, autoencoder = F, all = F, cluster = F){
  if(baseline == T){
    C=c(seq(0.01,0.2,0.05),seq(0.3,1,0.1))
    sigma=seq(0.1,1,0.3)
    cat("file,cost,sigma,TN,FN,FP,TP,Kappa,\n",
        file = "results/results_baseline.txt", append = F)
    for(i in 1:length(datasets)){
      bl_model <- baseline_classification(datasets[[i]], file_name = datasets_names[i],
                                          C = C, sigma =sigma) 
      cat(datasets_names[i], bl_model[1], bl_model[2], bl_model[3], bl_model[4], bl_model[5],
          bl_model[6], bl_model[7], "\n",
          file = "results/results_baseline.txt", append = T, sep = ",")
      # cat(bl_model[8], bl_model[9],"\n")
    } 
  }
  if(svdd == T){
    if(cluster == T){
      cat("file,TN,FP,FN,TP,Kappa,\n", file = "results/results_cluster_svdd.txt",
          append = F)
      # nu_list <- seq(0.01,0.2,0.02)
      gamma_list <- seq(0.1,0.6,0.05)
      C <- c(seq(0.01,0.2,0.02))
      for(i in 1:length(datasets)){
        clust_svdd_model <- cluster_svdd(datasets[[i]], file_name = datasets_names[i], C,
                                         nu_list = prop, gamma_list)
        cat(datasets_names[i], clust_svdd_model[1], clust_svdd_model[2], clust_svdd_model[3],
          clust_svdd_model[4], clust_svdd_model[5], "\n",file = "results/results_cluster_svdd.txt",
          append = T, sep = ",")}

    }
    else{
      C <- c(seq(0.01,0.2,0.02))
      gamma_list <- seq(0.1,0.6,0.06)
      cat("file,C,gam,nu,TN,FN,FP,TP,Kappa,\n", file = "results/results_svdd.txt", append = F)
      for(i in 1:length(datasets)){
        svdd_model <- svdd_classification(datasets[[i]], file_name = datasets_names[i], C, gamma_list)
        cat(datasets_names[i], svdd_model[5], svdd_model[6], svdd_model[7], svdd_model[1], svdd_model[2],
            svdd_model[3], svdd_model[4], svdd_model[8], "\n",file = "results/results_svdd.txt",
            append = T, sep = ",")
      }
    }
  }
  if(one_class == T){
    if(cluster == T){
      cat("file,TN,FP,FN,TP,Kappa,\n", file = "results/results_cluster_Scholkopf.txt",
          append = F)
      # nu_list <- seq(0.01,0.2,0.02)
      gamma_list <- seq(0.1,5,0.3)
      C <- c(seq(0.01,3,0.2))
      for(i in 1:length(datasets)){
        clust_svdd_model <- cluster_nu(datasets[[i]], file_name = datasets_names[i], C = C,
                                         nu_list = prop, gamma_list = gamma_list)
        cat(datasets_names[i], clust_svdd_model[1], clust_svdd_model[2], clust_svdd_model[3],
            clust_svdd_model[4], clust_svdd_model[5], "\n",file = "results/results_cluster_Scholkopf.txt",
            append = T, sep = ",")}
      
    }
    else{
      C <- c(seq(0.01,0.2,0.02))
      gamma_list <- seq(0.1,0.6,0.06)
      cat("file,cost,gamma,nu,TN,FN,FP,TP,Kappa,\n", file = "results/results_Scholkopf.txt",
          append = F)
      for(i in 1:length(datasets)){
        oc_model <- svdd_classification(datasets[[i]], file_name = datasets_names[i],C, gamma_list)
        cat(datasets_names[i], oc_model[5], oc_model[6], oc_model[7],oc_model[1], oc_model[2],
            oc_model[3], oc_model[4],oc_model[8], "\n",file = "results/results_Scholkopf.txt",
            append = T, sep = ",")
      }
    }
  }
  if(smote == T){
    sigma_list <- seq(0.1, 1, 0.1)
    cost_list <- seq(0.5,3,0.5)
    cat("file,cost,sigma,nSV_0,nSV_1,TN,FN,FP,TP,Kappa,\n", file = "results/results_SMOTE.txt", append = F)
    for(i in 1:length(datasets)){
      smote_model <- smote_classification(datasets[[i]], file_name = datasets_names[i], cost_list,
                                          sigma_list)
      cat(datasets_names[i], smote_model[5], smote_model[6], smote_model[8], smote_model[9],
          smote_model[1], smote_model[2], smote_model[3], smote_model[4], smote_model[7],
          "\n",file = "results/results_SMOTE.txt", append = T, sep = ",")
    }
  }
  if(weights == T){
    C=seq(0.5,3,0.5)
    sigma=seq(0.1,1,0.3)
    weight_normal <- c(0.5,1)
    weight_anomaly <- seq(2,20,2)
    cat("file,P0,P1,cost,sigma,nSV_0,nSV_1,TN,FN,FP,TP,Kappa,\n", file = "results/results_weights.txt",
        append = F)
    for(i in 1:length(datasets)){
      weight_model <- weight_classification(datasets[[i]], file_name = datasets_names[i], C, sigma,
                                            weight_normal, weight_anomaly)
      cat(datasets_names[i], weight_model[8], weight_model[9], weight_model[6], weight_model[5],
          weight_model[10], weight_model[11], weight_model[1], weight_model[2], weight_model[3],
          weight_model[4], weight_model[7], "\n",file = "results/results_weights.txt", append = T,
          sep = ",")
      
    }
    
  }
  if(logistic == T){
    cat("file,Corrected,TN,FN,FP,TP,Recall,Neg_pred,Kappa,\n", file = "results/results_LR.txt",
        append = F)
    for(i in 1:length(datasets)){
      lr_model <- LR_classification(datasets[[i]], file_name = datasets_names[i])
      cat(datasets_names[i],"F", lr_model[1], lr_model[2], lr_model[3], lr_model[4], lr_model[5],
          "\n",  file = "results/results_LR.txt", append = T, sep = ",")
      cat(datasets_names[i],"T", lr_model[6], lr_model[7], lr_model[8], lr_model[9], lr_model[10],
          "\n",  file = "results/results_LR.txt", append = T, sep = ",")
      
    }
  }

  
}

write_result(datasets, datasets_names, one_class = T, cluster = T)

