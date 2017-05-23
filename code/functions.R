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
  num_clusters <- results[which(results[,1] == name),2]
  km <- kmeans(df[,-ncol(df)], centers = num_clusters, iter.max = 20, nstart = 25)
  df$cluster <- km$cluster
  df <- df[order(df$cluster),]
  # df$id <- seq(1,dim(df)[1])
  df
}


