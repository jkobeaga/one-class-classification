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
    # Remove the variables from the metadata file
    # For the datasets Biodegrad with the variables 12,14,30 doesn't work
    metadata <- read.csv("./uci_datasets/biodegrad/FRaC/metadata", sep = "\t", header = F)
    metadata <- metadata[-c(12,14,30),]
    write.table(metadata,file = "./uci_datasets/biodegrad/FRaC/metadata", sep = "\t", row.names = F,
                col.names = F, quote = F)
    # For the datasets Ionosphere with the variables 25 doesn't work
    metadata <- read.csv("./uci_datasets/ionosphere/FRaC/metadata", sep = "\t", header = F)
    metadata <- metadata[-25,]
    write.table(metadata,file = "./uci_datasets/ionosphere/FRaC/metadata", sep = "\t", row.names = F,
                col.names = F, quote = F)
  }
  
  
  # Assign labels to results
  
  assign_label <- function(datasets, names, overwrite = F){
    if(overwrite == T){
      cat("file,TN,FN,FP,TP,Kappa,\n", file = "./results/recall/testing/results_frac.txt")
    }
    for(i in 1:length(datasets)){
      input_frac <- paste("./uci_datasets/", names[i], "/FRaC/", names[i],"_predictions", sep = "")
      frac_pred <- read.csv(input_frac, header = F)
      input_test <-  paste("./uci_datasets/", names[i],"/testing.txt" , sep = "")
      testing <- read.table(input_test, header = T, sep = ",")
      # Assigning label
      frac_pred$label <- ifelse(frac_pred[,1]>0,1,0)
      
      # Confusion matrix
      cm <- confusionMatrix(frac_pred$label, testing[,ncol(testing)], positive = "1")
      cat(names[i], round(cm$table[1,1],2), round(cm$table[1,2],2), round(cm$table[2,1],2),
          round(cm$table[2,2],2),round(cm$byClass[4],2) , "\n",
          file = "./results/recall/testing/results_frac.txt", append = T, sep = ",")
    }
  }
  
# Create new train-test datasets
for(j in 1:length(datasets)){
  # cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  create_train_test(datasets[[j]], folder = datasets_names[j])
}

# update metadata
frac_metadata(datasets, datasets_names)
# Calculate the measures:
assign_label(datasets, datasets_names, overwrite = F)
