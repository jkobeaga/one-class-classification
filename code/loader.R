
# Function to load all the necesary libraries 
load_libraries <- function(){
  set.seed(2017)
  library(DMwR)
  library(doMC)
  registerDoMC(4)
  library(dplyr)
  library(e1071)
  library(caret)
  library(kernlab)
  library(h2o)
  
  library(dummies)
}

# Function to create a datasets with dummy variables.
to_dummies <- function(df){
  df <- dummy.data.frame(df, names = colnames(df[,-dim(df)])[sapply(df[,c(1:(dim(df)[2]-1))],is.factor)])
  df
}

# Functions to load datasets and prepare to use them
load_blood <- function(){
  df <- read.csv("./uci_datasets/blood_trans/transfusion.txt", sep = ",", header = FALSE)
  df[,5] <- as.factor(df[,5])
  df <- to_dummies(df)
  df
}

load_breast <- function(){
  df <- read.csv("./uci_datasets/breast/BreastTissue.csv",sep = ";", dec = ",")
  df <- df[,-1]
  df <- df[,c(2:dim(df)[2],1)]
  df$Class <- as.factor(ifelse(df$Class == "fad" | df$Class == "mas" | df$Class == "gla", 0, 1))
  df <- to_dummies(df)
  df
  }

load_ecoli <- function(){
  df <- read.csv("./uci_datasets/ecoli/ecoli.csv", sep = ";", header = F)
  df <- df[,-1]
  df$V9 <- as.factor(ifelse(df$V9 == "cp", 0, 1))
  df <- to_dummies(df)
  df
}

load_fertility <- function(){
  df <- read.csv("./uci_datasets/fertility/fertility_Diagnosis.txt", sep = ",", header = F)
  df$V1 <- as.factor(ifelse(df$V1 == -1, "Winter",
                            ifelse(df$V1 == -0.33, "Spring",
                                   ifelse(df$V1 == 0.33, "Summer", "Fall"))))
  df$V6 <- as.factor(ifelse(df$V6 == -1, "L3", ifelse(df$V6 == 1, "M3", "No")))
  df$V7 <- as.factor(ifelse(df$V7 == 0.2, 1,
                            ifelse(df$V7 == 0.4, 2,
                                   ifelse(df$V7 == 0.6, 3,
                                          ifelse(df$V7 == 0.8,4,5)))))
  df$V8 <- as.factor(ifelse(df$V8 == -1, 0,ifelse(df$V8 == 0, 1,2)))
  df$V10 <- as.factor(ifelse(df$V10 == "N",0,1))
  df <- to_dummies(df)
  df
}

load_haberman <- function(){
  df <- read.csv("./uci_datasets/haberman/haberman.csv", sep = ",", header = F)
  df$V4 <- as.factor(df$V4-1)
  df <- to_dummies(df)
  df
}

load_liver <- function(){
  df <- read.csv("./uci_datasets/liver/Indian Liver Patient Dataset (ILPD).csv", sep = ",", header = F)
  df <- df[-which(is.na(df$V10)),]
  df$V11 <- as.factor(ifelse(df$V11 == 1,0,1))
  df <- to_dummies(df)
  df
}

load_ionosphere <- function(){
  df <- read.csv("./uci_datasets/ionosphere/ionosphere.csv", sep = ",", header = F)
  df$V35 <- as.factor(ifelse(df$V35 == "b", "1", "0"))
  df <- to_dummies(df)
  df
}

load_mammo <- function(){
  df <- read.csv("./uci_datasets/mammo/mammographic_masses.csv", sep = ",", header = F)
  df <- df[,-1]
  df$V2 <- as.numeric(df$V2)
  df <- df[-which(df$V3=="?"),]
  df <- df[-which(df$V4=="?"),]
  df <- df[-which(df$V5=="?"),]
  df$V6 <- as.factor(df$V6)
  df <- to_dummies(df)
  df
}

load_parkinson <- function(){
  df <- read.csv("./uci_datasets/parkinson/parkinsons.csv", sep = ",")
  df <- df[,-1]
  m_status <- match("status", colnames(df))
  df <- df[, c(1:(m_status-1),(m_status+1):dim(df)[2],m_status)]
  # Cambiamos 0<-->1 para que la clase normal sea 0.
  df$status <- as.factor(ifelse(df$status == 1, 0, 1))
  df <- to_dummies(df)
  df
}

load_biodegrad <- function(){
  df  <- read.csv("./uci_datasets/biodegrad/biodeg.csv", sep = ";", header = F)
  df$V42 <- as.factor(ifelse(df$V42 == "NRB", 0, 1))
  df <- to_dummies(df)
  df
}

load_seeds <- function(){
  df <- read.csv("./uci_datasets/seeds/seeds_dataset.txt", sep = "", header = F)
  df$V8 <- as.factor(ifelse(df$V8 == 3, 1, 0))
  df <- to_dummies(df)
  df
}

load_skin <- function(){
  df <- read.csv("./uci_datasets/skin_segmentation/Skin_NonSkin.txt", sep = "", header = F)
  df$V4 <- as.factor(ifelse(df$V4 == 2, 0, 1))
  df <- to_dummies(df)
  df
}

# load_yeast <- function(){
#   df <- read.csv("./uci_datasets/yeast/yeast.csv", sep = "", header = F)
#   df <- df[,-1]
#   df
# }

# Loading libraries
load_libraries()

# Loading all the datasets
blood_trans <- load_blood()
breast <- load_breast()
ecoli <- load_ecoli()
fertility <- load_fertility()
haberman <- load_haberman()
liver <- load_liver()
ionosphere <- load_ionosphere()
mammo <- load_mammo()
parkinson <- load_parkinson()
biodegrad  <- load_biodegrad()
seeds <- load_seeds()
skin <- load_skin()
# yeast <- load_yeast()

# List of all datasets we need
datasets <- list(blood_trans, breast, ecoli, fertility, haberman, liver, ionosphere, mammo,
                 parkinson, biodegrad, seeds)# skin

## Function to create an unbalanced dataset, (1-prop)- prop.
data_split <- function(df, prop = 0.05){
  n_pos <- round(length(which(df[,dim(df)[2]]==0))*prop)
  pos_obs <- sample(which(df[,dim(df)[2]]==1),n_pos)
  df_t <- df[c(pos_obs,which(df[,dim(df)[2]]==0)),]
  # df_pos <- df[-c(pos_obs,which(df[,dim(df)[2]]==0)),]
  df_t
}

# Prepare df for SVDD
svdd_df <- function(df){
    df <- lapply(df, function(x) type.convert(as.character(x)))
  df
}

# Create and write training and testing datasets
create_train_test <- function(df, folder, prop =  0.05){
  index <- createDataPartition(df[,dim(df)[2]], list = FALSE, p = 0.7)
  training <- df[index,]
  testing <- df[-index,]
  training <- data_split(training, prop = prop)
  location <- paste("./uci_datasets/", folder, sep = "")
  # Write files for almost all models
  write.table(training, file = paste(location, "/training.txt", sep = ""), sep = ",", row.names = FALSE)
  write.table(testing, file = paste(location, "/testing.txt", sep = ""), sep = ",", row.names = FALSE)
  # Write files for FRaC
  write.table(training[, -ncol(training)], file = paste(location, "/FRaC/training_frac", sep = ""),
            sep = "\t", col.names = FALSE, row.names = FALSE)
  write.table(testing[, -ncol(testing)], file = paste(location, "/FRaC/testing_frac", sep = ""),
            sep = "\t", col.names = FALSE, row.names = FALSE)
  write.table(training[, ncol(training)], file = paste(location, "/FRaC/training_labels", sep = ""),
            sep = "\t", col.names = FALSE, row.names = FALSE)
  write.table(testing[, ncol(testing)], file = paste(location, "/FRaC/testing_labels", sep = ""),
            sep = "\t", col.names = FALSE, row.names = FALSE)

}

datasets_names <- c("blood_trans", "breast", "ecoli", "fertility", "haberman", "liver",
                    "ionosphere", "mammo", "parkinson", "biodegrad", "seeds")# skin

for(i in 1:length(datasets)){
  cat("iiiiiiiiiiiiiiiiiiii", i, "\n")
  create_train_test(datasets[[i]], folder = datasets_names[i])
  
}


frac_metadata <- function(datas, names){
  for(j in 1:length(datas)){
    df <- datas[[j]]
    values <- unique(df[,1])
    if(length(values)<8){
      input <- paste(1, "\tnominal\t",paste(sort(values),collapse = ","), sep = "")
    }
    else{
      input <- paste(1, "\tcontinuous\t", round(min(df[,1]),2), ",", round(max(df[,1]),2),
                     sep = "")
    }
    write.table(input, file = paste("./uci_datasets/",names[j],"/metadata", sep = ""), quote =  F,
                                  row.names = F, col.names = F)
    for(i in 2:(ncol(df)-1)){
      values <- unique(df[,i])
      if(length(values)<8){
        input <- paste(i, "\tnominal\t", paste(sort(values),collapse = ","), sep = "")
      }
      else{
        input <- paste(i, "\tcontinuous\t", round(min(df[,i]),2), ",", round(max(df[,i]),2),
                       sep = "")
      }
      write.table(input, file = paste("./uci_datasets/",names[j],"/FRaC/metadata", sep = ""), quote =  F,
                row.names = F, col.names = F, append = T)
    }
  }
}



