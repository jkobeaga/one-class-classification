
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
  df <- read.csv("./uci_datasets/blood_transfusion/transfusion.txt", sep = ",", header = FALSE)
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
  df <- read.csv("./uci_datasets/indian_liver/Indian Liver Patient Dataset (ILPD).csv", sep = ",", header = F)
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
  df <- read.csv("./uci_datasets/mammographic_masses/mammographic_masses.csv", sep = ",", header = F)
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
  df  <- read.csv("./uci_datasets/biodegradation/biodeg.csv", sep = ";", header = F)
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

svdd_df <- function(df){
    df <- lapply(df, function(x) type.convert(as.character(x)))
  df
}


