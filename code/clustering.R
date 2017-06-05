# Function to calculate the best number of cluster for many different indexes
cluster_best <- function(df, methods, name){
  # clust_polling <- data.frame(clusters = integer(), votes = integer())
  clust_polling <- rep(0,10)
  for(met in methods){
    cat("\n",met)
    nb <- NbClust(df[,-ncol(df)], distance = "euclidean",min.nc = 2, max.nc = 10, method = "kmeans",
                  index = met )
    cat(nb$Best.nc[1])
    
    clust_polling[nb$Best.nc[1]] <- clust_polling[nb$Best.nc[1]] + 1
  }
  cat("Best number of clusters: ", which.max(clust_polling))
  cat("\nNumber of indices proposed: ", max(clust_polling))
  cat(name, which.max(clust_polling), "\n", file = "./results_training/clusters.txt", append = T, sep = ",")
  
}

# First line of the result file
cat("Dataset", "Clusters", "\n", file = "results/clusters.txt", append = F, sep = ",")

## Blood transfusion
clust_methods <- c("kl", "ch", "hartigan",
                   "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial",
                   "frey", "mcclain","dunn", "hubert", "sdindex", "dindex", "sdbw")
cluster_best(read.csv(file = "uci_datasets/blood_trans/training.txt"), clust_methods, "blood_trans") # 2 clusters

## Biodegrad
clust_methods <- c("kl", "ch", "hartigan",
                   "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial",
                   "frey", "mcclain","dunn", "hubert", "sdindex", "dindex", "sdbw")
cluster_best(read.csv(file = "uci_datasets/biodegrad/training.txt"), clust_methods, "biodegrad") # 2 clusters

## Breast
clust_methods <- c("kl", "ch", "hartigan",
                   "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial",
                   "frey", "mcclain","dunn", "hubert", "sdindex", "dindex", "sdbw")
cluster_best(read.csv(file = "uci_datasets/breast/training.txt"), clust_methods, "breast") # 2 clusters

## Ecoli
clust_methods <- c("kl", "ch", "hartigan",
                   "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ball", "ptbiserial",
                   "frey", "mcclain","dunn", "hubert", "sdindex", "dindex", "sdbw")
cluster_best(read.csv(file = "uci_datasets/ecoli/training.txt"), clust_methods, "ecoli") # 2 clusters

## Fertility
clust_methods <- c("kl", "ch", "hartigan",
                   "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ball", "ptbiserial",
                   "frey", "mcclain","dunn", "hubert", "sdindex", "dindex", "sdbw")
cluster_best(read.csv(file = "uci_datasets/fertility/training.txt"), clust_methods, "fertility") # 2 clusters

## Haberman
clust_methods <- c("kl", "ch", "hartigan",
                   "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial",
                   "frey", "mcclain","dunn", "hubert", "sdindex", "dindex", "sdbw")
cluster_best(read.csv(file = "uci_datasets/haberman/training.txt"), clust_methods, "haberman") # 3 clusters

## Liver
clust_methods <- c("kl", "ch", "hartigan",
                   "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball",
                    "mcclain","dunn", "hubert", "sdindex", "dindex", "sdbw")
cluster_best(read.csv(file = "uci_datasets/liver/training.txt"), clust_methods, "liver") # 2 clusters

## Ionoesphere
clust_methods <- c("kl", "ch", "hartigan",
                   "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ball",
                    "mcclain","dunn", "hubert", "sdindex", "dindex", "sdbw")
cluster_best(read.csv(file = "uci_datasets/ionosphere/training.txt"), clust_methods, "ionosphere") # 3 clusters

## Mammo
clust_methods <- c("kl", "ch", "hartigan",
                   "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial",
                   "frey", "mcclain","dunn", "hubert", "sdindex", "dindex", "sdbw")
cluster_best(read.csv(file = "uci_datasets/mammo/training.txt"), clust_methods, "mammo") # 2 clusters

## Parkinson
clust_methods <- c("kl", "ch", "hartigan",
                   "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial",
                   "frey", "mcclain","dunn", "hubert", "sdindex", "dindex", "sdbw")
cluster_best(read.csv(file = "uci_datasets/parkinson/training.txt"), clust_methods, "parkinson") # 3 clusters


## Seeds
clust_methods <- c("kl", "ch", "hartigan",
                   "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial",
                   "frey", "mcclain","dunn", "hubert", "sdindex", "dindex", "sdbw")
cluster_best(read.csv(file = "uci_datasets/seeds/training.txt"), clust_methods, "seeds") # 2 clusters