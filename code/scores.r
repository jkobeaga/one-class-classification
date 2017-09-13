# RECALL
svdd_r <- read.csv("./results/recall/testing/results_svdd.txt", sep = ",")
oneclass_r <- read.csv("./results/recall/testing/results_Scholkopf.txt", sep = ",")
smote_50_r <- read.csv("./results/recall/testing/results_SMOTE_50.txt", sep = ",")
smote_60_r <- read.csv("./results/recall/testing/results_SMOTE_60.txt", sep = ",")
smote_65_r <- read.csv("./results/recall/testing/results_SMOTE_65.txt", sep = ",")
weights_r <- read.csv("./results/recall/testing/results_weights.txt", sep = ",")
lr_r <- read.csv("./results/recall/testing/results_LR.txt", sep = ",")
autoencoder_r <- read.csv("./results/recall/testing/results_autoencoder.txt", sep = ",")
svdd_cr <- read.csv("./results/recall/testing/results_cluster_svdd.txt", sep = ",")
oneclass_cr <- read.csv("./results/recall/testing/results_cluster_Scholkopf.txt", sep = ",")
smote_50_cr <- read.csv("./results/recall/testing/results_cluster_SMOTE_50.txt", sep = ",")
smote_60_cr <- read.csv("./results/recall/testing/results_cluster_SMOTE_60.txt", sep = ",")
smote_65_cr <- read.csv("./results/recall/testing/results_cluster_SMOTE_65.txt", sep = ",")
weights_cr <- read.csv("./results/recall/testing/results_cluster_weights.txt", sep = ",")
frac <- read.csv("./results/recall/testing/results_frac.txt", sep = ",")


svdd_r <- add_metric(svdd_r)
oneclass_r <- add_metric(oneclass_r)
smote_50_r <- add_metric(smote_50_r)
smote_60_r <- add_metric(smote_60_r)
smote_65_r <- add_metric(smote_65_r)
weights_r <- add_metric(weights_r)
lr_r <- add_metric(lr_r)
autoencoder_r <- add_metric(autoencoder_r)
svdd_cr <- add_metric(svdd_cr)
oneclass_cr <- add_metric(oneclass_cr)
smote_50_cr <- add_metric(smote_50_cr)
smote_60_cr <- add_metric(smote_60_cr)
smote_65_cr <- add_metric(smote_65_cr)
weights_cr <- add_metric(weights_cr)
frac <- add_metric(frac)

results_df <- svdd_r[,c(1,8)]
colnames(results_df) <- c("file", "svdd")
results_df$psvm <- oneclass_r[,8]
results_df$smote50 <- smote_50_r[,8]
results_df$smote60 <- smote_60_r[,8]
results_df$smote65 <- smote_65_r[,8]
results_df$weights <- weights_r[,8]
results_df$lr <- lr_r[which(lr_r$Corrected=="TRUE"),9]
results_df$autoencoder <- autoencoder_r[,8]
results_df$svdd_c <- svdd_cr[,8]
results_df$psvm_c <- oneclass_cr[,8]
results_df$smote50_c <- smote_50_cr[,8]
results_df$smote60_c <- smote_60_cr[,8]
results_df$smote65_c <- smote_65_cr[,8]
results_df$weight_c <- weights_cr[,8]
results_df$frac <- frac[1:110,8]


scores <- rep(0,ncol(results_df)-1)
for(i in 1:nrow(results_df)){
  fila <- results_df[i,-1]
  scores <- scores + rank(fila)
}

# F1

svdd_r <- read.csv("./results/F1/testing/results_svdd.txt", sep = ",")
oneclass_r <- read.csv("./results/F1/testing/results_Scholkopf.txt", sep = ",")
smote_50_r <- read.csv("./results/F1/testing/results_SMOTE_50.txt", sep = ",")
smote_60_r <- read.csv("./results/F1/testing/results_SMOTE_60.txt", sep = ",")
smote_65_r <- read.csv("./results/F1/testing/results_SMOTE_65.txt", sep = ",")
weights_r <- read.csv("./results/F1/testing/results_weights.txt", sep = ",")
lr_r <- read.csv("./results/F1/testing/results_LR.txt", sep = ",")
autoencoder_r <- read.csv("./results/F1/testing/results_autoencoder.txt", sep = ",")
svdd_cr <- read.csv("./results/F1/testing/results_cluster_svdd.txt", sep = ",")
oneclass_cr <- read.csv("./results/F1/testing/results_cluster_Scholkopf.txt", sep = ",")
smote_50_cr <- read.csv("./results/F1/testing/results_cluster_SMOTE_50.txt", sep = ",")
smote_60_cr <- read.csv("./results/F1/testing/results_cluster_SMOTE_60.txt", sep = ",")
smote_65_cr <- read.csv("./results/F1/testing/results_cluster_SMOTE_65.txt", sep = ",")
weights_cr <- read.csv("./results/F1/testing/results_cluster_weights.txt", sep = ",")
frac <- read.csv("./results/recall/testing/results_frac.txt", sep = ",")


svdd_r <- add_metric(svdd_r)
oneclass_r <- add_metric(oneclass_r)
smote_50_r <- add_metric(smote_50_r)
smote_60_r <- add_metric(smote_60_r)
smote_65_r <- add_metric(smote_65_r)
weights_r <- add_metric(weights_r)
lr_r <- add_metric(lr_r)
autoencoder_r <- add_metric(autoencoder_r)
svdd_cr <- add_metric(svdd_cr)
oneclass_cr <- add_metric(oneclass_cr)
smote_50_cr <- add_metric(smote_50_cr)
smote_60_cr <- add_metric(smote_60_cr)
smote_65_cr <- add_metric(smote_65_cr)
weights_cr <- add_metric(weights_cr)
frac <- add_metric(frac)

results_df <- svdd_r[,c(1,10)]
colnames(results_df) <- c("file", "svdd")
results_df$psvm <- oneclass_r[,10]
results_df$smote50 <- smote_50_r[,10]
results_df$smote60 <- smote_60_r[,10]
results_df$smote65 <- smote_65_r[,10]
results_df$weights <- weights_r[,10]
results_df$lr <- lr_r[which(lr_r$Corrected=="TRUE"),11]
results_df$autoencoder <- autoencoder_r[,10]
results_df$svdd_c <- svdd_cr[,10]
results_df$psvm_c <- oneclass_cr[,10]
results_df$smote50_c <- smote_50_cr[,10]
results_df$smote60_c <- smote_60_cr[,10]
results_df$smote65_c <- smote_65_cr[,10]
results_df$weight_c <- weights_cr[,10]
results_df$frac <- frac[1:110,10]


scores <- rep(0,ncol(results_df)-1)
for(i in 1:nrow(results_df)){
  fila <- results_df[i,-1]
  scores <- scores + rank(fila)
}


# JACCARD


svdd_r <- read.csv("./results/jaccard/testing/results_svdd.txt", sep = ",")
oneclass_r <- read.csv("./results/jaccard/testing/results_Scholkopf.txt", sep = ",")
smote_50_r <- read.csv("./results/jaccard/testing/results_SMOTE_50.txt", sep = ",")
smote_60_r <- read.csv("./results/jaccard/testing/results_SMOTE_60.txt", sep = ",")
smote_65_r <- read.csv("./results/jaccard/testing/results_SMOTE_65.txt", sep = ",")
weights_r <- read.csv("./results/jaccard/testing/results_weights.txt", sep = ",")
lr_r <- read.csv("./results/jaccard/testing/results_LR.txt", sep = ",")
autoencoder_r <- read.csv("./results/jaccard/testing/results_autoencoder.txt", sep = ",")
svdd_cr <- read.csv("./results/jaccard/testing/results_cluster_svdd.txt", sep = ",")
oneclass_cr <- read.csv("./results/jaccard/testing/results_cluster_Scholkopf.txt", sep = ",")
smote_50_cr <- read.csv("./results/jaccard/testing/results_cluster_SMOTE_50.txt", sep = ",")
smote_60_cr <- read.csv("./results/jaccard/testing/results_cluster_SMOTE_60.txt", sep = ",")
smote_65_cr <- read.csv("./results/jaccard/testing/results_cluster_SMOTE_65.txt", sep = ",")
weights_cr <- read.csv("./results/jaccard/testing/results_cluster_weights.txt", sep = ",")
frac <- read.csv("./results/recall/testing/results_frac.txt", sep = ",")


svdd_r <- add_metric(svdd_r)
oneclass_r <- add_metric(oneclass_r)
smote_50_r <- add_metric(smote_50_r)
smote_60_r <- add_metric(smote_60_r)
smote_65_r <- add_metric(smote_65_r)
weights_r <- add_metric(weights_r)
lr_r <- add_metric(lr_r)
autoencoder_r <- add_metric(autoencoder_r)
svdd_cr <- add_metric(svdd_cr)
oneclass_cr <- add_metric(oneclass_cr)
smote_50_cr <- add_metric(smote_50_cr)
smote_60_cr <- add_metric(smote_60_cr)
smote_65_cr <- add_metric(smote_65_cr)
weights_cr <- add_metric(weights_cr)
frac <- add_metric(frac)

results_df <- svdd_r[,c(1,11)]
colnames(results_df) <- c("file", "svdd")
results_df$psvm <- oneclass_r[,11]
results_df$smote50 <- smote_50_r[,11]
results_df$smote60 <- smote_60_r[,11]
results_df$smote65 <- smote_65_r[,11]
results_df$weights <- weights_r[,11]
results_df$lr <- lr_r[which(lr_r$Corrected=="TRUE"),11]
results_df$autoencoder <- autoencoder_r[,11]
results_df$svdd_c <- svdd_cr[,11]
results_df$psvm_c <- oneclass_cr[,11]
results_df$smote50_c <- smote_50_cr[,11]
results_df$smote60_c <- smote_60_cr[,11]
results_df$smote65_c <- smote_65_cr[,11]
results_df$weight_c <- weights_cr[,11]
results_df$frac <- frac[1:110,11]


scores <- rep(0,ncol(results_df)-1)
for(i in 1:nrow(results_df)){
  fila <- results_df[i,-1]
  scores <- scores + rank(fila)
}
