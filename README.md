one-class classification

In this repository we will compare different techniques for anomaly detection. The techniques we have used for the review are: SMOTE, SVDD, PSVM, FRaC, Logistic Regression with corrections, DBN and clustering with one of these techniques.

We have used 11 differentes datasets from the UCI repository and we have created anomaly scenarios: we considered as an anomaly class the one which contains less observations. we select randomly 5% of the observations. Due to the randomness, we have created 10 train-test sets to make conclusions. We have also used different metrics to optimize and evaluate the models: Recall, F1 and Jaccard.

Running the write_results_train and write_results_test with the corresponding parameters for each model, you will have the training and testing results. To fit the model for FRaC, we have used the code provided by the creators, which you can cownload from their webpage (http://bcb.cs.tufts.edu/frac/). In this case, we used the executions.sh file to obtain the results.

The plotting_train, plotting and scores files are to evaluate the performance of the models.
