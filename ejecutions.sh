#!/bin/sh
########## BIODEGRAD ################ string-->float error (removing features 12,14,30 it works)

frac/detect -X ../../one-class-classification/uci_datasets/biodegrad/FRaC/training_frac -Q ../../one-class-classification/uci_datasets/biodegrad/FRaC/testing_frac -d ',' -m ../../one-class-classification/uci_datasets/biodegrad/FRaC/metadata -T -w ~/Development/Weka/weka-3-8-1/weka.jar -o ../../one-class-classification/uci_datasets/biodegrad/FRaC/biodegrad_predictions

########### BLOOD TRANSFUSION ############### OK

frac/detect -X ../../one-class-classification/uci_datasets/blood_trans/FRaC/training_frac -Q ../../one-class-classification/uci_datasets/blood_trans/FRaC/testing_frac -d ',' -m ../../one-class-classification/uci_datasets/blood_trans/FRaC/metadata -T -w ~/Development/Weka/weka-3-8-1/weka.jar -o ../../one-class-classification/uci_datasets/blood_trans/FRaC/blood_trans_predictions

########### BREAST ############ OK

frac/detect -X ../../one-class-classification/uci_datasets/breast/FRaC/training_frac -Q ../../one-class-classification/uci_datasets/breast/FRaC/testing_frac -d ',' -m ../../one-class-classification/uci_datasets/breast/FRaC/metadata -T -w ~/Development/Weka/weka-3-8-1/weka.jar -o ../../one-class-classification/uci_datasets/breast/FRaC/breast_predictions


########### ECOLI ########### OK

frac/detect -X ../../one-class-classification/uci_datasets/ecoli/FRaC/training_frac -Q ../../one-class-classification/uci_datasets/ecoli/FRaC/testing_frac -d ',' -m ../../one-class-classification/uci_datasets/ecoli/FRaC/metadata -T -w ~/Development/Weka/weka-3-8-1/weka.jar -o ../../one-class-classification/uci_datasets/ecoli/FRaC/ecoli_predictions

########### FERTILITY ############ OK

frac/detect -X ../../one-class-classification/uci_datasets/fertility/FRaC/training_frac -Q ../../one-class-classification/uci_datasets/fertility/FRaC/testing_frac -d ',' -m ../../one-class-classification/uci_datasets/fertility/FRaC/metadata -T -w ~/Development/Weka/weka-3-8-1/weka.jar -o ../../one-class-classification/uci_datasets/fertility/FRaC/fertility_predictions

########### HABERMAN ############# OK

frac/detect -X ../../one-class-classification/uci_datasets/haberman/FRaC/training_frac -Q ../../one-class-classification/uci_datasets/haberman/FRaC/testing_frac -d ',' -m ../../one-class-classification/uci_datasets/haberman/FRaC/metadata -T -w ~/Development/Weka/weka-3-8-1/weka.jar -o ../../one-class-classification/uci_datasets/haberman/FRaC/haberman_predictions

########### IONOSPHERE ########### string-->float error (removing feature 25 it works)

frac/detect -X ../../one-class-classification/uci_datasets/ionosphere/FRaC/training_frac -Q ../../one-class-classification/uci_datasets/ionosphere/FRaC/testing_frac -d ',' -m ../../one-class-classification/uci_datasets/ionosphere/FRaC/metadata -T -w ~/Development/Weka/weka-3-8-1/weka.jar -o ../../one-class-classification/uci_datasets/ionosphere/FRaC/ionosphere_predictions

########### LIVER ############ OK

frac/detect -X ../../one-class-classification/uci_datasets/liver/FRaC/training_frac -Q ../../one-class-classification/uci_datasets/liver/FRaC/testing_frac -d ',' -m ../../one-class-classification/uci_datasets/liver/FRaC/metadata -T -w ~/Development/Weka/weka-3-8-1/weka.jar -o ../../one-class-classification/uci_datasets/liver/FRaC/liver_predictions

########### MAMMO ############ OK

frac/detect -X ../../one-class-classification/uci_datasets/mammo/FRaC/training_frac -Q ../../one-class-classification/uci_datasets/mammo/FRaC/testing_frac -d ',' -m ../../one-class-classification/uci_datasets/mammo/FRaC/metadata -T -w ~/Development/Weka/weka-3-8-1/weka.jar -o ../../one-class-classification/uci_datasets/mammo/FRaC/mammo_predictions

########### PARKINSON ############ OK

frac/detect -X ../../one-class-classification/uci_datasets/parkinson/FRaC/training_frac -Q ../../one-class-classification/uci_datasets/parkinson/FRaC/testing_frac -d ',' -m ../../one-class-classification/uci_datasets/parkinson/FRaC/metadata -T -w ~/Development/Weka/weka-3-8-1/weka.jar -o ../../one-class-classification/uci_datasets/parkinson/FRaC/parkinson_predictions

########### SEEDS ############ OK

frac/detect -X ../../one-class-classification/uci_datasets/seeds/FRaC/training_frac -Q ../../one-class-classification/uci_datasets/seeds/FRaC/testing_frac -d ',' -m ../../one-class-classification/uci_datasets/seeds/FRaC/metadata -T -w ~/Development/Weka/weka-3-8-1/weka.jar -o ../../one-class-classification/uci_datasets/seeds/FRaC/seeds_predictions



