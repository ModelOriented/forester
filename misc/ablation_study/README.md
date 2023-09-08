# Description

This directory describes the experiments conducted during the Cyber Summer 2023 project in ablation study of the forester package. It's results are available in the for of paper titled TOADD, also available in this folder.

## Notebooks

The code used for achieving the outcomes is presented in the form of R Markdown notebooks. Each of these files is available in form of raw Rmd file, as well as more attractive and prepared html. The sequence for reading the notebooks is:

1. `ablation_study_datasets_info` - It describes the datasets used for ablation study with the usage of `data_check()` function. All files necessary for running the code are available in the directory, as well as the outcomes.
2. `ablation_study_preprocessing` - It describes the preparation of preprocessed datasets and strategies used for this sake. All files necessary for running the code are available in the directory. The most important outcomes are placed on google drive, in a folder: `ablation_preprocessing_results`.
3. `ablation_study_training` - It describes the training step of the study. Files necessary for running the code are available in the directory and on a google drive. The most important outcomes are placed on google drive, in a folder: `ablation_results`.
4. `ablation_study_results_preparation` - It processes large files obtianed during training and aggregates them in theform required for the study. Files necessary for running the code are available in the directory and on a google drive. The most important outcomes are placed in a folder: `ablation_processed_results` here and on google drive. The drive version has one more file. 
5. `ablation_study_results_analysis` - In this file we derive conclusions from the results obtained in previous steps, it is a proper ablation study file. Files necessary for running the code are available in the directory.
6. `ablation_study_paper_plots` - It prepares more advanced visualizations presented in the final paper.

## Data

Folders `ablations_processed_results`, and `regression_datsets` contain data used for running the notebooks. Other neccessary files are `binary_CC18.RData`, `data_issues_summary.csv`, and `regression_bench.RData`. Other crucial files are placed on a google drive, as the files were bigger than allowed by GitHub. 

Google drive: https://drive.google.com/drive/folders/1e8BIDJ4MjrSHoAKntYmUGsW2zifmKHxy?usp=sharing
