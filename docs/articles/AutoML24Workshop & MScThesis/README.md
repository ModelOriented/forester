# Description

This folder contains the source codes, initial datasets, and aggregated results of [@HuberR21](https://github.com/HubertR21) Master's Thesis named 'The impact of data preprocessing on the quality of tree-based models with forester package', which are also the results of poster, and paper called 'Do tree-based models need data preprocessing?'. The experiments were conducted with forester in version 1.6.1.

# Structure

- RData files contain datasets used in the study.
- Rmd notebooks start with 01-08 which indicates the order of their execution. With their usage the user is able to repeat the computations of this study. The subsequent files contain the following operations:
	- 01: Dataset selection for multiclass tasks from CC-18 benchmark.
	- 02: Creation of 6 datasets with artificially diminished data quality.
	- 03: Preparation of preprocessed datasets scenarios.
	- 04: Training the models on each preprocessed dataset.
	- 05: Preparation of aggregated results.
	- 06: Preliminary analysis of the results in their raw form.
	- 07: In-depth results analysis.
	- 08: Time complexity analysis.
- The MSc_processed_results directory contains the final results after the 05 step. It includes the following files:
	- The preprocessing_duration.RData file: Duration in seconds for each preprocessing strategy.
	- The testing_summary_table.RData file: Performance of each model evaluated on testing subset.
	- The training_duration.RData file: Duration in seconds for each dataset training.
	- The training_summary_table.RData file: Performance of each model evaluated on training subset.
	- The validation_summary_table.RData file: Performance of each model evaluated on validation subset.
	- The training_summary directory contains RData files with raw results for each dataset separately. *WARNING* Some files are too big for Git, thus they are available on Google Drive only.

As some files are too big for Git, they are available on Google Drive only: https://drive.google.com/drive/folders/1sQkzZE9sjqdIQTSQ2bKB4z0YfR-8hXm7?usp=sharing.
