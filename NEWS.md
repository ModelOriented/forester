# forester 1.6.0

-   Updated `.Rbuildignore`, `DESCRIPTION`, and `NAMESPACE`.
-   Added `cran-comments.md` file for future CRAN submission.
-   Fixed use of the deprecated size parameter in plotting functions, and replaced it with linewidth.
-   Fixed issues, which occured while creating plots for outputs with less than 10 models.
-   Enhanced the verbose mode, by adding more information about the current stage of the training process, added execution times for different training methods, and improved the overall readability of the messages.
-   Introduced parallel training for random search, and bayesian optimization, which significantly speeds up the training process. It works for `ranger`, and `xgboost` only, due to the limitations of other packages. It results in an addition of the `parallel` parameter in `train()`, `random_search()`, and `train_models_bayesopt()` functions.
-   Intorduced more verbosity options for bayesian optimization process by introudcing the `bayes_info` parameter in `train()`, and `train_models_bayesopt()` functions. The user can now track the quality of found hyperparameters.
-   Introduced the `select_models()` function, which lets the user shrink the output of `train()` function to the selected models only. It is useful when the user wants to focus on the best models only, and limit the size of the output, while maintaining the ability to generate reports, and other functionalities.
-   Fixed an issue where for different tasks we had incosistent parameters in the output of the `train()` function, regarding the predictions of all three sets.
-   Modified the tests, so they fit with the current package version, and added tests for `select_models()` function.
-   Added `check_correlation` parameter for both `train()` and `check_Data()` functions, which lets the user decide whether to check the correlation between the features or not. In some corner cases, with highly dimensional data, it can be time-consuming, and not necessary.
    
# forester 1.5.0

-   Updated `.Rbuildignore` and `.gitignore`.
-   Updated package `DESCRIPTION`, and `NAMESPACE`.
-   Removed: `choose_best_models.R`, `forester_palette.R`, `format_model_details.R`
-   Updated `check_data()`:
    -   Added more checks of input parameters,
    -   Modified `check_y_balance()` sub-function for multiclass classification.
-   Updated `custom_preprocessing()`:
    -   Added parameter type,
    -   Added more checks of input parameters,
    -   Added more advanced verbose options.
-   Added `multiclass classification` task, with all extensional functionalities included (report, explainability, plots, etc), which led to the abundance of changes in majority of functions.
-   Added more input checks for `train_test_balance()`.
-   Updated documentation.
-   Added manual test for multiclass classification task.
-   Moved old tests into `misc/old_tests_03_02_2023` folder.
-   Created a new, and more in depth tests for the package from scratch for regression, binary classification, and multiclass classification tasks.

# forester 1.4.2

-   In the DESCRIPTION updated the description, and RoxygenNote version.
-   In README updated guide for catboost installation.
-   Moved `verbose_cat()`, and `guess_type()`, from their own files to `check_data()`.
-   Removed `choose_best_models()` function, and implemented its logic directly in `train()`.
-   Removed the unused `create_ranked_list()`, and `format_models_details()` functions.
-   Fixed `plot_classification()` function, associated with the error *cannot xtfrm data frames* occurred.
-   Suppressed meaningless warnings from `prepare_data()`, and `preprocessing_removal()` functions.
-   Fixed error *cannot xtfrm data frames* in both reports.
-   Added folder `misc/manual_tests` with supplementary, manual tests of the package, focusing on report generation.
-   Removed `test-choose_best_model` test, and fixed two other tests.

# forester 1.4.1

-   In the DESCRIPTION:
    -   Added in Imports the `patchwork` package, and moved `arules` from Imports to Suggests,
    -   Updated the authors of the package.
-   In `report()` function:
    -   Added separate generation of binary classification and regression reports,
    -   Enhanced the quality of plots in the report,
    -   Fixed the bugs leading to lower quality of the report,
    -   Added detailed description of how to interpret the visualizations for the beginners.
-   Created `plot_classifcation` file, which overrides `plot()` function for the binary classification object returned by `train()`. The function lets us create plots for binary classification tasks, such as metrics comparison line plot, ROC curve, confusion matrix, and train vs test plot.
-   Created `plot_regression` file, which overrides `plot()` function for the regression object returned by `train()`. The function lets us create plots for regression tasks, such as residuals box plot, observed vs prediction plot, and train vs test plot.
-   Removed `report.Rmd` file, and created two separate files for different tasks called `report_binary.Rmd`, and `report_regression.Rmd`.
-   In `train()` function: added different classes for the outcomes, depending on the task type.
-   In `plot_metrics()`, changed color palette to `colors_discrete_forester`.
-   In `check_data()`, added a small fix for the report.

# forester 1.4.0

-   In the DESCRIPTION: Added in Suggests `SurvMetrics`, `randomForestSRC`, and `survival` packages.
-   In `train()` function:
    -   Added parameters `time`, and `status` describing the survival analysis task,
    -   Added parameters `time`, and `status` to the functions output,
    -   Modified inner methods, to work with survival analysis task.
-   In `check data()` function:
    -   Added parameters `time`, and `status` describing the survival analysis task,
    -   Modified the methods: `basic_info()`, `check_missing()`, `check_cor()`, and `check_y_balance()`, so they also work for the survival analysis.
-   In `preprocessing()` function, added the binarization of survival task target (status).
-   In `prepare_data()` function, added a method for the survival analysis task.
-   In `train_models()` function, added a method for training a survival analysis model from `randomForestSRC` package.
-   In `random_search()` function, added a method for tuning a survival analysis model.
-   In `train_models_bayesopt()` function, added a method for tuning a survival analysis model.
-   In `predict_models_all()`, `predict_models()` , and `predict_new` functions, added a method for predicting a survival analysis model.
-   In `score_models()`, added a method for evaluation of survival analysis models with `Brier Score`, and `Concordance Index (CIN)`.
-   In `guess_type()`, added method of detecting the survival analysis task.
-   Modified `tests` of the package, to cover the changes made in the package.

# forester 1.3.0

-   In the DESCRIPTION: Added in Imports the `VIM` package, and in the Suggests `sivs`, `parallel`, `rmcfs`, and `varrank` packages.
-   In `check_data()` function:
    -   removed 'no', and added 'index' to id_names used for id-like columns detection,
    -   Fixed minor printing and code formatting issues.
-   Added a `custom_preprocessing()` function, which is more advanced and customizable approach for the preprocessing pipeline. It executes other new functions implementing three major pillars of preprocessing. The functions are `preprocessing_removal()`, `preprocessing_imputation()`, `preprocessing_feature_selection()`:
    -   `preprocessing_removal()` - This function includes 6 modules for the removal of unwanted features / observations. We can remove duplicate columns, the ID-like columns, static columns (with specified staticity threshold), sparse columns (with specified sparsity threshold), and highly correlated ones (with specified high correlation threshold). Additionally we can remove the observations that are too sparse (sparsity threshold), and have missing target value. One can turn on and off each module by setting proper logical values.
    -   `preprocessing_imputation()` - Imputes missing values according to one of four prepared methods:
        -   `median-other` - The numeric features are imputed with median value, whereas the categorical ones with the 'other' string,
        -   `median-frequency` - The numeric features are imputed with median value, whereas the categorical ones with the most frequent value,
        -   `knn` - All features are imputed with KNN algorithm,
        -   `mice` - All features are imputed with MICE algorithm.
    -   `preprocessing_feature_selection()` - Conducts a feature selection process with one out of four proposed methods:
        -   `VI` - The variable importance method based on random forest,
        -   `MCFS` - The Monte Carlo Feature Selection,
        -   `MI` - The Varrank method based on mutual information scores,
        -   `BORUTA` - The BORUTA algorithm - short time.
-   Added tests for the `custom_preprocessing()`, `preprocessing_removal()`, `preprocessing_imputation()`, and `preprocessing_feature_selection()` functions.
-   In the `train()` function:
    -   Removed `advanced_preprocessing` parameter, as `custom_preprocessing()` is more advanced version of it,
    -   Added `custom_preprocessing` parameter, which takes the output of a `custom_preprocessing()` function,
    -   Added `deleted_rows` value to the output,
    -   Changed output parameter from `columns` to `deleted_columns`.
-   In the `preprocessing()` function:
    -   Removed `advanced` parameter, as `custom_preprocessing()` is more advanced version of it,
    -   Changed output parameter from `columns` to `rm_columns`.
    -   Removed the example from this function.
-   In the `save()` function:
    -   Renamed to `save_forest()`,
    -   Merged parameters `name` and `path` into `file` as it is standard approach
    -   Added an example of loading saved data to documentation.
-   In the `prepare_data()` function added encoding for the `ctree` model so it can use columns with more than 30 levels.
-   In the `random_search()` function added parameter `verbose`. When set to TRUE the function provides information about the training.
-   Removed plenty of examples from functions that are not meant for the user.
-   Fixed plenty of formatting issues in various functions.

## forester 1.2.1

-   In the `train()` function:
    -   Added the `split_seed` parameter which enables the user to set the seed for the train-test split method,
    -   Added the `train_inds`, `test_inds`, and `valid_inds` vectors to the train output, which enable the user to recover the information which observation went to the train, test, and validation sets.
-   In the `train_models_bayesopt()` function:
    -   Removed unwanted seeds before starting the Bayesian optimization.

# forester 1.2.0

-   In the `check_data()` function:
    -   Fixed typos.
-   Updated the documentation or examples in:
    -   `choose_best_model()`,
    -   `create_ranked_list()`,
    -   `format_models_details()`,
    -   `draw_scatter_plot()`,
    -   `predict_models()`,
    -   `predict_models_all()`,
    -   `save()`,
    -   `train_models()`,
    -   `train_test_balance()`.
-   In the `explain()` function:
    -   Updated the documentation and the examples,
    -   Fixed the handling of the factors.
-   In the `draw_radar_plot()` function:
    -   Updated the documentation,
    -   Deleted unnecessary prints.
-   In the `prepare_data()` function:
    -   Updated the documentation,
    -   Fixed the process of creating an 'other' label during the preparation of train / test splits.
-   In the `preprocessing()` function:
    -   Updated the documentation,
    -   Removed unused parameter type,
    -   Added a factorization of the target for binary classification task.
-   In the `random_search()` function:
    -   Updated the documentation,
    -   Fixed the method for the ranger model,
    -   Unified the grid with the Bayesian Optimization grid.
-   In the `score_models()` function:
    -   Updated the documentation,
    -   Added RMSE as a main metric for the regression task,
    -   Added accuracy as a main metric for the binary classification task,
    -   Fixed the scoring method for the regression.
-   In the `train()` function:
    -   Updated the documentation,
    -   Enabled providing the tibble as a data parameter (with warning),
    -   Added a warning if the user tries to use incorrect task type,
    -   Added score_train and predictions train to the output of the function.
-   In the `train_models_bayesopt()` function:
    -   Updated the documentation,
    -   Fixed the Bayesian Optimization. Before hand it didn't work at all for models other than ranger,
    -   Changed tuned hyperparameters and their values for all models, which increased the performance of the method.
-   Updated the majority of `tests` so they match the current version.
-   Added alternative catboost installation in the `README`.

# forester 1.1.4

-   In the `report()` function:
    -   Removed engine and tuning columns from the ranked list table,
    -   Explained how the model name is created, so the columns engine and tuning are not needed,
    -   Fixed the radar plot issues,
-   In the `draw_radar_plot()` function:
    -   Fixed radar plots, so the models are named correctly (by model names, not engines),
    -   Fixed issue where the plot for binary classification had an empty observation name,
    -   Removed unnecessary and invalid Metric names (the ones that are not used and engine, name, tuning).
-   In the `check_data()` function:
    -   Added a vector of possible outliers to the output,
    -   The outliers indexes are printed if there are less than 50 of them.
-   In the `train()` function:
    -   Added a vector of possible outliers to the output.

# forester 1.1.3

-   In the `train()` function:
    -   Added objects to the output of the function:
        -   Vector: valid_observed.
        -   Vectors: train_observed_labels, test_observed_labels, valid_observed_labels, predictions_all_labels predictions_best_best as labels for binary classification task.
    -   Removed catboost from default engines.
    -   Added check if catboost is installed.
    -   Removed parameters: loss, validation, tuning, keep, because they weren't used.
    -   Added train_test_split parameter that determines proportions of train, test and validation subsets.
    -   Conditioned the message about columns deleted during the advanced preprocessing.
-   In the `check_data()` function:
    -   Fixed an issue of too much unique values for the Crammer V's correlation. If the number of combinations exceeds 2\^29, the correlation is not calculated and the user is informed about it (it is caused by R limitations).
    -   Fixed an issue with omitting strong correlations that are negative.
    -   Added ticks and crosses that indicate whether something is of the high or low quality.
    -   Added the information which class is the dominating one in the unbalanced classification data set.
-   In the `preprocessing()` function:
    -   Added a factorization of the target at the beginning of the `binarize_target()` sub function.
    -   Resolved issues with data frames where columns are not saved as factors.
-   The `report()` function:
    -   If tinytex is not installed raises an error.
    -   If ggradar is not installed, there will be no error, no plot, and a note about installing the ggradar package.
-   Removed engine parameter from `predict_models_all()`.
-   Removed misspelled function `predcit_models_all()`.
-   The `train_models()` function no longer returns NULL objects if engine is not selected.
-   The `train_models_bayesopt()` function no longer returns NULL objects if engine is not selected.
-   For the `format_models_details()` function fixed a bug with the method not working for a classification task for an xgboost model.
-   In the `DESCRIPTION` catboost, ggradar and tinytex dependencies moved from Suggests to Imports and added crayon to Import.
-   Added installation guides for catboost, ggradar and tinytex in the `README.md`, which solves installation issues on macOS.

# forester 1.1.2

-   Cleared typos and inconsistent notation in the `train()` function.
-   Added info that the *Bayesian Optimization* takes a lot of time.
-   Fixed *'The best model details'* section in the report to be seen.
-   Added info about deleted columns during `preprocessing()`.
-   Added more details of the `train()` function output.
-   Added parameter *'seed'* to keep the data division comparable.
-   Cleared structure of the `predict_new()` function.
-   Kept numeric values of *model*$raw_data$lightgbm_data as numeric, and not string values.
-   Used different parameters for the xgboost model to remove the warnings.
-   Changed [1,2] target values to [0,1] for the regression task.
-   Added info about the type of correlation used between columns.

# forester 1.1.1

-   In the `save()` the file name has a right month.
-   The `train()` function returns table with metrics on validation subset.
-   The `score()` function returns tables with additional columns: `engine` and `tuning`.
-   Added a new vignette 'Knowledge Check' with extended use cases.
-   Setting `bayes_iter = 0` causes that `Bayestian_opt()` is not ran anymore.
-   Changed the parameter name from `random_iter` to `random_evals`.
-   Setting `verbose = FALSE` disables the `check_data` entirely.

# forester 1.1.0

-   Changes in a `train()` function by adding parameters to:
    -   Choose metrics.
    -   Choose metric to sort model.
    -   Add self-created metric function. \# forester 1.0.0
-   General code quality and readability improvements as well as improving the quality of the documentation.
-   `check_data()` added detection of id columns and reformatted the outputs.
-   `create_ranked_list()` able to work with missing values.
-   Improved data sets documentation.
-   `explain()` function redesigned to work on single and multiple models.
-   `plot_metrics()` improved all plots and changed them into `ggplot` visualizations, added a `feature importance` plot.
-   Renamed `predcit_models_all()` into `predict_models_all()` and enabled prediction on non-fixed, larger amount of models.
-   Added `predict_new()` function for new observations.
-   Expanded `preprocessing()` function by optional, advanced preprocessing consisting of deleting correlated values, deleting id columns, selecting only the most important features via the BORUTA algorithm.
-   Redesigned a file generated by the 'report()\` function, by enhancing the quality of the plots.
-   Added `save()` function which saves the output of the `train()` function.
-   Redesigned a `score_models()` function, so that the user can add their own metric function for scoring models.
-   Redesigned a `train()` function by:
    -   Adding a verbose parameter for silent runs.
    -   Adding parameters for limiting the iterations of random search and Bayesian optimization.
    -   Turning on the advanced preprocessing.
    -   Increased the output information by adding engine, predictions of all models, predictions of the best models and raw_training data set (unprocessed by `prepare_data()`).
-   Added a `verbose_cat()` function for optional messages.
