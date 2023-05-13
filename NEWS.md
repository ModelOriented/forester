# forester 1.2.1
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
