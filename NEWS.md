# forester 1.0.0
* General code quality and readability improvements as well as improving the quality of the documentation.
* `check_data()` added detection of id columns and reformatted the outputs.
* `create_ranked_list()` able to work with missing values.
* Improved datasets documentation.
* `explain()` function redesigned to work on single and multiple models.
* `plot_metrics()` improved all plots and changed them into `ggplot` visualizations, added a `feature importance` plot.
* Renamed `predcit_models_all()` into `predict_models_all()` and enabled prediction on non-fixed, larger amount of models.
* Added `predict_new()` function for new observations.
* Expanded `preprocessing()` function by optional, advanced preprocessing consisting of deleting correlated values, deleting id columns, selecting only the most important features via the BORUTA algorithm.
* Redesigned a file generated by the 'report()` function, by enhancing the quality of the plots.
* Added `save()` function which saves the output of the `train()` function.
* Redesigned a `score_models()` function, so that the user can add their own metric function for scoring models.
* Redesigned a `train()` function by:
	* Adding a verbose parameter for silent runs.
	* Adding parameters for limiting the iterations of random search and bayesian optimization.
	* Turning on the advanced preprocessing.
	* Increased the output information by adding engine, predictions of all models, predictions of the best models and raw_training dataset (unprocessed by `prepare_data()`).
* Added a `verbose_cat()` function for optional messages.