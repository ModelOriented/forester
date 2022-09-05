## **forester: automated partner for planting transparent tree-based models**

A significant amount of time is spent on building models with high performance. Selecting the appropriate model structures, optimizing hyperparameters and explainability are only part of the process of creating a machine learning-based solution. Despite the wide range of structures considered, tree-based models are champions in competitions or hackathons. So, aren't tree-based models enough?

They definitely are and that’s why we want to fully automate the machine learning process for them, so everyone will be able to use the computational power of the trees.

## Authors

This package is created inside the MI2 Data Lab as both scientific research and Bachelor thesis by:

- Adrianna Grudzień, Warsaw University of Technology, Bachelor Degree student
- Hubert Ruczyński, Warsaw University of Technology, Bachelor Degree student
- Patryk Słowakiewicz, Warsaw University of Technology, Bachelor Degree student
- MSc Anna Kozak, Warsaw University of Technology, project co-ordinator and supervisor
- PhD. Przemysław Biecek, Warsaw University of Technology, auxiliary supervisor

The previous version of forester was created by:

- Hoang Thien Ly, Warsaw University of Technology, Bachelor Degree
- Szymon Szmajdziński, Warsaw University of Technology, Bachelor Degree student

## What is the forester?

The forester is an autoML tool that wraps up all ML processes into a single `train()` function, which includes:

- Rendering a brief data check report,
- Preprocessing the initial dataset enough for models to be trained,
- Training 5 tree-based models with default parameters, random search and Bayesian optimization,
- Evaluating them and providing a ranked list.

However, that’s not everything that the forester has to offer. Via additional functions, the user can easily explain created models with the usage of DALEX or generate one of the predefined reports including:

- Data Check report - information about the dataset: correlation, outliers, missing values, …
- Detailed information about best models (parameters)
- Visualizations comparing the best models
- Explanations of the aforementioned models

## For whom is this package created?

The forester aims for both experienced and inexperienced users. The major advantage of this package is simple syntax and the ability to train plenty of highly effective models in just a few lines. It is really handy for the newcomers, because they don’t have to fully understand what stays behind the training process and they have to read only short documentation to train their first, yet highly effective models.

More experienced users are handed an easy-to-use tool that might be used for preparing high-quality baseline models for comparison with more advanced methods or a set of starting parameters for more thorough optimization.

## Preprocessing and data preparation

The first and probably most important step in the forester pipeline is preprocessing. It is so crucial because it enables the user to build models on all types of datasets, regardless of the data quality. This pipeline consists of:

- Guessing the type of ml task
- Rendering a check data report
- Preprocessing
- Balancing and splitting the dataset
- Preparation for different models

### Guessing type of ml task

The first point of the pipeline determines which type of ml task are we facing via the data analysis of the target column. For now supported tasks are binary classification and regression, however, in the future support for multilabel classification will be provided.

### Data Check report

The forester is designed for training models on data of even the lowest quality, however, in data science, we know that with poor quality input we will get poor output. That’s why to counter this issue we provide a data check report, which prints out issues present in our data. The report contains:

- Basic information about the dataset: number of observations, columns, their names and which one is a target value
- Information if there are static columns (with at least 99% same values)
- Information if there are duplicate columns
- Information about missing values in predictors and target
- Information about dimensionality issues (more features than observations, too big dimensionality for tree-based models)
- Information about strongly correlated categorical (Crammer’s V) and numerical (Pearson) values
- Information about outliers detected by 3 methods: mean standard deviation, median absolute deviation and inter quantile range
- Information about balance in target column (even distribution in quantiles for regression task)

With such a detailed report the user is aware of data quality and thus the quality of the models. Advanced users can address those issues if they want better-performing models.

### Preprocessing

However not all of the aforementioned problems we can pass to the training phase, that’s why we had to address 3 of them in preprocessing stage. We always remove static columns, because the decision tree cannot grow with such data. We also binarize the target column into numerical values for unified predictions. In the end, we have to manage missing values. To do that, we remove columns with more than 50% missing values and impute the rest via the MICE algorithm (Multivariate Imputation by Chained Equations).

### Balanced data splits

As the main task of the forester is training and predicting ml models we have to divide given datasets into train, test and validation subsets. To affirm the best possible results, we assure that given sets will be balanced in terms of target values.

### Data preparation for different models

There are plenty of problems regarding different models because they need different input data types for training and predicting. Moreover, some of them (e.g. xgboost) needs only numerical data (which means one hot encoding is needed) or factors for all possible values in both training and testing dataset. To face these issues we had to add group ‘other’ for every categorical value and map all level values into the training dataset, so it can make all possible predictions. Moreover, we create separate datasets for every model type, which enables us to train them and make predictions easily.

## Training process

After the data preparation phase, the forester conducts a training process on the training subset, which consists of creating 5 models:

- Random Forest (package: ranger)
- XGBoost (xgboost)
- Decision Tree (partykit)
- LightGBM (lightgbm)
- CatBoost (catboost)

These models are later trained with: default parameters, random search parameters, and Bayesian Optimization parameters, so in the end at least 15 models are trained. After the training phase, we are making predictions on the test subset and evaluate every model to make a ranked list of all models with calculated metrics (regression: MSE, R2, MAD; binary classification: F1, accuracy AUC), where they are sorted from the best one to the worst, by selected metric (MSE, AUC). The user can easily retrieve the parameters of the best models.

## Auto-generated reports

One of the extensional features which are great is an option to automatically generate reports from the training process. At this point there is only one predefined report which consists of:

- Data Check report
- Training report: Metrics for best models
- Radar plot comparing the best models by their metrics
- Train vs test plot, to control overfitting
- Training parameters
- Confusion matrix
- Boxplots for residuals
- Feature importance plot for the best model

In the end, we want to create more report templates, with the current one set as a default one. There might be reports more focused on: EDA, visualization of the training process, outcomes of the model with metrics and Explainable Artificial Intelligence (XAI).


