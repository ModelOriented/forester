library(DALEX)
library(xgboost)
library(lightgbm)

set.seed(43)

# Data agaricus.train as list object:
data("agaricus.train",package = 'xgboost') 
label_agaricus <- as.data.frame(agaricus.train$label)
agaricus_comb <- cbind(agaricus.train$data, agaricus.train$label * 10 + 1)
colnames(agaricus_comb)[127] <- c("label")

# Data apartments for regression:
data(apartments)
data(apartmentsTest)
apartments_test <- apartmentsTest[, 2:6]


# Data iris for classification:
data(iris)
iris_bin <- iris[iris$Species %in% c("setosa","versicolor"), ]
iris_bin$Species <- as.factor(iris_bin$Species)
iris_bin_test <- iris_bin[, 1:4]
rows <- sample(nrow(iris_bin_test))
iris_bin_test <- iris_bin_test[rows, ]


# Data titanic for classification:
data(titanic)
titanic_test <- titanic[, 1:8]
rows <- sample(nrow(titanic_test))
titanic_test <- titanic_test[rows, ]

###### Objects for compare function 

ranger <- make_ranger(apartments, "m2.price", "regression")
xgboost <- make_xgboost(apartments, "m2.price", "regression")

models_regr <- list(ranger, xgboost)

ranger <- make_ranger(iris_bin, "Species", "classification")
xgboost <- make_xgboost(iris_bin, "Species", "classification")

models_classif <- list(ranger, xgboost)




