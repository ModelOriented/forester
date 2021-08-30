library(DALEX)
library(xgboost)
library(lightgbm)

options(mypkg.connection = stdin())
set.seed(43)

# Data agaricus.train as list object:
data("agaricus.train",package = 'xgboost') 
label_agaricus <- as.data.frame(agaricus.train$label)
agaricus_comb <- cbind(agaricus.train$data, agaricus.train$label * 10 + 1)
colnames(agaricus_comb)[127] <- c("label")
colnames(agaricus_comb) <- lapply(colnames(agaricus_comb), function(x) gsub("[[:punct:]]", "_", x))
colnames(agaricus_comb)

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

# Data for testing imbalanced data 
titanic_imbalanced <- titanic[titanic$survived == "yes",]
titanic_imbalanced <- rbind(titanic_imbalanced, head(titanic, 10))

###### Objects for compare function 

ranger_regr <- make_ranger(apartments, "m2.price", "regression")
xgboost_regr <- make_xgboost(apartments, "m2.price", "regression")

ranger_classif <- make_ranger(iris_bin, "Species", "classification")
xgboost_classif <- make_xgboost(iris_bin, "Species", "classification")





