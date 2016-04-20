# load packages
library(dplyr)
library(BBmisc)
library(ggplot2)
library(GGally)
library(mlr)

# load dataset
train <- read.csv(file.path("data", "train.csv"), 
                  stringsAsFactors = FALSE, na.strings = "")
test <- read.csv(file.path("data", "test.csv"),
                 stringsAsFactors = FALSE, na.strings = "")

# add Survived variable to test
test$Survived <- factor(rep("NULL", nrow(test)), levels = c(0, 1))

# check missing values
vnapply(train, function(x)sum(is.na(x)))
vnapply(test, function(x)sum(is.na(x)))

# source preprocessing function
source("preprocmlR.R")

# preprocess data
train <- preprocmlR(train)
test <- preprocmlR(test)

# pairs plot
pdf("pairsPlot.pdf", height = 20, width = 20)
pairsPlot <- train %>% ggpairs
print(pairsPlot)
dev.off()

# descriptive tables
train %>% na.omit %>% group_by(Pclass, Age_cat, Sex) %>% 
  summarise(n(), mean(Survived == 1)) %>% as.data.frame

## predictive modelling in mlr:
# make task
classif_task <- makeClassifTask(id = "Titanic", data = train, 
                                target = "Survived", positive = "1")

# make learner
logreg_lrn <- makeLearner("classif.logreg", predict.type = "prob", 
                          fix.factors.prediction = TRUE)
rforest_lrn <- makeLearner("classif.randomForest", predict.type = "prob",
                          fix.factors.prediction = TRUE)
# create impute wrapper for logreg: impute numeric by mean/factor by mode
logreg_wrap <- makeImputeWrapper(logreg_lrn, 
                                 cols = list(Fare = imputeMean(),
                                                Embarked = imputeMode(),
                                                Age_cat = imputeMode()))
rforest_wrap <- makeImputeWrapper(rforest_lrn, 
                                 cols = list(Fare = imputeMean(),
                                             Embarked = imputeMode(),
                                             Age_cat = imputeMode()))
# resample description
rdescr <- makeResampleDesc("CV", iters = 10)

# resampled logreg
logreg_res <- resample(logreg_wrap, classif_task, rdescr)
rforest_res <- resample(rforest_wrap, classif_task, rdescr)
# more clean combination
bm <- benchmark(list(logreg_wrap, rforest_wrap), classif_task, rdescr)
