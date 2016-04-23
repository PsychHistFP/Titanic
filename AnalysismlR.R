# load packages
library(dplyr)
library(BBmisc)
library(ggplot2)
library(GGally)
library(mlr)
library(pryr)
library(stringr)

# source preprocessing function
source("preprocessing.R")

# load dataset
train <- read.csv(file.path("data", "train.csv"), 
                  stringsAsFactors = FALSE, na.strings = "")
test <- read.csv(file.path("data", "test.csv"),
                 stringsAsFactors = FALSE, na.strings = "")


# # check missing values
# vnapply(train, function(x)sum(is.na(x)))
# vnapply(test, function(x)sum(is.na(x)))

# preprocess data
#train <- preprocmlR(train)
train <- train %>% addFeatures %>% cleanData
test <- test %>% addFeatures %>% cleanData(test = TRUE)

# # pairs plot
# pdf("pairsPlot.pdf", height = 20, width = 20)
# pairsPlot <- train %>% ggpairs
# print(pairsPlot)
# dev.off()
# # pairs plot Sex, Age, Pclass
# pdf("pairsPlotsmall.pdf", height = 20, width = 20)
# pairsPlotsmall <- train %>% select(Sex, Age_cat, Pclass, Survived) %>%
#   ggpairs(lower = "blank")
# print(pairsPlotsmall)
# dev.off()

# # descriptive tables
# train %>% na.omit %>% group_by(Pclass, Age_cat, Sex) %>% 
#   summarise(n(), mean(Survived == 1)) %>% as.data.frame

## predictive modelling in mlr:
# make task
classif_task <- makeClassifTask(id = "Titanic", data = train, 
                                target = "Survived", positive = "1")

# make learners
rforest_lrn <- makeLearner("classif.randomForest", predict.type = "prob",
                          fix.factors.prediction = TRUE)


rforest_wrap <- makeImputeWrapper(rforest_lrn,
                                 classes = list(numeric = imputeMedian(),
                                                factor = imputeMode()),
                                 cols = list(Age = imputeLearner(
                                   makeLearner("regr.rpart"))))

# train tuned models
ps = makeParamSet(makeDiscreteParam("mtry", values = 3:8),
                  makeDiscreteParam("ntree", values = 1000*1:2))
ctrl = makeTuneControlGrid()
rdescr <- makeResampleDesc("CV", iters = 10)
res = tuneParams(rforest_wrap, task = classif_task, resampling = rdescr, 
                 par.set = ps, control = ctrl)
rforest_hyp_lrn <- setHyperPars(rforest_wrap, par.vals = res$x)
rforest_tuned <- train(rforest_hyp_lrn, classif_task)

# getLearnerModel(rforest_tuned, more.unwrap = TRUE)$importance
# getLearnerModel(rforest_tuned, more.unwrap = TRUE)$confusion


# resample description
# rdescr <- makeResampleDesc("CV", iters = 10)

# compare predictive performance 
# bm <- benchmark(list(rforest_wrap), classif_task, rdescr, 
#                 measures = list(mmce, acc))
# bm
# bm <- benchmark(rforest_wrap, classif_task, rdescr,
#                 measures = list(mmce, acc))

# make predictions
rforest_pred <- predict(rforest_tuned, newdata = test)

# write csv predictions for kaggle
rforest_pred_kaggle <- test %>% 
  mutate(Survived = as.data.frame(rforest_pred)$response) %>% 
  select(PassengerId, Survived)

write.csv(rforest_pred_kaggle, "rforest_pred.csv", row.names = FALSE)

