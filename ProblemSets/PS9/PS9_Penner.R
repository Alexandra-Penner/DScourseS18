# PS9
# Alli Penner
library(glmnet)
library(mlr)

# data prep
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

housing$lmedv    <- log(housing$medv)
housing$medv     <- NULL # drop median value
formula    <- as.formula(lmedv ~ .^3 +
                           poly(crim, 6) + 
                           poly(zn, 6) + 
                           poly(indus, 6) + 
                           poly(nox, 6) + 
                           poly(rm, 6) +
                           poly(age, 6) + 
                           poly(dis, 6) +
                           poly(rad, 6) + 
                           poly(tax, 6) + 
                           poly(ptratio, 6) + 
                           poly(b, 6) + 
                           poly(lstat, 6))
mod_matrix <- data.frame(model.matrix(formula, housing))

mod_matrix[, 1] = housing$lmedv
colnames(mod_matrix)[1] = "lmedv" 


# Break up the data:
n <- nrow(mod_matrix)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
housing.train <- mod_matrix[train,]
housing.test <- mod_matrix[test, ]


# lasso
lassodata <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")

# new prediction algorithm
LASSOModel <- makeLearner("regr.glmnet")

resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

# Do the resampling
sampleResults <- resample(learner = LASSOmodel, task = lassodata, resampling = resampleStrat, measures=list(rmse))

# Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
LASSOparam <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

# Take 50 random guesses at lambda 
tuneMethod <- makeTuneControlRandom(maxit = 50L)

# tune
tunedModel <- tuneParams(learner = LASSOModel,
                         task = lassodata,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = LASSOparam,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
LASSOModel <- setHyperPars(learner=LASSOModel, par.vals = tunedModel$x)

# cross validates
resample(LASSOModel,lassodata,resampleStrat,measures=list(rmse))

# Train the final model
finallasso <- train(learner = LASSOModel, task = lassodata)

predictionlasso <- predict(finallasso, newdata = housing.test)

performance(prediction)
str(prediction)

# ridge regression
RIDGEModel <- makeLearner("regr.glmnet")
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)
ridgedata <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")

# Do the resampling
sampleResults <- resample(learner = RIDGEModel, task = ridgedata, resampling = resampleStrat, measures=list(rmse))

# Search over penalty parameter lambda and force elastic net parameter to be 1 (ridge)
RIDGEparam <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))

# Take 50 random guesses at lambda 
tuneMethod <- makeTuneControlRandom(maxit = 50L)

# tune
tunedModel <- tuneParams(learner = RIDGEModel,
                         task = ridgedata,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = RIDGEparam,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
RIDGEModel <- setHyperPars(learner=RIDGEModel, par.vals = tunedModel$x)

# cross validates
resample(RIDGEModel,ridgedata,resampleStrat,measures=list(rmse))

# Train the final model
finalridge <- train(learner = RIDGEModel, task = ridgedata)

predictionridge <- predict(finalridge, newdata = housing.test)

performance(predictionridge,measures = list(rmse) )
str(prediction)


# elastic net regression
elasticModel <- makeLearner("regr.glmnet")
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)
elasticdata <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")

# Do the resampling
sampleResults <- resample(learner = elasticModel, task = elasticdata, resampling = resampleStrat, measures=list(rmse))

# Search over penalty parameter lambda and force elastic net parameter to be 1 (elastic)
elasticparam <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))

# Take 50 random guesses at lambda 
tuneMethod <- makeTuneControlRandom(maxit = 50L)

# tune
tunedModel <- tuneParams(learner = elasticModel,
                         task = elasticdata,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = elasticparam,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
elasticModel <- setHyperPars(learner=elasticModel, par.vals = tunedModel$x)

# cross validates
resample(elasticModel,elasticdata,resampleStrat,measures=list(rmse))

# Train the final model
finalelastic <- train(learner = elasticModel, task = elasticdata)

predictionelastic <- predict(finalelastic, newdata = housing.test)

performance(predictionelastic,measures = list(rmse) )
str(predictionelastic)
