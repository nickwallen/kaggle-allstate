
#
# trains, tunes, predicts, and creates a kaggle submission file for the current
# champion shopping model.  ultimately the shopping model will need to be 
# ensembled with the customer model.  the summary.func argument is used 
# the function used to flatten/summarize multiple shopping points
#
export.alpha.model <- function (summary.func = weighted.sum.most.recent, 
                                verbose      = FALSE, 
                                file         = "../../submissions/red-swingline-predictions-shopping.csv") {
  
  # fetch the competition training data set and transform it for training
  raw <- fetch (train = TRUE)
  train <- extract.quotes (raw, summary.func)
  train <- extract.purchases (raw, train)
  
  # train the alpha model
  models <- cache ("alpha-models", train.alpha.model(train))
  
  # transform the test data for prediction
  raw <- fetch (train = FALSE)
  test <- extract.quotes (raw, summary.func)
  
  # make a prediction for each option [a-g] using the correct model
  predict.alpha.model (models, test)
  
  # create a submission file that can be uploaded to kaggle
  create.submission (test, file)
}

#
# trains a separate model for each option.
#
train.alpha.model <- function (train, verbose = TRUE) {
  
  # which model parameters will be tuned?
  parameters <- expand.grid (
    n.trees           = c(50, 100, 200), 
    shrinkage         = 0.1,
    interaction.depth = c(1, 5, 9))
  
  # tune/train a separate model for each option
  models <- lapply (options(), function (option) {
    train (
      method    = "gbm", 
      trControl = trainControl (method = "cv", number = 2),
      y         = train [[ option ]],
      x         = train [, 2:23, with = F],
      tuneGrid  = parameters,
      verbose   = verbose )
  })
  
  # name each of the models in the list
  names(models) <- options.hat()
  
  return (models)
}

#
# creates predictions from the alpha model
#
predict.alpha.model <- function (models, predictors) {
  
  # make a prediction for each option [a-g] using the correct model
  for (option.hat in names (models)) {
    predictors [, option.hat := predict (models [[option.hat]], .SD), .SDcols = 2:23, with = FALSE ]
  }
  
  # the predictions are added in-place to the predictors data set
  return (NULL)
}

#
# a function which returns a weighted mean where the most recent
# values (towards the end of the list) are given higher weights
#
weighted.mean.most.recent <- function (x) {
  weights <- c(1:length(x)) / length(x)
  weighted.mean (x, weights)
}

#
# a function which returns a weighted mean where the most recent
# values (towards the end of the list) are given higher weights
#
weighted.sum.most.recent <- function (x) {
  weights <- c(1:length(x)) / length(x)
  sum (x * weights)
}
