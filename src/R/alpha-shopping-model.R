
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
  data <- fetch ()
  shopping.train <- extract.shopping.history (data, summary.func)
  shopping.train <- extract.purchase.history (data, shopping.train)
  
  # train the alpha model
  models <- cache ("alpha-models", train.alpha.model(shopping.train))
  
  # transform the test data for prediction
  shopping.test <- extract.shopping.history (fetch (train = FALSE), summary.func)
  
  # make a prediction for each option [a-g] using the correct model
  predict.alpha.model (models, shopping.test)
  
  # create a submission file that can be uploaded to kaggle
  create.submission (shopping.test, file)
}

#
# trains a separate model for each option.
#
train.alpha.model <- function (shopping.train, verbose = TRUE) {
  
  # which model parameters will be tuned?
  tune.grid <- expand.grid (
    n.trees           = c(50, 100, 200), 
    shrinkage         = 0.1,
    interaction.depth = c(1, 5, 9))
  
  # tune/train a separate model for each option
  models <- lapply (options(), function (option) {
    train (
      method    = "gbm", 
      trControl = trainControl (method = "cv", number = 2),
      y         = shopping.train [[ option ]],
      x         = shopping.train [, 2:23, with = F],
      tuneGrid  = tune.grid,
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

#
# a customer has one or more "shopping points" which characterize the options a 
# customer shopped for in the past.  this function extracts and flattens a 
# customer's shopping history into a single record that can then be used for 
# training and prediction.  
#
# the 'summarize.func' argument defines how multiple shopping points are flattened 
# into a single record for each customer.  the function can be changed to summarize 
# the shopping history in different ways.  by default, the function simply sums the 
# number of times a customer shopped for each option/selection {A0, A1, ..., B0, B1, ... }.
#
# the shopping history is available in both training and test sets.
#
extract.shopping.history <- function (data, summarize.func = sum, ...) {
  require (caret)
  
  # extract only the shopping records
  shopping <- data [record.type == "shopping"]

  # transform the options into dummy variables; option.a becomes (option.a0, option.a1, option.a2)
  dummies <- dummyVars (~ option.a + option.b + option.c + option.d + option.e + option.f + option.g, shopping)
  shopping <- data.table (customer.id = shopping$customer.id, predict (dummies, shopping))
  
  # summarize the options which were shopped for using summarize.func
  shopping <- shopping [, lapply(.SD, summarize.func, ...), by = customer.id ]
  setkey (shopping, "customer.id")
  
  return (shopping)
}

#
# there is always one, and only one, purchase record for each customer.  this 
# record defines which options a customer actually purchased from AllState.  
#
# the competition is focused on predicting which options a customer will 
# purchase.  of course, the purchase records are available only in training data, 
# not the test data.
#
extract.purchase.history <- function (data, shopping) {
  
  # which options does the shopper actually purchase?
  purchase <- data [ record.type == "purchase", c("customer.id", options()), with = FALSE ]
  if (nrow (purchase) == 0)
    stop ("no purchase record(s) found.  these only exist in the training data.")
  
  # data table doesn't merge well with factors; the level names are lost
  options.as.numeric (purchase)
  
  # merge the shopping and purchase history
  setkey (purchase, "customer.id")
  shopping [ purchase, `:=` (option.a = option.a, 
                             option.b = option.b, 
                             option.c = option.c, 
                             option.d = option.d, 
                             option.e = option.e, 
                             option.f = option.f, 
                             option.g = option.g) ]
  
  # convert back to factors after the data is merged
  options.as.factors (shopping)
  
  return (shopping)
}
