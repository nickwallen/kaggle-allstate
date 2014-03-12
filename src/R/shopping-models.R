
#
# trains, tunes, predicts, and creates a kaggle submission file for the current
# champion shopping model.  ultimately the shopping model will need to be 
# ensembled with the customer model.
#
champion.shopping.model <- function (verbose = FALSE) {
  
  # fetch the competition training data set and transform it for training
  input <- fetch()
  shopping.train <- extract.shopping.history (input, sum)
  shopping.train <- extract.purchase.history (input, shopping.train)
  
  # defines how the parameter tuning will occur
  control <- trainControl (method = "cv", number = 2)
  
  # which model parameters will be tuned?
  tune.grid <- expand.grid (
    n.trees           = c(50, 100, 200), 
    shrinkage         = 0.1,
    interaction.depth = c(1, 5, 9))
  
  # tune/train a separate model for each option
  models <- lapply (options(), function (option) {
    train (
      method    = "gbm", 
      trControl = control,
      y         = shopping.train [[ option ]],
      x         = shopping.train [, 2:23, with = F],
      tuneGrid  = tune.grid,
      verbose   = verbose )
  })
  
  # name each of the models in the list
  names(models) <- options.hat()
  
  # transform the test data for prediction
  shopping.test <- extract.shopping.history (fetch (train = FALSE), sum)
  
  # each option has its own prediction model...
  for (option.hat in names (models)) {
    
    # make a prediction for each option [a-g] using the correct model 
    shopping.test [, option.hat := predict (models [[ option.hat ]], .SD), 
                   .SDcols = 2:23, with = FALSE ]
  }
  
  # create a submission file that can be uploaded to kaggle
  create.submission (shopping.test)
}

#
# the popular model simply selects the most popular option in the training data.  
# popularity simply determines how many times the customer base has looked at
# a particular option in the shopping history.
#
popular.model <- function (data) {
  
  # find the total number of customers who chose each option
  options <- rbindlist ( list (
    data [, list (option = "option.a", .N), by = list (choice = option.a)],
    data [, list (option = "option.b", .N), by = option.b],
    data [, list (option = "option.c", .N), by = option.c],
    data [, list (option = "option.d", .N), by = option.d],
    data [, list (option = "option.e", .N), by = option.e],
    data [, list (option = "option.f", .N), by = option.f],
    data [, list (option = "option.g", .N), by = option.g]
  ))
  
  # find only the most popular choice for each option
  popular.options <- options[, .SD [ N == max (N) ], by = option]
  
  # return a representation of this model that can be passed to 'predict'
  model <- list (popular.options = popular.options)
  class (model) <- "popular.model"
  
  return (model)
}

#
# TODO - caret has some way to build your own models using caret's infrastructure.  explore
# that some more.  building a separate predict implementation was just an experiement
# so that it could be treated just like any other candidate model.
#
# creates predictions from the "popular model" built using the 'popular.model' function.
# this is an implementation of the generic function stats::predict.
#
predict.popular.model <- function (model, newdata) {

  # need a prediction for each customer
  customers <- newdata$customer.id
  
  # extract the most popular options
  option.mx <- t (model$popular.options [, list (option, choice)])
  options <- option.mx ["option", ]
  choices <- option.mx ["choice", ]

  # how many predictions do we need to make?
  rows <- length (customers)
  
  # how many variables are there to predict?
  cols <- length (choices)
  
  # is there a more efficient way to do this?
  predictions <- data.frame (matrix (rep (choices, rows), ncol = cols, byrow = T))
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
extract.shopping.history <- function (data, summarize.func = sum) {
  
  # extract only the shopping records
  shopping <- data [record.type == "shopping"]
  
  # transform the options into dummy variables; option.a becomes (option.a0, option.a1, option.a2)
  dummies <- dummyVars (~ option.a + option.b + option.c + option.d + option.e + option.f + option.g, shopping)
  shopping <- data.table (customer.id = shopping$customer.id, predict (dummies, shopping))
  
  # summarize the options which were shopped for using summarize.func
  shopping <- shopping [, lapply(.SD, summarize.func), by = customer.id ]
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
  setkey (purchase, "customer.id")
  
  # data table doesn't merge well with factors; the level names are lost
  options.as.numeric (purchase)
  
  # merge the shopping and purchase history
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
