

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
# TODO - caret has some way to build our your own models using caret's infrastructure.  explore
# that some more.
#
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
# flattens a customer's shopping history into a single record that can then be 
# used for training and prediction.  the summarize function can be changed to 
# summarize the shopping history in different ways.
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

#
# runs the complete the shopping model and exports the results to a file
# which can be submitted to kaggle.
#
champion.shopping.model <- function(verbose = FALSE) {
  
  # fetch the competition training data set and transform it for training
  shopping.train <- extract.shopping.history (fetch (), sum)
  
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
      y         = shopping.train [[option]],
      x         = shopping.train [, 2:23, with = F],
      tuneGrid  = tune.grid,
      verbose   = verbose )
  })
  
  # transform the test data for prediction
  shopping.test <- extract.shopping.history (fetch (train = FALSE), sum)
  
  # make a prediction for each option
  for (option.hat in names (models)) {
    
    shopping.test [, option.hat := predict (models[[option.hat]], .SD), 
                      .SDcols = 2:23, with = FALSE ]
  }
  
  # create a competition submission
  create.submission (shopping.test)
}