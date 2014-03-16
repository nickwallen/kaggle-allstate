
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
# trains, tunes, predicts, and creates a kaggle submission file for the current
# champion shopping model.  ultimately the shopping model will need to be 
# ensembled with the customer model.  the summary.func argument is used 
# the function used to flatten/summarize multiple shopping points
#
champion.shopping.model <- function (summary.func = weighted.sum.most.recent, 
                                     verbose      = FALSE, 
                                     file         = "../../submissions/red-swingline-predictions-shopping.csv") {
  
  # fetch the competition training data set and transform it for training
  input <- fetch()
  shopping.train <- extract.shopping.history (input, summary.func)
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
  shopping.test <- extract.shopping.history (fetch (train = FALSE), summary.func)
  
  # each option has its own prediction model...
  for (option.hat in names (models)) {
    
    # make a prediction for each option [a-g] using the correct model 
    shopping.test [, option.hat := predict (models [[ option.hat ]], .SD), 
                   .SDcols = 2:23, with = FALSE ]
  }
  
  # create a submission file that can be uploaded to kaggle
  create.submission (shopping.test, file)
}

#
# the popular model simply selects the most popular option that was shopped for 
# in the training data.  popularity is determined by how many times customers 
# have looked at a particular option in the shopping history.
#
popular.shopping.model <- function (file = "../../submissions/red-swingline-predictions-popular.csv") {
  require (reshape2)
  require (data.table)
  
  # the popular model requires no training - it simply chooses the most popular option
  test <- fetch(train = FALSE)
  
  # find the total number of customers who chose each option
  popular.options <- rbindlist ( list (
    test [, list (option = "option.a", .N), by = list (choice = option.a)],
    test [, list (option = "option.b", .N), by = option.b],
    test [, list (option = "option.c", .N), by = option.c],
    test [, list (option = "option.d", .N), by = option.d],
    test [, list (option = "option.e", .N), by = option.e],
    test [, list (option = "option.f", .N), by = option.f],
    test [, list (option = "option.g", .N), by = option.g]
  ))
  
  # find only the most popular choice for each option
  popular.options <- popular.options[, .SD [ N == max (N) ], by = option]
  popular.options <- t ( popular.options [, list(option, choice)]) [2, ]

  # only the list of customers to predict for is needed from the test data
  customers <- unique (test$customer.id)
  
  # predict the most popular options for every customer (TODO - this is ugly)
  rows <- length (customers)
  options.matrix <- matrix (rep (popular.options, rows), nrow = rows, byrow = TRUE)
  predictions <- data.table (customer.id = customers, options.matrix)
  setnames (predictions, paste0("V", 1:7), options.hat())  
  
  # create a submission file that can be uploaded to kaggle
  create.submission (predictions, file)
}

#
# an implementation of the naive model for the competition.  this model simply chooses
# the options that the customer last shopped for.
#
naive.shopping.model <- function (file = "../../submissions/red-swingline-predictions-naive.csv") {
  require (reshape2)
  require (data.table)
  
  # the naive model requires no training - it simply chooses the most popular option
  test <- fetch(train = FALSE)
  setkey (test, "customer.id")
  
  # find the last shopping record for each customer
  last.shopping.pt <- test [ record.type == "shopping", 
                             .SD [ as.numeric (shopping.pt) == max ( as.numeric (shopping.pt)) ], 
                             by = customer.id ]
  predictions <- last.shopping.pt [, c("customer.id", options()), with = FALSE]
  
  # the last options becomes the prediction
  setnames (predictions, options(), options.hat())
  
  # create a submission file that can be uploaded to kaggle
  create.submission (predictions, file)
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
