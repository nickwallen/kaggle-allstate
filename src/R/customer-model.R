
#
# trains, tunes, predicts, and creates a kaggle submission file for the current
# champion shopping model.  ultimately the shopping model will need to be 
# ensembled with the customer model.  the summary.func argument is used 
# the function used to flatten/summarize multiple shopping points
#
export.customer.model <- function (file  = "../../submissions/red-swingline-predictions-customer.csv") {
  
  # train the models and append the outcomes (each customer's purchase options)
  data <- fetch (train = TRUE)
  train <- preprocess.customer.model (data)
  extract.purchases (data, train)
  models <- train.customer.model (train)
  
  # predict the future
  test <- preprocess.customer.model (fetch (train = FALSE))
  predict.customer.model (models, test)
  create.submission (test, file)
}

preprocess.customer.model <- function (train) {
  
  # extract a customer's attributes from the last quote they received
  latest.quote <- train [ record.type == "shopping", .SD [ which.max (shopping.pt) ], by = customer.id ]
  customers <- latest.quote [, c("customer.id", customer.attributes()), with = FALSE ]
  
  # additional features
  add.population.density (customers)
  add.senior.driver (customers)
  add.teen.driver (customers)
  add.young.driver (customers)
  add.with.children (customers)
  add.customer.density (customers)
  add.customer.density.by.location (customers)
  
  return (customers)
}

#
# returns the names of each of the customer model predictors
#
customer.model.predictors <- function (train) {
  predictors <- non.options (train) 
  predictors <- setdiff (predictors, c("customer.id", "location", "state"))
}

#
# trains the customer model; one for each option.
#
train.customer.model <- function (train) {
 
  # remove any missing values before training
  train <- train [ complete.cases (train) ]
  predictors <- customer.model.predictors (train)  
  
  # which model parameters will be tuned?
  tune.grid <- expand.grid (
    n.trees           = c(50, 100, 200), 
    shrinkage         = 0.1,
    interaction.depth = c(1, 5, 9))
  
  # tune/train a separate model for each option
  models <- lapply (options(), function (option) {
    train (
      method    = "gbm", 
      tuneGrid  = tune.grid,
      trControl = trainControl (method = "cv", number = 2),
      y         = train [[ option ]],
      x         = train [, predictors, with = F] )
  })
  
  # name each of the models in the list
  names(models) <- options.hat()
}

#
# creates predictions from the customer model
#
predict.customer.model <- function (models, predictors) {
  
  # make a prediction for each option [a-g] using the correct model
  for (option.hat in names (models)) {
    m <- models [[option.hat]]
    test [, option.hat := predict (m, .SD), .SDcols = predictors, with = FALSE ]
  }
  
  # the predictions are added in-place to the predictors data set
  return (NULL)
}
