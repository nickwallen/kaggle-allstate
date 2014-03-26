
#
# processes the data 
#
preprocess.unquoted.model <- function (data, train = TRUE) {
  
  # extract the shopping history
  shopping <- extract.shopping.history (data)
  
  # extract attributes about each customer; use the first quote data only
  customers <- data [ shopping.pt == 1, list (
    customer.id, 
    day.of.week, 
    time.of.day.hours, 
    group.size, 
    homeowner, 
    car.age, 
    car.value, 
    risk.factor, 
    age.oldest, 
    age.youngest, 
    married.couple, 
    option.c.previous,
    duration.previous, 
    cost
    )]
  setkey (customers, "customer.id")
  
  # generate the outcome only for training data - FUGLY
  if (train) {
    
    # yes/no indicator instead of using the raw count from last.quote
    add.last.quote (data)
    data [, unquoted.purchase := (last.quote == 0) ]
    data [, unquoted.purchase := as.factor (unquoted.purchase) ]
    
    # the unquoted purchase indicator will be the same for each customer
    unquoted <- data [, list (
      unquoted.purchase = unique (unquoted.purchase)
    ), by = customer.id]

    # merge everyting
    return (customers [ shopping ][ unquoted ])
    
  } else {
    
    return (customers [ shopping ])
  }
}

#
# trains the unquoted model given a set of training data
#
train.unquoted.model <- function (data) {
  require ("caret")
  
  # which model parameters will be tuned?
  tune.grid <- expand.grid (
    n.trees           = c(50, 100, 200), 
    shrinkage         = 0.1,
    interaction.depth = c(1, 5, 9))
  
  predictors <- setdiff (names(data), c("customer.id", "unquoted.purchase"))
  
  # tune/train a separate model for each option
  model <- train (
    x         = data[, predictors, with = FALSE],
    y         = data [["unquoted.purchase"]],
    method    = "gbm", 
    #trControl = trainControl (method = "cv", number = 2),
    #tuneGrid  = tune.grid,
    trControl = trainControl (method = "none"),
    tuneGrid  = data.frame (n.trees = 200, shrinkage = 0.1, interaction.depth = 5),
    verbose   = TRUE 
  )
  
  return (model)
}

#
# predict the future with the unquoted model
#
predict.unquoted.model <- function (model, data) {
  
  # again... FUGLY
  data [, unquoted.purchase.hat := predict (model, newdata = .SD), .SDcols = 1:length(data) ]
  
  #unquoted.purchase.hat <- predict (model, newdata = data)
}


