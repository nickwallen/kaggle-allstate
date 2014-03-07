
#
# models the shopping history and predicts a single option using 'gbm'. 
#
gbm.model <- function (option,
                       train, 
                       i.depth = c(1, 5, 9),
                       n.trees = c(50, 100, 150, 200),
                       shrink  = 0.1,
                       control = trainControl (method = "cv", number = 5),
                       ...) {
  # tune and train the model
  fit <- train (
    method    = "gbm", 
    trControl = control,
    y         = as.factor (shopping [[option]]),
    x         = shopping [, 2:23, with = F ],
    tuneGrid  = expand.grid (interaction.depth = i.depth, n.trees = n.trees, shrinkage = shrink),
    ... )
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
# used for training and prediction.
#
flatten.shopping.history <- function (data) {
  
  # determine how many times a customer shopped for each option choice
  shopping <- data [ record.type == "shopping", list (
    
    # option a
    option.a.0 = sum (option.a == 0),
    option.a.1 = sum (option.a == 1),
    option.a.2 = sum (option.a == 2),
    
    # option b
    option.b.0 = sum (option.b == 0),
    option.b.1 = sum (option.b == 1),
    
    # option c
    option.c.1 = sum (option.c == 1),
    option.c.2 = sum (option.c == 2),
    option.c.3 = sum (option.c == 3),
    option.c.4 = sum (option.c == 4),
    
    # option d
    option.d.1 = sum (option.d == 1),
    option.d.2 = sum (option.d == 2),
    option.d.3 = sum (option.d == 3),
    
    # option e
    option.e.0 = sum (option.e == 0),
    option.e.1 = sum (option.e == 1),
    
    # option f 
    option.f.0 = sum (option.f == 0),
    option.f.1 = sum (option.f == 1),
    option.f.2 = sum (option.f == 2),
    option.f.3 = sum (option.f == 3),
    
    # option g
    option.g.1 = sum (option.g == 1),
    option.g.2 = sum (option.g == 2),
    option.g.3 = sum (option.g == 3),
    option.g.4 = sum (option.g == 4)
    
  ), by = customer.id ]
  setkey (shopping, "customer.id")
  
  # which options to the shopper actually purchase?
  purchase <- data [ record.type == "purchase", c("customer.id", options()), with = FALSE ]
  setkey (purchase, "customer.id")
  
  # merge the shopping and purchase history
  shopping [ purchase, `:=` (option.a = option.a, 
                             option.b = option.b,
                             option.c = option.c,
                             option.d = option.d,
                             option.e = option.e,
                             option.f = option.f,
                             option.g = option.g )]
  
  return (shopping)
}

