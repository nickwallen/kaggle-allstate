
#
# the naive model by which to compare all other models.  the naive model simply
# selects the most popular option in the training data.
#
naive.model <- function (train) {
  
  # find the total number of customers who chose each option
  options <- rbindlist ( list (
    train [, list (option = "option.a", .N), by = list (choice = option.a)],
    train [, list (option = "option.b", .N), by = option.b],
    train [, list (option = "option.c", .N), by = option.c],
    train [, list (option = "option.d", .N), by = option.d],
    train [, list (option = "option.e", .N), by = option.e],
    train [, list (option = "option.f", .N), by = option.f],
    train [, list (option = "option.g", .N), by = option.g]
  ))
  
  # find only the most popular choice for each option
  popular.options <- options[, .SD [ N == max (N) ], by = option]
  
  # return a representation of this model that can be passed to 'predict'
  model <- list (popular.options = popular.options)
  class (model) <- "naive"
  
  return (model)
}

#
# creates predictions from a naive model built using the 'naive.model' function.
# this is an implementation of the generic function stats::predict.
#
predict.naive <- function (model, newdata) {
  
  # extract the most popular options
  option.mx <- t (model$popular.options [, list (option, choice)])
  options <- option.mx ["option", ]
  choices <- option.mx ["choice", ]

  # how many predictions do we need to make?
  rows <- nrow (newdata)
  
  # how many variables are there to predict?
  cols <- length (choices)
  
  # is there a more efficient way to do this?
  predictions <- data.frame (matrix (rep (choices, rows), ncol = cols, byrow = T))
}
