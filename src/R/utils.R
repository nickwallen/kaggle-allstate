

#
# the opposite of the %in% operator; aka not in.  this implementation was 
# shamelessly stolen from the help("%in%") documentation.
#
#    (1:10) %nin% c(3,7,12)
#
"%nin%" <- function(x, y) !x %in% y

#
# returns the name of the variables which will contain the predicted
# option values. the prediction for 'option.x' will be called 'option.x.hat'
#
options.hat <- function () {
  paste0 (options(), ".hat")
}

#
# the names of each variable that contains the options that need to be predicted.
#
options <- function () {
  c ("option.a", "option.b", "option.c", "option.d", "option.e", "option.f", "option.g")
}

#
# the names of all predictor variables; ie all those except options() and options.hat()
#
non.options <- function (data) {
  setdiff(names(data), options())
}

#
# defines all of the valid 'levels' for each option
#
option.a.levels <- function () c(0, 1, 2)
option.b.levels <- function () c(0, 1)
option.c.levels <- function () c(1, 2, 3, 4)
option.d.levels <- function () c(1, 2, 3)
option.e.levels <- function () c(0, 1)
option.f.levels <- function () c(0, 1, 2, 3)
option.g.levels <- function () c(1, 2, 3, 4)

#
# creates a valid csv that can be submitted to kaggle
#
create.submission <- function (data, file = "../../submissions/red-swingline-predictions.csv") {
  
  # extract only the customer and predicted plan
  add.plan.hat (data)
  submission <- data [, list (customer_ID = customer.id, plan = plan.hat) ]
  
  # write to a csv file
  write.csv (submission, file, row.names = FALSE, quote = FALSE)
  
  return (normalizePath (file))
}


