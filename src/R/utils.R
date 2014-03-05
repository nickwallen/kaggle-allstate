

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
