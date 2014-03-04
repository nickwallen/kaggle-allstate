

#
# the opposite of the %in% operator; aka not in.  this implementation was 
# shamelessly stolen from the help("%in%") documentation.
#
#    (1:10) %nin% c(3,7,12)
#
"%nin%" <- function(x, y) !x %in% y
