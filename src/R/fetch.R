library (data.table)



#
# loads and cleans the competition data.  the function defaults to
# loading the training data.  the test data can be loaded by supplying
# the 'path' argument.
#
fetch <- function (path = "../../data/train.csv.zip") {
  require (data.table)
  require (lubridate)
  
  # unzip and load the training data
  data.csv <- unzip(path, exdir = tempdir())
  train <- fread (data.csv)
  
  # fix-up the column names
  orig <- names (train)
  setnames (train, orig, make.names (tolower (orig), allow_ = F, unique = T))
  setnames (train, "day", "day.of.week")
  
  # clean-up the data
  train [, `:=` (
    customer.id    = as.character (customer.id),
    shopping.pt    = as.factor (shopping.pt),
    record.type    = as.factor (record.type),
    day.of.week    = wday (day.of.week, label = T, abbr = T),
    state          = as.factor (state),
    location       = as.factor (location),
    risk.factor    = as.factor (risk.factor),
    married.couple = as.factor (married.couple),
    car.value      = as.factor (car.value),
    homeowner      = as.factor (homeowner),
    c.previous     = as.factor (c.previous),
    a              = as.factor (a),
    b              = as.factor (b),
    c              = as.factor (c),
    d              = as.factor (d),
    e              = as.factor (e),
    f              = as.factor (f),
    g              = as.factor (g),
    cost           = as.numeric (cost)
  )]
  
}

#
# converts a numeric representation of the day of week to an 
# abbreviated string.  hint: 0 = "Mon".  this function redefines
# the numeric implementation for the generatic function lubridate::wday.
#
wday.numeric <- function(x, label = FALSE, abbr = TRUE) {
  if (!label) 
    return (x)
  
  labels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  if (abbr) {
    labels <- substr (labels, 1, 3)
  }
  
  ordered(x, levels = 0:6, labels = labels)
}


