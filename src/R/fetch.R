
#
# loads and cleans the competition data.  the function defaults to
# loading the training data.  the test data can be loaded by supplying
# FALSE to the 'train' argument.
#
fetch <- function (train = TRUE) {
  require (data.table)
  require (lubridate)

  # determine the path to the source data
  path <- ifelse (train, "../../data/train.csv.zip", "../../data/test.csv.zip")
  
  # unzip and load the training data
  data.csv <- unzip(path, exdir = tempdir())
  train <- fread (data.csv)
  
  # fix-up the column names
  orig <- names (train)
  setnames (train, orig, make.names (tolower (orig), allow_ = F, unique = T))
  setnames (train, "day",        "day.of.week")
  setnames (train, "time",       "time.of.day")
  setnames (train, "c.previous", "option.c.previous")
  setnames (train, 
            c("a","b","c","d","e","f","g"), 
            c("option.a","option.b","option.c","option.d","option.e","option.f","option.g"))
  
  # clean-up the data types
  train [, `:=` (
    customer.id       = as.character (customer.id),
    shopping.pt       = as.factor (shopping.pt),
    day.of.week       = wday (day.of.week, label = T, abbr = T),
    time.of.day.hours = as.hours (time.of.day),
    
    # many of the features are factors
    state             = as.factor (state),
    location          = as.factor (location),
    risk.factor       = as.factor (risk.factor),
    car.value         = as.factor (car.value),
    option.c.previous = as.factor (option.c.previous),
    option.a          = as.factor (option.a),
    option.b          = as.factor (option.b),
    option.c          = as.factor (option.c),
    option.d          = as.factor (option.d),
    option.e          = as.factor (option.e),
    option.f          = as.factor (option.f),
    option.g          = as.factor (option.g),
    cost              = as.numeric (cost),

    # make some of the factor levels more intellegible
    homeowner         = factor (homeowner,      levels = 0:1, labels = c("no","yes")),
    record.type       = factor (record.type,    levels = 0:1, labels = c("shopping","purchase")),
    married.couple    = factor (married.couple, levels = 0:1, labels = c("no","yes"))
  )]
  
  # reorder some of the columns
  setcolorder(train, c(1:5,26,6:25))
  
  return (train)
}

#
# converts a numeric representation of the day of week to an 
# abbreviated string.  hint: 0 = "Mon".  this function redefines
# the numeric implementation for the generatic function lubridate::wday.
#
wday.numeric <- function(x, label = FALSE, abbr = TRUE) {
  if (!label) {
    return (x)
  }
  
  labels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  if (abbr) {
    labels <- substr (labels, 1, 3)
  }
  
  ordered(x, levels = 0:6, labels = labels)
}

#
# converts a 24-hour time of day string such as "04:30" to a numeric
# value that represents the number of minutes since midnight.  for example,
# "01:30" would be 90 as this is 90 minutes past midnight.
#
as.hours <- function (x) {
  require (stringr)
  
  # extract the HH and MM components of the input
  hours <- str_extract (x, "^[0-9]{1,2}")
  minutes <- str_extract (x, "[0-9]{2}$")

  # convert the input to minutes since midnight
  as.numeric (hours) + (as.numeric (minutes) / 60)
}
