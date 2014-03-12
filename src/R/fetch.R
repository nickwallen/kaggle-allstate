
#
# loads and cleans the competition data.  the function defaults to
# loading the training data.  the test data can be loaded by supplying
# FALSE to the 'train' argument.
#
fetch <- function (train = TRUE) {
  require (data.table)
  require (lubridate)

  # determine the path to the source data
  path <- ifelse (train, "../../data/train.csv.zip", "../../data/test_v2.csv.zip")
  
  # unzip and load the training data
  data.csv <- unzip(path, exdir = tempdir())
  data <- fread (data.csv)
  
  # fix-up the column names
  orig <- names (data)
  setnames (data, orig, make.names (tolower (orig), allow_ = F, unique = T))
  setnames (data, "day",        "day.of.week")
  setnames (data, "time",       "time.of.day")
  setnames (data, "c.previous", "option.c.previous")
  setnames (data, c("a","b","c","d","e","f","g"), options())
  
  # clean-up the data types
  data [, `:=` (
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
  
  # treat all NAs as a separate factor level
  data [, `:=` (
    risk.factor       = replace.na (risk.factor,       "missing"),
    option.c.previous = replace.na (option.c.previous, "missing")
  )]
    
  # reorder some of the columns
  setcolorder(data, c(1:5,26,6:25))
  
  return (data)
}

#
# replaces NA that exist in a factor
#
replace.na <- function (x, new.level) {
  if (!is.factor (x)) {
    stop ("function intended only for factors")
  }
  
  # add the new level to the factor
  levels (x) <- c( levels(x), new.level)

  # replace all NAs with the new level
  x [is.na(x)] <- new.level
  
  return (x)
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
