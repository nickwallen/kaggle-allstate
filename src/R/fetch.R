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
  data.csv <- unzip (path, exdir = tempdir())
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
    record.type       = factor (record.type, levels = 0:1, labels = c("shopping","purchase")),
    day.of.week       = wday (day.of.week, label = T, abbr = T),
    time.of.day.mins  = as.minutes (time.of.day),
    state             = as.factor (state),
    location          = as.factor (location),
    risk.factor       = as.factor (risk.factor),
    married.couple    = factor (married.couple, levels = 0:1, labels = c("no","yes")),
    car.value         = as.factor (car.value),
    homeowner         = factor (homeowner, levels = 0:1, labels = c("no","yes")),
    option.c.previous = as.factor (option.c.previous),
    option.a          = as.factor (option.a),
    option.b          = as.factor (option.b),
    option.c          = as.factor (option.c),
    option.d          = as.factor (option.d),
    option.e          = as.factor (option.e),
    option.f          = as.factor (option.f),
    option.g          = as.factor (option.g),
    cost              = as.numeric (cost)
  )]
  
  # reorder some of the columns
  setcolorder (train, c(1:5,26,6:25))
  
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
as.minutes <- function (x) {
  require (stringr)
  
  # extract the HH and MM components of the input
  hours <- str_extract (x, "^[0-9]{1,2}")
  minutes <- str_extract (x, "[0-9]{2}$")

  # convert the input to minutes since midnight
  as.numeric (hours) * 60 + as.numeric (minutes)
}

#
# calculates the duration in days between a customer's first shopping 
# contact and their last purchase contact. 
#
# WARNING: this makes a big assumption that the first contact to purchase 
# is always within 7 days.  this is *probably* a fairly safe assumption 
# for most of the data.  the data does not provide enough information
# to do any better.
#
shopping.duration <- function (data) {
  
  # define the first and last shopping point for each record.  this makes
  # the logic later more clear.  for example... 
  #
  # where shopping point = 1, the first is 1 and the last is 1.  
  # where shopping point = 3, the first is 1 and the last is 3.
  shopping.pts <- data [, list (
    customer.id,
    shopping.pt.first = 1,
    shopping.pt.last = shopping.pt
    )]
  
  # find the day of week for the first shopping point
  setkeyv (data, c("customer.id", "shopping.pt"))
  setkeyv (shopping.pts, c("customer.id", "shopping.pt.first"))
  shopping.pts [ data, day.of.week.first := day.of.week]
  
  # find the day of week for the last shopping point
  setkeyv (data, c("customer.id", "shopping.pt"))
  setkeyv (shopping.pts, c("customer.id", "shopping.pt.last"))
  shopping.pts [ data, day.of.week.last := day.of.week]
  
  # can assume that the point of first contact to purchase was within the same week
  shopping.pts [ day.of.week.first <= day.of.week.last, 
                 shopping.duration := day.of.week.last - day.of.week.first ]
  
  # must assume that the point of first contact was on week 1 and purchase on week 2
  shopping.pts [ day.of.week.first >  day.of.week.last, 
                 shopping.duration := day.of.week.last - (day.of.week.first - as.integer(7)) ]
  
  # merge the shopping duration back into the original feature/data set
  setkey (data, "customer.id")
  setkey (shopping.pts, "customer.id")
  data [ shopping.pts, shopping.duration := shopping.duration]  
  
  # the input data is modified in-place
  return (NULL)
}

