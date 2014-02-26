library (data.table)


fetch <- function (path = "../../data/train.csv.zip") {
  
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
    day.of.week    = wday (day.of.week + 1, label = T, abbr = T),
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