
#
# extracts the purchase history for each customer from a source data set and then
# merges it (appends columns) to a destination data set.
#
# there is always one, and only one, purchase record for each customer.  this 
# record defines which options a customer actually purchased from AllState.  of 
# course, the purchase records are available only in training data, not the test data.
#
extract.purchases <- function (src, dest, merge.by = c("customer.id")) {
  
  # which options does the shopper actually purchase?
  purchase <- src [ record.type == "purchase", c("customer.id", options()), with = FALSE ]
  if (nrow (purchase) == 0)
    stop ("no purchase record(s) found.  these only exist in the training data.")
  
  # data table doesn't merge well with factors; the level names are lost
  options.as.numeric (purchase)
  
  # merge the dest and purchase history
  setkeyv (dest,     merge.by)
  setkeyv (purchase, merge.by)
  dest [ purchase, `:=` (option.a = option.a, 
                         option.b = option.b, 
                         option.c = option.c, 
                         option.d = option.d, 
                         option.e = option.e, 
                         option.f = option.f, 
                         option.g = option.g) ]
  
  # convert back to factors after the data is merged
  options.as.factors (dest)
  
  # make explicit that the input data is modified in-place
  return (NULL)
}

#
# extracts the 1 or more quotes that each customer has received from a 
# source data set.  the function then 'flattens' the multiple quotes into a single 
# record that summarizes that customer's quote history.
#
# the 'summarize.func' argument defines *how* multiple quotes are flattened 
# into a single record.  the function can be changed to summarize the quote history 
# in different ways.  additional arguments can be passed to the summarize 
# function.  by default, the function simply sums the number of times a customer 
# shopped for each option/selection {A0, A1, ..., B0, B1, ... }.
#
# each customer has received one or more quotes in the past before making a 
# purchase.  each quote is enumerated via the 'shopping.pnt' field which indicates
# the order in which the quotes were provided over time.  
#
extract.quotes <- function (data, summarize.func = sum, ...) {
  require (caret)
  
  # extract only the shopping records
  shopping <- data [record.type == "shopping"]
  
  # transform the options into dummy variables; option.a becomes (option.a0, option.a1, option.a2)
  dummies <- dummyVars (~ option.a + option.b + option.c + option.d + option.e + option.f + option.g, shopping)
  shopping <- data.table (customer.id = shopping$customer.id, predict (dummies, shopping))
  
  # summarize the options which were shopped for using summarize.func
  shopping <- shopping [, lapply(.SD, summarize.func, ...), by = customer.id ]
  setkey (shopping, "customer.id")
  
  return (shopping)
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
add.shopping.duration <- function (data) {
  
  # define the first and last shopping point for each record.  this makes
  # the logic later more clear.  for example... 
  #
  # where shopping point = 1, the first is 1 and the last is 1.  
  # where shopping point = 3, the first is 1 and the last is 3.
  shopping.pts <- data [, list (
    customer.id,
    shopping.pt.first = as.integer(1),
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
  
  # make explicit that the input data is modified in-place
  return (NULL)
}

# 
# a Plan is a chosen set of options.  There are 2,304 unique products 
# (3*2*4*3*2*4*4).
#
add.plan <- function (data, label = "plan") {
  data [, label := paste0 (option.a, option.b, option.c, option.d, 
                          option.e, option.f, option.g ), with = FALSE]
  
  # make explicit that the input data is modified in-place
  return (NULL)
}

# 
# create a single feature that represents all of the predicted options; aka product.hat
#
add.plan.hat <- function (data) {
  data [, plan.hat := paste0 (option.a.hat, option.b.hat, option.c.hat, option.d.hat, 
                              option.e.hat, option.f.hat, option.g.hat ) ]

  # make explicit that the input data is modified in-place
  return (NULL)
}

#
# converts all of the options from int/numeric to factors
#
options.as.factors <- function (data) {
  
  data [, `:=` (
    option.a = factor (option.a, levels = option.a.levels()),
    option.b = factor (option.b, levels = option.b.levels()),
    option.c = factor (option.c, levels = option.c.levels()),
    option.d = factor (option.d, levels = option.d.levels()),
    option.e = factor (option.e, levels = option.e.levels()),
    option.f = factor (option.f, levels = option.f.levels()),
    option.g = factor (option.g, levels = option.g.levels())
  )]
  
  # updates made in-place
  return (NULL)
}

#
# converts all of the options from int/numeric to factors
#
options.hat.as.factors <- function (data) {
  
  data [, `:=` (
    option.a.hat = factor (option.a.hat, levels = option.a.levels()),
    option.b.hat = factor (option.b.hat, levels = option.b.levels()),
    option.c.hat = factor (option.c.hat, levels = option.c.levels()),
    option.d.hat = factor (option.d.hat, levels = option.d.levels()),
    option.e.hat = factor (option.e.hat, levels = option.e.levels()),
    option.f.hat = factor (option.f.hat, levels = option.f.levels()),
    option.g.hat = factor (option.g.hat, levels = option.g.levels())
  )]
  
  # updates made in-place
  return (NULL)
}

#
# converts all of the options from factors to int/numerics
#
options.as.numeric <- function (data) {
  
  data [, `:=` (
    option.a = as.numeric (as.character (option.a)),
    option.b = as.numeric (as.character (option.b)),
    option.c = as.numeric (as.character (option.c)),
    option.d = as.numeric (as.character (option.d)),
    option.e = as.numeric (as.character (option.e)),
    option.f = as.numeric (as.character (option.f)),
    option.g = as.numeric (as.character (option.g))
  )]
  
  # updates made in-place
  return (NULL)
}

#
# converts all of the options from factors to int/numerics
#
options.hat.as.numeric <- function (data) {
  
  data [, `:=` (
    option.a.hat = as.numeric (as.character (option.a.hat)),
    option.b.hat = as.numeric (as.character (option.b.hat)),
    option.c.hat = as.numeric (as.character (option.c.hat)),
    option.d.hat = as.numeric (as.character (option.d.hat)),
    option.e.hat = as.numeric (as.character (option.e.hat)),
    option.f.hat = as.numeric (as.character (option.f.hat)),
    option.g.hat = as.numeric (as.character (option.g.hat))
  )]
  
  # updates made in-place
  return (NULL)
}

#
# a feature which contains the shopping.pt value of the latest quote which matches
# the customer's final purchase.  if the customer was never quoted for a purchase,
# the value will be 0.
#
add.last.quote <- function (data) {
  
  # extract purchases only
  purchases <- data [ record.type == "purchase", c("customer.id", options()), with = F ]
  add.plan (purchases)
  
  # extract quotes only
  quotes <- data [ record.type == "shopping", c("customer.id", "shopping.pt", options()), with = F ]
  quotes [ , shopping.pt := as.numeric (shopping.pt)]
  add.plan (quotes)
  
  setkeyv (purchases, c("customer.id", "plan"))
  setkeyv (quotes, c("customer.id", "plan"))
  
  # find the last time the purchase was quoted
  purchases [ quotes, last.quote := max (shopping.pt) ]
  purchases [ is.na (last.quote), last.quote := 0 ]
  
  # merge the last quote value into the input data
  setkey (purchases, "customer.id")
  setkey (data, "customer.id")
  data [purchases, last.quote := last.quote ]
  
  # updates made in-place
  return (NULL)
}

#
# a young driver is anyone under 25
#
add.young.driver <- function (data) {
  data [, young.driver := FALSE ]
  data [ age.youngest < 25, young.driver := TRUE ]
  data [, young.driver := as.factor (young.driver) ]
  
  # updates made in-place
  return (NULL)
}

#
# a senior driver is anyone over the age of 25
#
add.senior.driver <- function (data) {
  data [, senior.driver := FALSE ]
  data [ age.oldest > 65, senior.driver := TRUE ]  
  data [, senior.driver := as.factor (senior.driver) ]
  
  # updates made in-place
  return (NULL)
}

#
# a teen driver is anyone 16-19 years of age
#
add.teen.driver <- function (data) {
  data [, teen.driver := FALSE ]
  data [ age.youngest < 20, teen.driver := TRUE ]
  data [, teen.driver := as.factor (teen.driver) ]
  
  # updates made in-place
  return (NULL)
}

#
# indicates whether the customer is likely to have children
#
add.married.with.children <- function (data) {
  data [, married.with.children := FALSE ]
  data [  married.couple == "yes" & group.size > 2, married.with.children := TRUE ]
  data [, married.with.children := as.factor (married.with.children)]
}

#
# add the population density rank of the customer's home state
#
add.population.density <- function (data) {
  expect.one.record.per.customer (data)
  
  # fetch the population density data
  density <- fread ("../../data/population-density.csv")
  
  # add the population density rank to the data  
  setkey (density, "state")
  setkey (data, "state")
  data [density, population.density.rank := rank]
  
  # updates made in-place
  return (NULL)
}

#
# insurers may be able to offer different rates, which might drive customers
# to choose different options, based on the density of customers in a geographic
# region
#
add.customer.density <- function (data) {
  
  # assertion
  expect.one.record.per.customer (data)
  
  # rank each state by the number of customers
  density <- data [, .N, by = state]
  density [, rank := rank (N) ]
  
  # add the customer density rank to the data
  setkey (data, "state")
  setkey (density, "state")
  data [density, customer.density.rank := rank]
  
  # updates made in-place
  return (NULL)
}

#
# insurers may be able to offer different rates, which might drive customers
# to choose different options, based on the density of customers in a geographic
# region
#
add.customer.density.by.location <- function (data) {
  
  # assertion
  expect.one.record.per.customer (data)
  
  # rank each location by the number of customers
  density <- data [, .N, by = location]
  density [, rank := rank (N) ]
  
  # add the customer density rank to the data
  setkey (density, "location")
  setkey (data, "location")
  data [density, customer.density.rank.by.location := rank]
  
  # updates made in-place
  return (NULL)
}

#
# rank each state by the average cost.  this rank is then added to the input data set.
#
add.cost.rank <- function (data, costs = fetch (train = TRUE)) {
  
  #costs [, state = as.character (state)]
  #data  [, state = as.character (state)]
  
  # rank each state by the average cost
  cost.rank <- costs [, mean (cost), by = state]
  cost.rank <- cost.rank [, rank := rank (V1)]
  
  # add the customer density rank to the data
  setkey (data, "state")
  setkey (cost.rank, "state")
  data [cost.rank, cost.rank := rank]
  
  # updates made in-place
  return (NULL)
}

#
# determines whether the data has only one record per customer or not.  returns
# false if there is more than one record per customer.
#
expect.one.record.per.customer <- function (data) {

  records.per.customer <- data [, .N, by = customer.id]
  customers.with.multiple <- records.per.customer[N > 1]
  
  if (nrow (customers.with.multiple) > 0) {
    stop ("epic fail! expect only 1 record per customer")
  }
}
