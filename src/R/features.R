
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
add.plan <- function (data) {
  data [, plan := paste0 (option.a, option.b, option.c, option.d, 
                          option.e, option.f, option.g )]
  
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

