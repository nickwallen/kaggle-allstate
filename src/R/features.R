
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
  
  # make explicit that the input data is modified in-place
  return (NULL)
}

# 
# a product is a chosen set of options.  There are 2,304 unique products 
# (3*2*4*3*2*4*4).
#
add.product <- function (data) {

  data [, product := paste0 (option.a, option.b, option.c, option.d, 
                             option.e, option.f, option.g )]
}

# 
# create a single feature that represents all of the predicted options; aka product.hat
#
add.product.hat <- function (data) {
  
  data [, product.hat := paste0 (option.a.hat, option.b.hat, option.c.hat, option.d.hat, 
                                 option.e.hat, option.f.hat, option.g.hat ) ]
}


#
# flattens a customer's shopping history into a single record that can then be 
# used for training and prediction.
#
flatten.shopping.history <- function (data) {
  
  # determine how many times a customer shopped for each option choice
  shopping <- data [ record.type == "shopping", list (
    
    # option a
    option.a.0 = sum (option.a == 0),
    option.a.1 = sum (option.a == 1),
    option.a.2 = sum (option.a == 2),
    
    # option b
    option.b.0 = sum (option.b == 0),
    option.b.1 = sum (option.b == 1),
    
    # option c
    option.c.1 = sum (option.c == 1),
    option.c.2 = sum (option.c == 2),
    option.c.3 = sum (option.c == 3),
    option.c.4 = sum (option.c == 4),
    
    # option d
    option.d.1 = sum (option.d == 1),
    option.d.2 = sum (option.d == 2),
    option.d.3 = sum (option.d == 3),
    
    # option e
    option.e.0 = sum (option.e == 0),
    option.e.1 = sum (option.e == 1),
    
    # option f 
    option.f.0 = sum (option.f == 0),
    option.f.1 = sum (option.f == 1),
    option.f.2 = sum (option.f == 2),
    option.f.3 = sum (option.f == 3),

    # option g
    option.g.1 = sum (option.g == 1),
    option.g.2 = sum (option.g == 2),
    option.g.3 = sum (option.g == 3),
    option.g.4 = sum (option.g == 4)
    
  ), by = customer.id ]
  setkey (shopping, "customer.id")
  
  # which options to the shopper actually purchase?
  purchase <- data [ record.type == "purchase", c("customer.id", options()), with = FALSE ]
  setkey (purchase, "customer.id")

  # merge the shopping and purchase history
  shopping [ purchase, `:=` (option.a = option.a, 
                             option.b = option.b,
                             option.c = option.c,
                             option.d = option.d,
                             option.e = option.e,
                             option.f = option.f,
                             option.g = option.g )]
  
  return (shopping)
}
