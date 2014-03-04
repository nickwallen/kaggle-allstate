

#
# according to the kaggle-allstate competition rules, the scoring
# is all-or-nothing.  all of the options a-g must be predicted correctly
# to receive credit.
#
accuracy.score <- function (data) {
  
  # collapse the set of options [a-g] into a single 'product'
  add.product (data)
  add.product.hat (data)
  
  # each of the options must be predicted accurately to receive credit
  data [ product == product.hat, score := 1]
  data [ product != product.hat, score := 0]
  
  # the percentage of correct predictions in the data
  accuracy <- sum (data$score) / nrow (data)
}

