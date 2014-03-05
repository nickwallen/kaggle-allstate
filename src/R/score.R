

#
# according to the kaggle-allstate competition rules, the scoring
# is all-or-nothing.  all of the options a-g must be predicted correctly
# to receive credit.
#
accuracy.score <- function (data) {

  # sanity check - expect all of the option fields present
  if (any (c(options(), options.hat()) %nin% names(data))) {
    stop ("missing option fields in the input")
  }
  
  # collapse the set of options [a-g] into a single 'product'
  add.product (data)
  add.product.hat (data)
  
  # each of the options must be predicted accurately to receive credit
  data [ product == product.hat, score := 1]
  data [ product != product.hat, score := 0]
  
  # the percentage of correct predictions in the data
  accuracy <- sum (data$score) / nrow (data)
  return (accuracy)
}

#
# calculates a partial accuracy score.  credit is given for each option
# that is predicted correctly.  this differs from the competition's all or
# nothing scoring.  this partial scoring mechanism may help in refining or tuning
# models.
#
partial.accuracy.score <- function (data) {
  require (reshape2)
  
  # sanity check - expect all of the option fields present
  if (any (c(options(), options.hat()) %nin% names(data))) {
    stop ("missing option fields in the input")
  }

  # fix up factors - TODO ugly; there has to be a better way (and place) to do this
  data [, `:=` (
    option.a.hat = factor (option.a.hat, levels = levels (option.a)),
    option.b.hat = factor (option.b.hat, levels = levels (option.b)),
    option.c.hat = factor (option.c.hat, levels = levels (option.c)),
    option.d.hat = factor (option.d.hat, levels = levels (option.d)),
    option.e.hat = factor (option.e.hat, levels = levels (option.e)),
    option.f.hat = factor (option.f.hat, levels = levels (option.f)),
    option.g.hat = factor (option.g.hat, levels = levels (option.g))
  )]
  
  # the partial score is the sum of all correctly predicted options
  data [, partial.score := 
          (option.a == option.a.hat) + 
          (option.b == option.b.hat) + 
          (option.c == option.c.hat) +           
          (option.d == option.d.hat) + 
          (option.e == option.e.hat) + 
          (option.f == option.f.hat) + 
          (option.g == option.g.hat) 
        ]
  
  # the percentage of correct predictions in the data
  accuracy <- sum (data$partial.score) / (nrow (data) * 7)
  return (accuracy)
}
