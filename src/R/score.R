

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
  
  # collapse the set of options [a-g] into a single 'Plan'
  add.plan (data)
  add.plan.hat (data)
  
  # each of the options must be predicted accurately to receive credit
  data [ plan == plan.hat, score := 1]
  data [ plan != plan.hat, score := 0]
  
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
