
#
# the naive shopping model simply chooses the options (aka plan) that the customer
# most recently shopped for.  this serves as the published benchmark for the 
# competition.
#
# the model is built using caret's custom model "plugin" infrastructure.  this 
# should make it simple to compare, contrast, ensemble, etc using functionality 
# that already exists in caret.
#
naive.shopping.model <- list (
  
  type       = "Classification",
  library    = "data.table",
  loop       = NULL,
  prob       = NULL,
  grid       = function(x, y, len = NULL) NULL,
  parameters = data.frame(parameter=c("option"), class=c("character"), label=c("option")),
  
  #
  # no training required for the naive model
  #
  fit = function (x, y, wts, param, lev, last, weights, classProbs, ...) {
    
    # there is no training required, but we do need to know which option to predict
    return (list (option = as.character (param [1, 1])))    
  },
  
  #
  # predict the future!
  #
  predict = function (modelFit, newdata, preProc = NULL, submodels = NULL) {
    
    # the model tells us which option to predict
    option <- modelFit$option
    
    # TODO - 'latest quote' search takes ~30 secs; is there a faster way?
    d <- as.data.table (newdata)
    setkeyv (d, c("record.type", "customer.id", "shopping.pt"))
    
    # find the most recent shopping record (quote) for each customer
    latest.quote <- d [ record.type == "shopping", 
                           .SD [ which.max (shopping.pt), option, with = FALSE ], 
                           by = customer.id ]
    
    # return the specific option that was predicted
    return (latest.quote [[ option ]])
  }
)

#
# an implementation of the naive model for the competition.  this model simply chooses
# the options that the customer last shopped for.
#
export.naive.model <- function (data = fetch (train = FALSE), 
                                file = "../../submissions/red-swingline-predictions-naive.csv") {
  
  # create a naive model for each option
  models <- lapply (options(), function (option) {
    train (
      method    = naive.shopping.model, 
      trControl = trainControl (method = "none"),
      y         = data [[ option ]],
      x         = data,
      tuneGrid  = data.frame (option = option))
  })
  names(models) <- options.hat()
  
  # each option [a-g] has its own prediction model; caret does not support multi-output models
  predictions <- lapply (options.hat(), function (option.hat) {
    predict (models [[option.hat]], data )
  })
  names(predictions) <- options.hat()
  
  # label the predictions with the customer id
  predictions$customer.id <- unique (data$customer.id)
  
  # create a submission file that can be uploaded to kaggle
  create.submission (as.data.table (predictions), file)
}
