


#
# the popular model simply predicts the most popular shopped for or purchased options for 
# each customer and every customer.  this can be used as an alternative 'naive' model
# for model comparison.
#
# the model is built using caret's custom model "plugin" infrastructure.  this 
# should make it simple to compare, contrast, ensemble, etc using functionality 
# that already exists in caret.
#
popular.shopping.model <- list (
  
  type       = "Classification",
  library    = "data.table",
  loop       = NULL,
  prob       = NULL,
  grid       = function(x, y, len = NULL) NULL,
  parameters = data.frame(parameter=c("option"), class=c("character"), label=c("option")),
  
  #
  # trains the popular model.  the model simply finds the most popular option choice
  # amongst the data.
  #
  fit = function (x, y, wts, param, lev, last, weights, classProbs, ...) {

    # find the most popular option in the training data
    most.popular <- sort (table (y), decreasing = TRUE) [1]
    
    # there is no training required, but we do need to know which option to predict
    return (list (option       = as.character (param [1, 1]), 
                  most.popular = most.popular))   
  },
  
  #
  # predict the future!
  #
  predict = function (modelFit, newdata, preProc = NULL, submodels = NULL) {
    
    # the model tells us which option to predict
    option       <- modelFit$option
    most.popular <- names (modelFit$most.popular)
    
    # a prediction is needed for each customer
    pred.count <- length (unique (newdata$customer.id))
    
    # predict the most popular option for each customer
    outcomes <- rep (most.popular, pred.count)
  }
)

#
# trains the popular model given a set of training data
#
train.popular.model <- function (data) {
  require ("caret")
  
  # create a naive model for each option
  models <- lapply (options(), function (option) {
    train (
      method    = popular.shopping.model, 
      trControl = trainControl (method = "none"),
      y         = data [[ option ]],
      x         = data,
      tuneGrid  = data.frame (option = option))
  })
  names(models) <- options.hat()
  
  return (models)
}

#
# makes predictions for all of the options using the popular model.
#
predict.popular.model <- function (models, predictors) {
  require ("caret")
  
  # each option [a-g] has its own prediction model; caret does not support multi-output models
  outcomes <- lapply (options.hat(), function (option.hat) {
    predict (models [[option.hat]], predictors )
  })
  names(outcomes) <- options.hat()
  
  # label the outcomes with the customer id
  #outcomes$customer.id <- unique (predictors$customer.id)
  
  return ( as.data.table (outcomes))
}

#
# trains, predicts and creates a competition submission file for the popular model 
# used as a benchmark for the competition.  this function uses only the competition data
# set.  
#
export.popular.model <- function (file = "../../submissions/red-swingline-predictions-popular.csv") {
  
  # when preparing the model for submission, only the competition test set is needed
  data <- fetch (train = FALSE)
  
  # train
  models <- train.popular.model (data)
  
  # predict
  predictors <- data [, non.options (data), with = FALSE]
  outcomes <- predict.popular.model (models, predictors)
  
  # submit
  create.submission (outcomes, file)
}
