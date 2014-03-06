
############################################################################ 
# Executes an expression 'expr' and caches the result under the name 
# 'cache.name'. If the expression result has already been cached, it will be 
# returned from the cache instead of re-calculated.  This can save significant 
# time for functions that don't need to always be re-run. 
############################################################################
cache <- function (cache.name, 
                   expr, 
                   cache.dir         = getOption("cache.dir", default = ".cache"), 
                   cache.ignore      = getOption("cache.ignore", default = F),
                   cache.compress    = getOption("cache.compress", default = T),
                   on.missing.result = getOption("on.missing.result", default = warning)) {
  
  result <- NULL
  cache.file <- sprintf ("%s/%s.rds", cache.dir, cache.name)
  
  # has the result already been cached?
  if (file.exists (cache.file) && !cache.ignore) {
    logdebug ("Found '%s' cached as '%s'", cache.name, cache.file)
    result <- readRDS(cache.file)
    
  } else {
    # eval the expression 
    logdebug ("'%s' has NOT been cached or is being ignored", cache.name)
    result <- eval(expr)
    
    # sanity check, just in case
    if(0 == length(result))
      on.missing.result ( sprintf ("attempting to cache an empty result: '%s'", cache.name))
    
    # create the cache directory, if necessary
    dir.create (path = cache.dir, showWarnings = FALSE)
    
    # cache the result
    logdebug ("Cacheing '%s' as '%s'", cache.name, cache.file)
    saveRDS (result, cache.file, compress = cache.compress)
  }
  
  return(result)
}
