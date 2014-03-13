
#
# sources all R files in the current working directory.  i wish there was a 
# better way to include all of the necessary files.
#
source.all <- function(path = getwd(), trace = TRUE, ...) {

  # for each file...
  for (file in list.files (path, pattern = "\\.[Rr]$")) {
    if(trace) {
      cat (sprintf ("sourcing %s...\n", file))
    }

    # source it!
    source (file.path (path, file), ...)
  }
}