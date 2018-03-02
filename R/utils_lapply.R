rave_lapply <- function(X, FUN, ..., .get_values = TRUE){
  lapply(X, function(x){
    rave::async({
      FUN(x, ...)
    }, plan = NULL)
  }) ->
    futures

  if(.get_values){
    futures = get_results(futures)
  }
  return(futures)
}

get_results <- function(futures){
  lapply(futures, future::value)
}
