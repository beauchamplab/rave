# Function names should be consistent with the output IDs
# will be used to render outputs

text_result <- function(result, ...){
  result$get_value('my_text_result')
}


plot_result <- function(result, ...) {
  pl <- result$get_value('my_plot_result')
  
  plot(pl$x, pl$y, type='l')
}

