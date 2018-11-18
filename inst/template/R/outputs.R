# File defines all the outputs

output_plot_function <- function(result){

  # Make sure all variables exists
  # You could remove the validation part since any error occur in this part won't
  # shut RAVE down. However, it's strongly recommended to have validation here
  # for aesthetic message display and for user experience (error message is misleading)

  validate(
    need(
      all(c('time_points', 'el_collapsed') %in% names(result)),
      message = 'No result yet. Try to use "cmd/ctrl + Enter" to re-run.'
    )
  )

  image(x = result$time_points, y = 1:nrow(result$el_collapsed), z = t(result$el_collapsed))
}



output_text_function <- function(result){

  validate(
    need(
      all(c('start_time', 'end_time') %in% names(result)),
      message = 'No result yet. Try to use "cmd/ctrl + Enter" to re-run.'
    )
  )

  rt = time_diff(result$start_time, result$end_time)
  text = sprintf('Total runtime: %.2f %s', rt$delta, rt$units)
  text
}


output_print_function <- function(result){

  validate(
    need(
      all(c('user_inputs') %in% names(result)),
      message = 'No result yet. Try to use "cmd/ctrl + Enter" to re-run.'
    )
  )

  cat("I'm printing some random number\n")
  print(rnorm(1))
  cat('Some user inputs\n')
  print(result$user_inputs)
}


output_table_function <- function(result){
  result %?<-% list()
  validate(need('epochs' %in% names(result), message = 'No result yet. Try to use "cmd/ctrl + Enter" to re-run.'))
  # Only shows the first 10 rows
  result$epochs[1:10, ]
}
