



#' Function to get color without errors
#' @param col color name or number or code
#' @param alpha include alpha or not
get_color <- function(col, alpha = F){
  tryCatch({
    col2rgb(col, alpha = alpha)
  }, error = function(e){
    col2rgb(as.numeric(col), alpha = alpha)
  })
}
