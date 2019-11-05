
# # Similar to dropNulls
# # @param x list to drop
# # @param deep iterativly apply this function
# # @param nulls see is_invalid
# dropInvalid <- function(x, deep = FALSE, nulls = c('')){
#   if(deep && is.list(x)){
#     x = lapply(x, dropInvalid, deep = deep, nulls = nulls)
#     x = dropNulls(x)
#   }else{
#     x = x[vapply(x, is_valid, FUN.VALUE = logical(1), nulls = nulls)]
#     if(length(x) == 0){
#       x = NULL
#     }
#   }
#   x
# }

# # Check if object is invalid
# # @param x object
# # @param nulls 'na' for is.na, '' for is.blank, 'Inf' for is.infinite, default is.null is also checked
# is_valid <- function(x, nulls = c('')){
#   if(
#     length(x) == 0 || is.null(x) ||
#     ('' %in% nulls && x == '') ||
#     ('NA' %in% nulls && length(x) == 1 && is.na(x)) ||
#     ('Inf' %in% nulls && is.numeric(x) && length(x) == 1 && is.infinite(x))
#   ){
#     return(FALSE)
#   }
#   return(TRUE)
# }
