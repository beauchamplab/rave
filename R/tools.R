#' @export
do_for_electrode <- function(X, FUN, ..., simplify = TRUE, quiet = TRUE){


  if(quiet){
    suppressMessages(capture.output({
      do.call('do_for_electrode', args = c(list(
        X = X,
        FUN = FUN,
        simplify = simplify,
        quiet = FALSE
      ), list(...))) ->
        results
      return(results)
    },
    type = c('output', 'message')))
    return(results)
  }


  X = as.data.frame(X)

  results <- data.frame()
  tmp_env <- new.env(parent = environment())

  for(subject_id in unique(X[,1])){

    subject <- prophet$get_subject(subject_id)
    electrodes <- unique(X[X[, 1] == subject_id, 2])
    electrodes = subject$filter_valid_electrodes(electrodes)

    sapply(electrodes, function(e){
      elec = SingleElectrode$new(subject, e)
      result <- FUN(elec$data, ...)
      elec$deactivate()
      list(
        subject_id = subject_id,
        electrode = e,
        result = result
      )
    }, USE.NAMES = T) %>%
      t() %>%
      as.data.frame() ->
      res
    results <- rbind(results, res)
  }

  if(simplify){
    tryCatch({
      plyr::ldply(results$result, function(x){
        x %>%
          unlist() %>%
          as.vector() %>%
          t() %>%
          as.data.frame()
      }) -> tmp
      return(cbind(results[, 1:2], tmp))
    }, error = function(e){
      warning('Your function should return a vector if you want to simplify the result')
      return(NULL)
    }) ->
      simp_res
    if(!is.null(simp_res)){
      results = simp_res
    }
  }

  return(results)
}


#' @export
`%for_e%` <- function(lhs, rhs){
  X = as.data.frame(lhs)
  expr <- eval(substitute(substitute(rhs)), parent.frame())

  FUN = function(data){
    eval(expr, envir = environment())
  }
  do_for_electrode(X, FUN)
}



# X = data.frame(
#   subject = rep('subject_lij118_ChBr', 3),
#   electrodes = 1:3
# )
#
# do_per_electrode(X, function(x){as.data.frame(t(x[1:10,1,1,1]))})
