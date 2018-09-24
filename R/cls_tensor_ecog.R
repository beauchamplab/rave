#' @export
ECoGTensor <- R6::R6Class(
  classname = 'ECoGTensor',
  inherit = Tensor,
  public = list(
    flatten = function(include_index = T, value_name = 'value'){
      nrow = prod(self$dim)
      re = data.frame(V = as.vector(self$get_data()))
      names(re) = value_name
      if(include_index){
        for(i in 1:length(self$varnames)){
          vn = self$varnames[i]
          if(i > 1){
            each = prod(self$dim[1: (i - 1)])
          }else{
            each = 1
          }
          times = nrow / self$dim[i] / each

          re[[vn]] = rep(self$dimnames[[i]], each = each, times = times)
          if(i == 1){
            re[['Trial_Number']] = rep(1:self$dim[1], each = 1, times = times)
          }
        }
        re = cbind(re[-1], re[1])
      }
      re
    },
    initialize = function(data, dim, dimnames, varnames, hybrid = F, swap_file = tempfile()){

      # get attributes of data
      dim %?<-% base::dim(data)
      dim %?<-% length(data)
      dimnames %?<-% base::dimnames(data)
      dimnames %?<-% lapply(1:length(varnames), function(v){ seq_len(dim[v]) })

      names(dimnames) = varnames

      self$last_used = Sys.time()
      self$dimnames = dimnames
      self$dim = dim
      private$.data = data

      rm(data)

      tryCatch({
        if('Frequency' %in% varnames){
          self$dimnames$Frequency = as.numeric(self$dimnames$Frequency)
        }
      }, error = function(e){})
      tryCatch({
        if('Time' %in% varnames){
          self$dimnames$Time = as.numeric(self$dimnames$Time)
        }
      }, error = function(e){})
      tryCatch({
        if('Electrode' %in% varnames){
          self$dimnames$Electrode = as.numeric(self$dimnames$Electrode)
        }
      }, error = function(e){})


      self$hybrid = hybrid
      self$use_index = T

      self$swap_file = swap_file

      # to_swap
      if(hybrid){
        self$to_swap_now(use_index = T, swap_file = swap_file)
      }
    }
  )
)


