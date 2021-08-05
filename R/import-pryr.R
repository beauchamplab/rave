env_address <- function(x){
  ns <- asNamespace('dipsaus')
  ns$object_address(x)
  # if(requireNamespace('pryr', quietly = TRUE)){
  #   f <- pryr::address
  # }else{
  #   f <- function(x){
  #     attr = attributes(x)
  #     on.exit({
  #       attributes(x) = attr
  #     })
  #     e = utils::capture.output(print(structure(x, class = 'environment')))
  #     s = ''
  #     p = 'environment: ([0-9a-fx]+)';
  #     if(length(e) >= 1 && any(stringr::str_detect( e, p ))){
  #       e = e[stringr::str_detect( e, p )];
  #       s = stringr::str_match(e, 'environment: ([0-9a-fx]+)')[1,2]
  #     }
  #     
  #     return(s)
  #   }
  # }
  # f(x)
}


print.bytes <- function(x, digits = 3, ...){
  power <- min(floor(log(abs(x), 1000)), 4)
  if (power < 1) {
    unit <- "B"
  }
  else {
    unit <- c("kB", "MB", "GB", "TB")[[power]]
    x <- x/(1000^power)
  }
  formatted <- format(signif(x, digits = digits), big.mark = ",",
                      scientific = FALSE)
  cat(formatted, " ", unit, "\n", sep = "")
}

show_bytes <- function(x){
  structure(x, class = "bytes")
}

node_size <- function(){
  bit <- 8L * .Machine$sizeof.pointer
  if (!(bit == 32L || bit == 64L)) {
    stop("Unknown architecture", call. = FALSE)
  }
  if (bit == 32L)
    28L
  else 56L
}


mem_used <- function(){
  mem = dipsaus::mem_limit2()
  show_bytes(mem$total - mem$free)
}



object_size <- function(..., env = parent.frame()){
  if( dipsaus::package_installed('lobstr') ){
    f <- lobstr::obj_size
  }else{
    f <- function(..., env){
      sapply(list(...), function(x){
        utils::object.size(x)
      }) ->
        sizes
      
      show_bytes(sum(sizes))
    }
    
  }
  f(..., env = env)
}
