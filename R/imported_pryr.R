env_address <- function(x){
  f = get_from_package(f = 'address', pkg = 'pryr', ifNotFound = function(x){
    attr = attributes(x)
    on.exit({
      attributes(x) = attr
    })
    e = capture.output(print(structure(x, class = 'environment')))
    s = ''
    p = 'environment: ([0-9a-fx]+)';
    if(length(e) >= 1 && any(stringr::str_detect( e, p ))){
      e = e[stringr::str_detect( e, p )];
      s = stringr::str_match(e, 'environment: ([0-9a-fx]+)')[1,2]
    }

    return(s)
  })

  f(x)
}


print.bytes <- function(x, digits = 3, ...){
  f = get_from_package(f = 'print.bytes', pkg = 'pryr', internal = TRUE, ifNotFound = function (x, digits = 3, ...)
  {
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
  })

  f(x, digits, ...)
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
  f = get_from_package(f = 'mem_used', pkg = 'pryr', ifNotFound = function (){
    show_bytes(sum(gc()[, 1] * c(node_size(), 8)))
  })
  f()
}



object_size <- function(..., env = parent.frame()){
  f = get_from_package(f = 'object_size', pkg = 'pryr', ifNotFound = function (..., env){

    sapply(list(...), function(x){
      object.size(x)
    }) ->
      sizes

    show_bytes(sum(sizes))
  })
  f(..., env = env)
}
