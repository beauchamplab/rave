#' Convert text to numeric vector
#' @param text character like "1-10,13"
#' @param sep default is ','
#' @param sort sort result
#' @param unique remove duplicated value from result?
#' @export
parse_selections <- function(text, sep = '[, ]+', sort = F, unique = T){
  if(length(text) == 0 || str_trim(text) == ''){
    return(NULL)
  }

  if(is.numeric(text)){
    return(text)
  }
  s = as.vector(str_split(text, sep, simplify = T))
  s = str_trim(s)
  s = s[s!='']

  s = s[str_detect(s, '^[0-9-:~]+$')]

  re = NULL
  for(ss in s){
    if(str_detect(ss, '[-:~]')){
      ss = as.vector(str_split(ss, '[-:~]', simplify = T))
      ss = ss[str_detect(ss, '^[0-9]+$')]
      ss = as.numeric(ss)
      if(length(ss) >= 2){
        re = c(re, (ss[1]:ss[2]))
      }
    }else{
      re = c(re, as.numeric(ss))
    }
  }

  if(unique){
    re = unique(re)
  }

  if(sort){
    re = sort(re)
  }

  return(re)
}

#' Convert integer vector to text
#' @param nums integer vector
#' @param link character to concatenate
#' @param concatenate strings
#' @param max_lag how to define consecutive
#' @export
deparse_selections <- function(nums, link = '-', concatenate = T, max_lag = 1){
  if(length(nums) == 0){
    return('')
  }
  alag = 1:max(1, max_lag)
  nums = sort(unique(nums))
  lg = c(NA, nums)[1:length(nums)]
  ind = nums - lg; ind[1] = 0
  ind2 = c(ind[-1], -1)

  apply(cbind(nums[!ind %in% alag], nums[!ind2 %in% alag]), 1,function(x){
    if(x[1] == x[2]){
      str_c(x[1])
    }else{
      str_c(x, collapse = link)
    }
  }) ->
    re
  if(concatenate){
    re = str_c(re, collapse = ',')
  }
  re
}
