#' @import stringr
#' @export
make_module <- function(
  name, script_path,
  module_id = paste0(sample(c(LETTERS, letters)), collapse = ''),
  launch = F
){
  s = readLines(script_path)

  named_starts = str_detect(s, '^```\\{r rave_[\\w]+[,\\}]')
  all_starts = str_detect(s, '^```\\{r')

  ends = str_detect(s, '^```[\\ ]*$')

  # s[named_starts]
  # s[all_starts]

  named_ind = which(named_starts)
  start_ind = which(all_starts)
  end_ind = which(ends)

  chund_ind = cbind(start_ind,end_ind)[start_ind %in% named_ind,]

  apply(chund_ind, 1, function(x){
    if(x[2] - x[1] > 2){
      seq(x[1] + 1, x[2] - 1)
    }else{
      NULL
    }
  }) ->
    chunks

  script = s[sort(unlist(chunks))]

  require(rave)
  module = ModuleEnvir$new(module_id = module_id, label_name = name,
                           .script_content = script, rmd_path = script_path)
  if(launch){
    init_app(list(module))
  }

  return(module)

}
