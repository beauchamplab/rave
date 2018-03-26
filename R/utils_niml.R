`set_if_null<-` <- function(x, values) {
  if(is.null(x)) return(values)
  return (x)
}


#' @export
`%&%` <- function(s1,s2) paste0(s1,s2)

# check for containment, by default containiment is inclusive of the boundaries: min(rng) <= x <= max(rng)
# specificy your own functions for lower bound and upper bound if you want
# works with vector-valued x for default LB_ and UB_ functions
# also works when length(rng) == 1
is_within <- function(x, minmax) (x>=minmax[1]) & (x<=minmax[2])

#' @export
`%within%` <- function(a,b) {
  is_within(a,b)
}


#
# EXAMPLE USAGE
#
# write.niml(matrix(1:10, nrow=5), 1:5, c('a', 'b'), 'test') %>% system
#
# if your matrix already has column names and row names (which should be integer) the function
# will use them


# this function returns a command that needs to be run in order to actually create
# the niml file. This is done to allow parallel creation of the ConvertDset scripts and then
# batch execution of the commands and handles path issues that sometimes result from calilng
# system() within R
write.niml <- function(values_matrix, electrode_numbers=NULL, value_labels=NULL, prefix='', add_electrodes_as_column=TRUE,
                       value_file='__vals.dat', index_file='__ind.dat',
                       work_dir = './', ...) {

  AFNI_PATH = try_normalizePath(rave_options('suma_path'))
  faces_per_electrode = rave_options('suma_nodes_per_electrodes')

  fname = prefix %&% '_' %&% str_replace_all(Sys.time(), '\\ |:', '_')
  niml_fname <- fname %&% '.niml.dset'
  csv_fname = fname %&% '.csv'

  # get useful defaults
  # if value_labels weren't passed in, maybe there are column names?
  set_if_null(value_labels) <- colnames(values_matrix)
  # if these are still null, then just set an increasing number
  set_if_null(value_labels) <- 'Val_' %&% (1:ncol(values_matrix))

  # if electrode_numbers weren't passed in, maybe there are row.names?
  if(length(electrode_numbers) == 0){
    electrode_numbers = 1:nrow(values_matrix)
  }else{
    electrode_numbers = as.numeric(electrode_numbers)
  }

  # adding the electrode number as a column is a nice thing to do
  if(add_electrodes_as_column){
    values_matrix <- cbind(electrode_numbers, values_matrix)
    value_labels <- c('e#', value_labels)
  }

  # duplicate indices and values to match #faces in SUMA spheres
  indices <- rep(electrode_numbers, each=faces_per_electrode)
  values <- values_matrix[rep(1:nrow(values_matrix), each=faces_per_electrode),]

  values = matrix(as.numeric(values), ncol = length(value_labels), byrow = F)

  # Turn the electrode ID into an ascending vertex ID,
  # this is aware that electrode numbers may not be sequential and AFNI starts at 0
  indices = (indices - 1) * faces_per_electrode + seq(0, faces_per_electrode - 1)

  # write out the values and indicies files
  if(!dir.exists(work_dir)){
    dir.create(work_dir, recursive = T)
  }
  work_dir = try_normalizePath(work_dir)
  value_file = file.path(work_dir, value_file)
  index_file = file.path(work_dir, index_file)
  niml_fname = file.path(work_dir, niml_fname)
  csv_fname = file.path(work_dir, csv_fname)

  write.csv(values_matrix, csv_fname, row.names = F)

  mapply(function(x, file) {
    write.table(x, file, row.names=FALSE, col.names=FALSE)
  },list(values, indices), c(value_file, index_file))

  cmd <- sprintf("%s/ConvertDset -o_niml -input %s -i_1D -node_index_1D %s -dset_labels '%s' -prefix %s",
                 AFNI_PATH, value_file, index_file, paste0(value_labels, collapse=' '), niml_fname)

  attr(cmd, which = 'path') <- niml_fname

  logger('For full cleanup, AFTER running the ConvertDset command, delete: ' %&% value_file %&% ' and ' %&% index_file)

  print(list(
    args = c(
      '-o_niml', '',
      '-input', sprintf('"%s"', value_file),
      '-i_1D -node_index_1D', sprintf('"%s"', index_file),
      '-dset_labels', sprintf("'%s'", paste0(value_labels, collapse=' ')),
      '-prefix', sprintf('"%s"', niml_fname)
    ),
    env = c(sprintf('PATH=$PATH:"%s"', AFNI_PATH),
            'DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace')
  ))
  system2(
    'ConvertDset',
    args = c(
      '-o_niml', '',
      '-input', sprintf('"%s"', value_file),
      '-i_1D', '',
      '-node_index_1D', sprintf('"%s"', index_file),
      '-dset_labels', sprintf("'%s'", paste0(value_labels, collapse=' ')),
      '-prefix', sprintf('"%s"', niml_fname)
    ),
    env = c(sprintf('PATH=$PATH:"%s"', AFNI_PATH),
            'DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace'),
    wait = F
  )

  return (cmd)
}

# helper function to build value labels
format_stat <- function(nm, stats=c('b', 't', 'p')) {
  sapply(stats, function(stat) sprintf('%s(%s)', stat, nm), USE.NAMES = FALSE)
}

get_f <- function(formula, data) {
  format_f(lm(formula, data))
}

format_f <-  function(lm.mod, test_name='All') {
  nms <- sapply(c('Rsq(%s)', 'F(%s)', 'p(%s)'), sprintf, test_name)

  with(summary(lm.mod), {
    c(r.squared, fstatistic[1],
      pf(fstatistic[1], fstatistic[2], fstatistic[3], lower.tail=FALSE))
  }) %>% set_names(nms) %>% `class<-`('fres')
}

# relying on a generic here
pretty.fres <- function(fres) {
  # don't save intermediate results back into fres or else it changes the type into character,
  # messing up following lines
  c(
    # R2
    ifelse(fres[1] < 0.01, '<0.01', round(fres[1],2)),
    #F stat
    ifelse(fres[2] < 0.1, '<0.1', round(fres[2],1)),
    #p value
    format(fres[3], digits=1)
  )
}

# helper function for t-tests that returns the values wanted by format_stat
get_t <- function(...) with(t.test(...), c(estimate, statistic, p.value)) %>% `class<-`('tres')

pretty.tres <- function(tres) {
  mapply(format, tres, digits=c(2,2,1)) %>%
    set_names(c('m', 't', 'p'))
}





# By Dipterix
launch_suma <- function(
  root_dir, spec_file
){
  if(missing(spec_file)){
    spec_file = rave_options('suma_spec_file')
  }
  # make everything absolute
  suma_path = try_normalizePath(rave_options('suma_path'))
  spec_file = try_normalizePath(file.path(root_dir, spec_file))

  wd = getwd()
  on.exit({setwd(wd)})
  if(dir.exists(root_dir)){
    setwd(root_dir)
    system2('suma',
            args = c('-spec', sprintf('"%s"', spec_file)),
            env = c(sprintf('PATH=$PATH:"%s"', suma_path),
                    'DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace'),
            wait = F)
  }

  setwd(wd)
}

