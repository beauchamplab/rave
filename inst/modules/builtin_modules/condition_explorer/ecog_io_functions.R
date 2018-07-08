require(magrittr)
require(stringr)

`set_if_null<-` <- function(x, values) {
    if(is.null(x)) return(values)
    return (x)
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
            faces_per_electrode=42, AFNI_PATH = '~/abin/') {

    niml_fname <- prefix %&% '_' %&% str_replace_all(Sys.time(), '\\ |:', '_') %&% '.niml.dset'

    # get useful defaults
    # if value_labels weren't passed in, maybe there are column names?
    set_if_null(value_labels) <- colnames(values_matrix)
    # if these are still null, then just set an increasing number
    set_if_null(value_labels) <- 'Val_' %&% (1:ncol(values_matrix))

    # if electrode_numbers weren't passed in, maybe there are row.names?
    set_if_null(electrode_numbers) <- row.names(values_matrix)

    # if electrode numbers are present, ensure they are all integer values
    if(!all(is.integer(electrode_numbers))) electrode_numbers <- 1:nrow(values_matrix)

    # adding the electrode number as a column is a nice thing to do
    if(add_electrodes_as_column){
        values_matrix <- cbind(electrode_numbers, values_matrix)
        value_labels <- c('e#', value_labels)
    }

    # duplicate indices and values to match #faces in SUMA spheres
    indices <- rep(electrode_numbers, each=faces_per_electrode)
    values <- values_matrix[rep(1:nrow(values_matrix), each=faces_per_electrode),]

    # Turn the electrode ID into an ascending vertex ID,
    # this is aware that electrode numbers may not be sequential and AFNI starts at 0
    indices = (indices - 1) * faces_per_electrode + seq(0, faces_per_electrode - 1)

    # write out the values and indicies files
    mapply(function(x, file) {
        write.table(x, file, row.names=FALSE, col.names=FALSE)
    },list(values, indices), c(value_file, index_file))

    cmd <- sprintf("%s/ConvertDset -o_niml -input %s -i_1D -node_index_1D %s -dset_labels '%s' -prefix %s",
                    AFNI_PATH, value_file, index_file, paste0(value_labels, collapse=' '), niml_fname)

    attr(cmd, which = 'path') <- niml_fname

    print('For full cleanup, AFTER running the ConvertDset command, delete: ' %&% value_file %&% ' and ' %&% index_file)

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

