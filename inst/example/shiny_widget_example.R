# A example to write a temporary model

library(shiny)

# Set data directory
rafe_opts$set_options(
  data_dir = '/Users/beauchamplab/Dropbox/rafe/data/'
)

print(find_subject_ids()[1])                       # Will print all subject ID


module <- empty_module(
  subject_id = find_subject_ids()[1],              # pre-load data into memory for debug
  electrodes = 1:2
)

ls(module$runtime_env)


with(module$runtime_env, {

  require(magrittr)
  SHINY_INPUT = list(
    select_input('selected_electrode', 'Select Electrode', choices = '',
                 init = function(){
                   list(
                     choices = data_env$electrodes,
                     selected = get_local_var('selected_electrode')
                   )
                 }),
    section_input('tabset', 'Select Subset Type',
                  `Tab 1` = list(
                    numeric_input('number', 'Input a Number', value = 0),
                    slider_input('slider', 'Slider', min = 0, max = 1, value = 0, init = function(){
                      list(
                        value = runif(1)
                      )
                    })
                  ),
                  `Tab 2` = list(
                    checkbox_input('header', 'My data has header', value = TRUE, init = function(){
                      list(
                        value = get_local_var('header')
                      )
                    }),
                    file_input('file', 'Mask')
                  )
                ),
    action_button('btn', 'A Button')
  )

  SHINY_VALIDATE = function(params){
    tabset <- get_local_var('tabset')
    if((tabset == 'Tab 2') && is.null(get_local_var('file', NULL))){
      return(
        'No file Input!'
      )
    }else{
      return(NULL)
    }
  }


  SHINY_EXECUTE = function(params, ...){
    selected_electrode <- get_local_var('selected_electrode')
    tabset <- get_local_var('tabset', 'Tab 1')
    number <- get_local_var('number')
    slider <- get_local_var('slider')
    header <- get_local_var('header')
    file <- get_local_var('file')
    btn <- get_local_var('btn')
    mean_signal <- mean(data_env$data[,,,data_env$electrodes == selected_electrode])

    if(!is.null(file)){
      infile_dat = read.csv(file$datapath, header = header)
    }

    return(list(
      result_print = function(){
        cat('INPUTS:', '\nSelected Electrode: ', selected_electrode,
            '\nCurrent Input Tab: ', tabset, '\nButton Value: ', btn,
            '\n')


        switch(tabset,
               `Tab 1` = {
                 cat('Numeric Input: ', number, '\n')
                 cat('Slider Input: ', slider, '\n')
               },
               `Tab 2` = {
                 cat(sprintf('  (Reading a file %s headers)', ifelse(header, 'with', 'without')))
               })

        cat('\n\nVariables:\n')

        environment(SHINY_EXECUTE) %>%
          ls() ->
          var_names

        print(var_names)

        return(NULL)
      },
      result_table = function(){
        if(exists('infile_dat', environment())){
          return(infile_dat)
        }
      }
    ))
  }

  SHINY_OUTPUT = list(
    `Output Summary` = list(
      verbatimtext_output('result_print', 'Output:')
    ),
    `Infile Table` = list(
      datatable_output('result_table', 'Infile')
    )
  )
})




# Change Module

init_app(module$id)
