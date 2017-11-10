# Example 1: simple usage

use_multicores(20)              # Use multi-cores


worker <- run_script({
  Sys.sleep(2)                  # Wait for 3 seconds to execute next line
  print(sprintf("[%s] The script doesn't block!", toString(Sys.time())))
})

print('Other code')

print(worker$check())           # FALSE
print(worker$result)            # Not yet evaluated


# Now wait 3 seconds
Sys.sleep(2)

print(worker$check())           # TRUE
print(worker$result)            # Evaluated Result


# Example 2: With shiny app


use_multicores(20) # Switch to use_singlecore() and try again

library(shiny)

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(
        width = 12,
        verbatimTextOutput('result')
      )
    )
  ),

  server = function(input, output, session){


    timer <- reactiveTimer(20)

    result <- reactiveValues(
      text = NULL,
      text_other = NULL,
      executed = FALSE
    )


    script = function(){

      # Code Block 1
      result$text = c(result$text, sprintf("Code Block 1 - [%s] The script that blocks", toString(Sys.time())))

      # Code Block 2
      tmp_file <- tempfile()

      worker <- run_script({
        write(sprintf('Code Block 2 Started at [%s]', toString(Sys.time())), file = tmp_file, append = T)
        for(i in 1:10){
          Sys.sleep(1)
          write(sprintf('Current Iteration: %d, result: %.2f', i, rnorm(1)), file = tmp_file, append = T)
        }
        write('Done', file = tmp_file, append = T)
        Sys.sleep(1)
        sprintf("Code Block 2 - [%s] The long script doesn't block!", toString(Sys.time()))
      })

      # Code Block 3
      result$text = c(result$text, sprintf("Code Block 3 - [%s] The script that blocks", toString(Sys.time())))


      local({
        tmp_observer <- observe({
          timer()
          worker$check()
          if(worker$check() == TRUE){
            result$text = c(result$text, worker$result)
            unlink(tmp_file)
            tmp_observer$destroy()
          }else{
            result$text_other <- readLines(tmp_file)
          }
        })
      })
      return(result)
    }




    # Register output
    output$result <- renderPrint({

      # first time running code
      if(!isolate(result$executed)){
        result$executed = TRUE
        worker = script()
      }

      cat(result$text, sep = '\n')
      cat('\nBlock 2 callbacks\n')
      cat(result$text_other, sep = '\n')
    })
  }
)
