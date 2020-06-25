library(shiny)

# As an example, I created a
# function to check whether power is loaded, returns TRUE/FALSE
check_power <- function(referenced = TRUE, env = getDefaultDataRepository()){
  if(referenced){
    inherits(env$.private$repo$power, 'ECoGTensor')
  } else {
    inherits(env$.private$repo$raw_power, 'ECoGTensor')
  }
}


# Check whether the data is loaded
# Should sit inside of `define_initialization` of comp.R
rave_validate(expr = {
  
  # You can have any expr here 
  referenced = TRUE
  
  # rave_need captures TRUE/FALSE or error (FALSE) and returns TRUE/FALSE
  # If not TRUE captured, then rave_validate fails and RAVE pauses,
  # poping up modals
  has_power = rave_need(check_power(referenced))
  
  # Multiple `rave_need` is allowed, failing one invalidates the process
  
}, onfailing = function(opt, map, env = getDefaultDataRepository()){
  # opt is a list containing whatever variables created in `expr`
  # `map` is where you should save/read your data, 
  # I'll create a function getModuleMap('ravebuiltins') so that you can also
  # access to map in other places. map is shared across modules within package
  # other parameters must be specified with default
  
  # To debug, you can use opt = dipsaus::fastmap2()
  # and map = dipsaus::fastmap2()
  # opt$has_power = FALSE; opt$referenced = TRUE
  
  # Define a shiny module, we can prepare common ones
  # Here's something we need to consider:
  # Notice we can't use `project_name` or any global variables like we used as it's the first statement
  # in define_initialization()
  # We don't have module_tools since it might be missing (no data is loaded)
  # For the example below, I was using a trick because R 3.6 doesn't return error
  # when asking for NULL$aaa
  ui <- div('UI of choices',
            selectInput('project', 'Project', choices = get_projects(), 
                        selected = env$subject$project_name))
  
  # Observers MUST explicitly defined here they will be marked and destroyed once user hit "import"
  # See `Test` section below
  observe({ ... })
  
  # I'm not sure whether we need to return a list or just UI
  # we can return a list that might be useful to tell RAVE that
  # you could hide some components. For example, if
  # expectedloadingtime is NULL, then hide `Expected loading time` 
  # Need to think more on what UI can be customized
  return(list(
    # return UI, either function or HTML tags { }
    ui = ui,
    title = '',
    expectedloadingtime = 10,
    expectedloadingsize = 1024^2
  ))
}, import = function(opt, map){
  # opt will contain variables in `expr`, and variables created in UI
  # map is where you should save your data to
  # Use it as a global repository for the package (across modules but within one session)
  # One use case of map is you can create dynamically generated data here
  # Don't feed in function, that might cause memory leak
  # I would feed map with data needed by functions
  
})


# ------------------------ Test ---------------------------------
##### 1. Test to destroy shiny observer (not common) ####
shinyApp(fluidPage(
  actionButton('in1', 'Show message'),
  actionButton('in2', 'Destroy observer')
), function(input, output, session) {
  o = shiny::observeEvent(input$in1, {
    print(Sys.time())
  })
  shiny::observeEvent(input$in2, {
    o$destroy()
  })
}
)

# #### 2. NULL$aaa$bbb is still NULL. Some sneaky changes R made for 3.6 ####
NULL$aaa$bbb


# #### 3. memory leak test ####
pryr::mem_used()

# Case 1: variable created inside of function and returns function/environment
f1 <- (function(){
  x <- rnorm(1000000)
  function(){ length(x) }
})()
pryr::object_size(f1)
f1()
pryr::object_size(f1)



# case 2: variable declaired in params
f2 <- (function(x = rnorm(1000000)){
  function(){ length(x) }
})()

pryr::object_size(f2)
f2()
pryr::object_size(f2)



# case 3: use environment to control memory leak (only leak a small portion)
env <- new.env(); env$x <- rnorm(1000000)
f3 <- (function(){
  # `envir` will be leaked
  envir = env
  function(){ length(envir$x) }
})()

pryr::object_size(f3)
f3()
pryr::object_size(f3)

env$x = 1
pryr::object_size(f3)


# Case 4: save as case 3, but using function. 
# confirmed what's leaked is pointer of env instead of things inside of env
env <- list(); env$f <- f1
f4 <- (function(){
  envir = env
  function(){ envir$f() }
})()
pryr::object_size(env)
pryr::object_size(f4)
f4()
pryr::object_size(f4)
env$f = 1
pryr::object_size(f4)


# #### 4. Another memory leak example: ####
library(pryr)
gc()
start_mem <- mem_used()
start_time <- as.numeric(Sys.time())
for (i in 1:8) {
  cat(i, ": ", sep = "")
  print(mem_used())
  e <- new.env(parent = emptyenv())
  for (j in 1:10000) {
    # Generate random key
    x <- as.character(runif(1))
    exists(x, envir = e, inherits = FALSE)
  }
  rm(e, x)
}
end_time <- as.numeric(Sys.time())
gc()
end_mem <- mem_used()
cat("Elapsed time:", round(end_time - start_time, 1), "seconds\n")
cat("Memory leaked:", end_mem - start_mem, "bytes\n")


# this is why I use fastmap2
gc()
start_mem <- mem_used()
start_time <- as.numeric(Sys.time())
for (i in 1:8) {
  cat(i, ": ", sep = "")
  print(mem_used())
  e <- dipsaus::fastmap2()
  for (j in 1:10000) {
    # Generate random key
    x <- as.character(runif(1))
    is.null(e$x)
    # You can also use this to test x
    .subset2(e, 'has')(x)
  }
  rm(e, x)
}
end_time <- as.numeric(Sys.time())
gc()
end_mem <- mem_used()
cat("Elapsed time:", round(end_time - start_time, 1), "seconds\n")
cat("Memory leaked:", end_mem - start_mem, "bytes\n")

