# UI for 3d viewer
input %?<-% getDefaultReactiveInput()
session %?<-% getDefaultReactiveDomain()
output %?<-% getDefaultReactiveOutput()

local_data = reactiveValues(
    viewer_data = NULL,
    summary_data = NULL
)

viewer_3d = function(){
    # UI
    fluidRow(
        div(
            class = 'col-sm-12 col-md-3',
            textOutput(ns('3d_summary')),
            uiOutput(ns('3d_params')),
            actionButton(ns('3d_generate'), 'Calculate Data')
        ),
        div(
           class = 'col-sm-12 col-md-9',
            div(style = 'float:right; width: 50px; position: absolute; right: 20px;',
                plotOutput(ns('3d_color_bar'), height = '60vh')),
            threejsr::threejsOutput(ns('3d_output'), height = '60vh')
        )
    )

}

observeEvent(input[['3d_generate']], {
    # generate summary

    # .prog <- progress('Getting analysis for all electrodes', max=length(preload_info$electrodes))
    #
    # on.exit({.prog$close()})
    # .prog$inc()

    # showNotification(p())

    dat <- get_summary()

    # for each electrode, we want to test the different conditions
    .FUN <- if(length(levels(dat$condition)) > 1) {
      function(x) {
        get_f(power ~ condition, data=x)
      }
    } else {
      function(x) {
        get_t(x$power) %>% set_names(c('b', 't', 'p'))
      }
    }

    res <- sapply(dat %>% split((.)$elec), .FUN) %>% t

    # call get Summary then fill datt

    local_data$viewer_data = res
    local_data$summary_data = list(
        baseline = BASELINE,
        frequencies = FREQUENCY
    )
})

output[['3d_summary']] <- renderText({
    if(is.null(local_data$summary_data)){
        return('Please press "Calculate Data" button to generate summaries.')
    }else{
        sprintf('Electrodes (%s), with baseline %.1f - %.1f (s) and frequency range %.0f - %.0f (Hz)',
                deparse_selections(preload_info$electrodes),
                BASELINE[1],BASELINE[2],
                FREQUENCY[1], FREQUENCY[2])
    }
})

output[['3d_params']] <- renderUI({
    if(!is.null(local_data$viewer_data)){
        vars = colnames(local_data$viewer_data)
        tagList(
            selectInput(ns('param_displ'), 'Display Variable', choices = vars)
        )
    }
})

output[['3d_output']] <- threejsr::renderThreejs({
    if(!is.null(local_data$viewer_data)){
        dat = local_data$viewer_data
        vars = colnames(dat)
        marker = paste0(
            apply(dat, 1, function(x) {
                paste(sprintf('%s: %.4f', vars, x), collapse = '<br />')
            })
        )

        center = 0
        displ = input$param_displ
        # if(displ == 'p'){
        #     center = 1
        # }
        module_tools$plot_3d_electrodes(
            electrodes = preload_info$electrodes,
            values = dat[,displ == vars],
            marker = marker,
            control_gui = T,
            center = center
        )

        # power = module_tools$get_power()
        # power = baseline(power, -1, 0)
        # val = power$subset(Frequency = Frequency %in% 75:150)$collapse(keep = c(3,4), method = 'mean')
        # module_tools$plot_3d_electrodes(
        #     electrodes = preload_info$electrodes,
        #     values = val,
        #     key_frame = preload_info$time_points,
        #     control_gui = T
        # )
    }
})



output[['3d_color_bar']] <- renderPlot({
  k=100
  pal = colorRampPalette(c('navy', 'white', 'red'))(k)
  par(mar = rep(0.25,4))
  image(matrix(1:k, nrow=1), col = pal, useRaster = T)
})




