require(plotly)
# coordinates = scripts$load_ecog_coordinates('subject_APX1604')
# channels = 1:240
generate_mesh3d = function(coordinates, channels, selected){
  
  # Setting camera
  camera = list(
    up = list(x = 0, y = 0, z = 1),
    center = list(x = 0, y = 0, z = 0),
    eye = list(x = 0.1, y = 0.1, z = 1.5)
  )
  
  # Settings xyz ranges
  xyz_scene = list(
    xaxis = list(range = range(coordinates$X) + c(-1,1) * 1,
                 nticks = 4),
    yaxis = list(range = range(coordinates$Y) + c(-1,1) * 1,
                 nticks = 4),
    zaxis = list(range = range(coordinates$Z) + c(-1,1) * 1,
                 nticks = 4)
  )
  
  if(length(selected) == 0){
    selected = FALSE
  }
  
  data = data.frame(
    coordinates,
    Channels = str_c(channels, ' (#', 1:length(channels), ')'),
    Mask = selected,
    stringsAsFactors = F
  )
  
  plot_ly(x = ~X, y = ~Y, z = ~Z, data = data) %>% 
    add_trace(type = 'scatter3d',
              color = ~Mask,
              hoverinfo = 'skip',
              mode = 'markers',
              colors = c("#0D0887FF", "#F0F921FF"),
              showlegend = F
    ) %>% 
    add_text(text = ~ifelse(Mask, Channels, ''),
             textfont = list(
               family = "sans serif",
               size = 14,
               color = toRGB("grey20")
             ),
             hoverinfo = 'skip',
             showlegend = F,
             textposition = "top right") %>% 
    layout(
      scene = c(
        list(
          camera = camera
        ),
        xyz_scene
      )
      
    )
  
  
}