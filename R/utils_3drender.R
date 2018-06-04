




render_3d_electrodes <- function(
  tbl, loaded_electrodes = NULL, values = NULL,
  cont_pal = c("#00008F", "#00009F", "#0000AF", "#0000BF", "#0000CF",
               "#0000DF", "#0000EF", "#0000FF", "#0010FF", "#0020FF",
               "#0030FF", "#0040FF", "#0050FF", "#0060FF", "#0070FF",
               "#0080FF", "#008FFF", "#009FFF", "#00AFFF", "#00BFFF",
               "#00CFFF", "#00DFFF", "#00EFFF", "#00FFFF", "#10FFEF",
               "#20FFDF", "#30FFCF", "#40FFBF", "#50FFAF", "#60FF9F",
               "#70FF8F", "#80FF80", "#8FFF70", "#9FFF60", "#AFFF50",
               "#BFFF40", "#CFFF30", "#DFFF20", "#EFFF10", "#FFFF00",
               "#FFEF00", "#FFDF00", "#FFCF00", "#FFBF00", "#FFAF00",
               "#FF9F00", "#FF8F00", "#FF8000", "#FF7000", "#FF6000",
               "#FF5000", "#FF4000", "#FF3000", "#FF2000", "#FF1000",
               "#FF0000", "#EF0000", "#DF0000", "#CF0000", "#BF0000",
               "#AF0000", "#9F0000", "#8F0000", "#800000")
){

  plotly::plot_ly(
    x = ~Coord_x,
    y = ~Coord_y,
    z = ~Coord_z,
    text = ~sprintf('%d - %s', Channel, ifelse(is.na(Label), 'No name', Label)),
    hoverinfo = 'none'
  ) ->
    p
#
#   if(sum(tbl$EpilepsyChan)){
#     p %>%
#       plotly::add_markers(
#         data = tbl[tbl$EpilepsyChan, ],
#         name = "Epilepsey",
#         mode = 'markers',
#         hoverinfo = 'text',
#         marker = list(
#           symbol = 'cross',
#           opacity = 0.5,
#           color = "#8DA0CB"
#         )
#       ) ->
#       p
#   }
#
#
#   if(sum((tbl$BadChan & !tbl$EpilepsyChan))){
#     p %>%
#       plotly::add_markers(
#         data = tbl[(tbl$BadChan & !tbl$EpilepsyChan), ],
#         name = "Bad Channels",
#         mode = 'markers',
#         marker = list(
#           symbol = 'cross',
#           opacity = 0.5,
#           color = "#B3B3B3"
#         )
#       ) ->
#       p
#   }

  # if(length(!(tbl$EpilepsyChan | tbl$BadChan | tbl$Channel %in% loaded_electrodes))){
    p %>% plotly::add_markers(
      data = tbl[!(tbl$EpilepsyChan | tbl$BadChan | tbl$Channel %in% loaded_electrodes), ],
      name = "Good Channels",
      mode = 'markers',
      hoverinfo = 'text',
      marker = list(
        symbol = 'circle-open',
        color = "#66C2A5"
      )
    ) ->
      p

  # }

  if(length(loaded_electrodes)){
    if(length(loaded_electrodes) == length(values)){
      p %>%
        plotly::add_markers(
          data = tbl[tbl$Channel %in% loaded_electrodes, ],
          name = "Good [loaded]",
          hoverinfo = 'text',
          text = ~sprintf('%d - %s\n%.2f', Channel, ifelse(is.na(Label), 'No name', Label), values),
          marker = list(
            symbol = 'circle',
            color = values,
            colors = cont_pal,
            showscale = T,
            colorbar = list(
              len = 0.5,
              ypad = 0,
              yanchor = 'top',
              title = 'Value',
              cauto = FALSE,
              cmax = ~max(abs(values)),
              cmin = ~(-max(abs(values))),
              autocolorscale = F
            )
          ),
          showlegend = T
        ) ->
        p
    }else{
      p %>%
        plotly::add_markers(
          data = tbl[tbl$Channel %in% loaded_electrodes, ],
          name = "Good [loaded]",
          mode = 'markers',
          hoverinfo = 'text',
          marker = list(
            symbol = 'circle',
            color = "#FC8D62"
          )
        ) ->
        p
    }
  }
  return(p)
}
