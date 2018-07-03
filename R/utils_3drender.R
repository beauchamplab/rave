




render_3d_electrodes <- function(
  tbl, loaded_electrodes = NULL, values = NULL,
  cont_pal = colorRampPalette(colors = c('navy', 'black', 'red')), resolution = 1001
){

  resolution = floor(resolution / 2) * 2 + 1
  pal = cont_pal(resolution)


  loaded = tbl$Electrode %in% loaded_electrodes

  # case 1: length(values) == 0
  if(length(values) == 0){
    cols = rep(pal[(resolution + 1) / 2], length(loaded))
  }else if(length(values) == sum(loaded)){
    cols = rep(pal[(resolution + 1) / 2], length(loaded))
    cols[loaded] = values
  }else{
    assertthat::assert_that(length(values) == length(loaded), msg = 'values must have the same length as number of electrodes')
    cols = values
  }

  lapply(seq_len(nrow(tbl)), function(ii){
    row = tbl[ii, ]
    mesh_name = sprintf('Electrode %d%s %s', row$Electrode,
                        ifelse(is.na(row$Label), '', sprintf(' (%s)', row$Label)),
                        ifelse(loaded[ii] && length(values[ii]) == 1 && !is.na(as.character(values[ii])), as.character(values[ii]), '')
                        )

    threejsr::GeomSphere$new(
      position = c(row$Coord_x, row$Coord_y, row$Coord_z),
      mesh_name = mesh_name,
      mesh_info = mesh_name,
      radius = 4,
      layer = 3
    ) ->
      g
    if(loaded[ii]){
      c = t(get_color(cols[ii])) / 255; colnames(c) = NULL
      g$add_event(event_type = 'position', name = 'static', event_data = rbind(c,c), key_frames = 0:1)
    }
    g
  }) ->
    geoms
  threejsr:::threejs_scene.default(elements = geoms, control_gui = F)

}


get_color <- function(col){
  tryCatch({
    col2rgb(col)
  }, error = function(e){
    col2rgb(as.numeric(col))
  })
}
