




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
                        ifelse(loaded[ii] && !is.na(as.character(values[ii])), as.character(values[ii]), '')
                        )

    threejsr::GeomSphere$new(
      position = c(row$Coord_x, row$Coord_y, row$Coord_z),
      mesh_name = mesh_name,
      mesh_info = mesh_name,
      radius = 4,
      layer = 2
    ) ->
      g
    if(loaded[ii]){
      c = as.vector(get_color(cols[ii])); names(c) = NULL
      g$animation_event(event_data = list(c),loop = T)
    }
    g
  }) ->
    geoms
  threejsr::threejs_scene(geoms)

}


get_color <- function(col){
  tryCatch({
    col2rgb(col)
  }, error = function(e){
    col2rgb(as.numeric(col))
  })
}
