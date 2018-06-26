




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

  col = rep(1, nrow(tbl));
  col[tbl$Electrode %in% loaded_electrodes] = 2

  rgbs = grDevices::col2rgb(col)

  lapply(seq_len(nrow(tbl)), function(ii){
    row = tbl[ii, ]
    rgb = rgbs[, ii]; names(rgb) = NULL
    threejsr::GeomSphere$new(
      position = c(row$Coord_x, row$Coord_y, row$Coord_z),
      mesh_name = sprintf('Electrode %d%s', row$Electrode, ifelse(
        is.na(row$Label), '', sprintf(' (%s)', row$Label)
      )),
      radius = 4,
      layer = 2
    ) ->
      g
    g$animation_event(event_data = list(rgb),loop = T)
    g
  }) ->
    geoms
  threejsr::threejs_scene(geoms)

}
