save_plot <- function(expr,
                      path,
                      width = as.numeric(rave_options('image_width')),
                      height = as.numeric(rave_options('image_height')), ...){
  png(filename = path, width = width, height = height, ...)

  f <- ~expr
  lazyeval::f_eval(f)


  dev.off()
}
