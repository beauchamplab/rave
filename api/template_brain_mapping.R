project = 'demo'
subjects = c('KC', 'YAB')
save_to = '~/Desktop/junk.csv'


# Prepare for the result
results = NULL

N27brain = threeBrain::merge_brain()$template_object

# Coordinate of AC (No need to calculate AC as SUMA brain is centered at AC)
# AC = N27brain$vox2vox_MNI305[1:3,4]


# Do it in parallel
rave:::rave_setup_workers()
tbls = dipsaus::lapply_async2(subjects, function(sub){
  cat('Analyzing for subject -', sub, '\n')
  brain <- rave::rave_brain2(sprintf('%s/%s', project, sub))
  
  # Ignore the past calculation and set electrodes as surface ones to enable 141 mapping
  # You can decide late whether to use 141 instead of MNI305
  
  # Get attached surface type for each electrodes
  surf_type = brain$electrodes$raw_table$SurfaceType
  if(is.null(surf_type)){
    surf_type = 'pial'
  }
  
  brain$electrodes$raw_table = brain$electrodes$raw_table[,1:5]
  brain$electrodes$raw_table$SurfaceElectrode = TRUE
  brain$electrodes$raw_table$SurfaceType = surf_type
  
  brain$electrodes$raw_table$MNI305_x = 0
  brain$electrodes$raw_table$MNI305_y = 0
  brain$electrodes$raw_table$MNI305_z = 0
  
  # Other not important
  brain$electrodes$raw_table$Radius = 2
  brain$electrodes$raw_table$VertexNumber = -1
  brain$electrodes$raw_table$Hemisphere = NA
  
  tbl = tryCatch({
    tbl = brain$calculate_template_coordinates(save_to = FALSE)
    tbl
  }, error = function(e){
    brain$electrodes$raw_table$SurfaceElectrode = FALSE
    tbl = brain$calculate_template_coordinates(save_to = FALSE)
    tbl
  })
  
  
  # Get N27brain's mesh
  N27coord = sapply( seq_len(nrow(tbl)), function(i){
    row = tbl[i,]
    if(!is.null(N27brain$surfaces[[row$SurfaceType]]$group)){
      tryCatch({
        vert <- N27brain$surfaces[[row$SurfaceType]]$group$get_data(
          sprintf(
            'free_vertices_Standard 141 %s Hemisphere - %s (N27)',
            c('Left', 'Right')[(row$Hemisphere=='right') + 1],
            row$SurfaceType
          )
        )
        coord = vert[row$VertexNumber, ]
        if(length(coord) == 3){
          return(coord)
        }
      }, error = function(e){
        c(NA, NA, NA)
      })
      
    }
    c(NA, NA, NA)
  })
  tbl$N27_x = N27coord[1,]
  tbl$N27_y = N27coord[2,]
  tbl$N27_z = N27coord[3,]
  tbl$Subject = sub
  tbl
}, callback = function(sub){
  paste('Analyzing for subject -', sub)
}, future.chunk.size = 1)

tbls = do.call(rbind, tbls)

tbls

write.csv(tbls, file = save_to)






