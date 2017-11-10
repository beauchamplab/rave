# RHOP

params = list(
  K = 3,
  lamu=0,
  lamv=0.1,
  lamw=0,
  omega=0,
  v=TRUE,
  maxIterations=1000,
  standardizationMethod = "flatten_trials",
  center=TRUE,
  scaled=TRUE,
  removeOutliers = TRUE
)

SHINY_INPUT = list(

  #   lamu (default 0): Optional, soft thresholding parameter for trials
  numeric_input('lamu', 'Thresholding Parameter (Trials)', min = 0, value = 0, triggers = FALSE,
                init = function(){
                  list(value = get_local_var('lamu', 0))
                }),



  #   lamv (default 0): Optional, soft thresholding parameter for electrodes
  numeric_input('lamv', 'Thresholding Parameter (Electrodes)', min = 0, value = 0.1, triggers = FALSE,
                init = function(){
                  list(value = get_local_var('lamv', 0.1))
                }),




  #   lamw (default 0): Optional, soft thresholding for frequencies
  numeric_input('lamw', 'Thresholding Parameter (Frequencies)', min = 0, value = 0, triggers = FALSE,
                init = function(){
                  list(value = get_local_var('lamw', 0))
                }),



  #   omega (default tridiagonal matrix): Optional, (time x time) positive semi-definite marix for smoothing penalty over time
  numeric_input('omega', 'Smoothing Penalty over Time', min = 0, value = 0, triggers = FALSE,
                init = function(){
                  list(value = get_local_var('omega', 0))
                }),





  # K: The number of components, 0 < K < min(size(x))-1

  numeric_input('K', 'Max Factors to extract', value = 1, min = 1, step = 1, triggers = FALSE,
                init = function(){
                  list(value = get_local_var('K', 1))
                }),





  #   maxIterations (default 1000): Optional, maximum number of iterations of alternating regression steps

  numeric_input('maxIterations', 'Max Iterations', value = 1000, min = 100, step = 1, triggers = FALSE,
                init = function(){
                  list(value = get_local_var('maxIterations', 1000))
                }),







  #   standardizationMethod (default 'none'): Optional, standardization options 'none', 'flatten_trials', and 'array_normal'

  select_input('standardizationMethod', 'Standardization Method', triggers = FALSE,
               choices = c('none', 'flatten_trials', 'array_normal'),
               selected = 'flatten_trials', init = function(){
                 list(selected = get_local_var('standardizationMethod', 'flatten_trials'))
               }),



  checkbox_input('removeOutliers', 'Remove Outliers', value = FALSE, triggers = FALSE,
                 init = function(){
                   list(value = get_local_var('removeOutliers', FALSE))
                 }),


  #   center (default true): Optional, boolean value indicating to mean-center data
  checkbox_input('center', 'Center Data', value = TRUE, triggers = FALSE,
                 init = function(){
                   list(value = get_local_var('center', TRUE))
                 }),



  #   scaled (default true): Optional, boolean value indicating scaling by standard deviation
  checkbox_input('scaled', 'Re-scale Data', value = TRUE, triggers = FALSE,
                 init = function(){
                   list(value = get_local_var('scaled', TRUE))
                 }),


  checkbox_input('v', 'Verbatim in Console', value = TRUE, triggers = FALSE,
                 init = function(){
                   list(value = get_local_var('v', TRUE))
                 }),


  action_button('run_rhop', 'Run RHOP')
)



detect_trial_outliers <- function(X, v=TRUE, maxIterations=1000, standardizationMethod="none", center=TRUE, scaled=TRUE, removeOutliers=TRUE){
  # This script serves as an R wrapper for the Matlab function 'process_patient_data.m'. It performs the regularized higher-order PCA as described in [1].
  #
  # Args:
  #   X: Multidimensional array of  ECoG covariate data with dimensions (trials x freq x time x electrode)
  #   standardizationMethod (default 'none'): Optional, standardization options 'none', 'flatten_trials', and 'array_normal'
  #   center (default true): Optional, boolean value indicating to mean-center data
  #   scaled (default true): Optional, boolean value indicating scaling by standard deviation
  #   removeOutliers (default true): Optional, boolean value indicating to remove outliers from data
  #   ovewrite (default true): Optional, boolean value indicating to overwrite saved data files
  #   v (default true): Optional, boolean valye indicating to print all messages in code
  #   maxIterations (default 1000): Optional, maximum number of iterations for outlier detection
  #
  # Returns:
  #   X: ECoG data, with pre-processing specified by incoming parameters
  #   Outliers: List of trials identified as outliers
  #
  # References
  # [1] Fred Campbell, ""

  # Load global variable 'rhop_code_dir', the directory containing required rhop Matlab code

  #saveOutputAs = configure_saveOutputAs(saveOutputAs)

  # Save parameters in a temporary HDF5 file
  uuid = uuid::UUIDgenerate()

  filename = paste("tmp_params_", uuid, ".h5", sep="")
  filepath = file.path(rafe_opts$get_options('temp_dir'), filename)
  if (file.exists(filepath)) { file.remove(filepath) }

  save_hdf5(X = X, name = 'X', group = 'ecog', file_name = filename, chunk = c(1, dim(X)[2:3], 1), level = 6)
  save_hdf5(X = v, name = 'v', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = maxIterations, name = 'maxIterations', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = standardizationMethod, name = 'standardizationMethod', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = center, name = 'center', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = scaled, name = 'scaled', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = removeOutliers, name = 'removeOutliers', group = 'params', file_name = filename, create_new = FALSE)


  # Run matlab function
  #cmd = paste('matlab -nodesktop -nosplash -r "cmd_process_patient_data ', filepath, '"', sep="")
  # cmd = paste("matlab -nodesktop -nosplash -r './inst/modules/RHOP/cmd_process_patient_data(", filepath, ")'", sep="")
  # './inst/modules/RHOP/'
  # system(cmd)
  system2(
    command = 'matlab',
    args = sprintf("-nodesktop -nosplash -r 'cmd_process_patient_data %s'", filename),
    env = c(
      sprintf("PATH=$PATH:%s", rafe_opts$get_options('matlab_path')),
      sprintf("MATLABPATH=%s:%s",
              tools::file_path_as_absolute(rafe_opts$get_options('temp_dir')),
              tools::file_path_as_absolute('./inst/modules/RHOP/')
      )
    )
  )


  # Load data and delete temporary files
  outliers = rhdf5::h5read(filepath, "/out/outliers")
  if (outliers == 0) { outliers = c() }
  results <- list(processed.X=rhdf5::h5read(filepath, "/out/outX"), outliers=outliers)


  unlink(filepath)

  return (results)
}

rhopca_for_ecog <- function(X, K, lamu=0, lamv=0, lamw=0, omega=0, v=TRUE, maxIterations=1000){
  # This script serves as an R wrapper for the Matlab function 'hopca_cptpa_ecog.m'. It performs the regularized higher-order PCA as described in [1].
  #
  # Args:
  #   X: An EcoG data matrix with dimensions (trials x freq x time x electrode)
  #   K: The number of components, 0 < K < min(size(x))-1
  #   lamu (default 0): Optional, soft thresholding parameter for trials
  #   lamv (default 0): Optional, soft thresholding parameter for electrodes
  #   lamw (default 0): Optional, soft thresholding for frequencies
  #   omega (default tridiagonal matrix): Optional, (time x time) positive semi-definite marix for smoothing penalty over time
  #   maxIterations (default 1000): Optional, maximum number of iterations of alternating regression steps
  #   v (default true): Optional, boolean value indicating to print all convergence messages from algorithm
  #
  # Returns:
  #   U: (trail x K) the trail factors
  #   V: (electrode x K) the node factors
  #   W: (frequency x K) the frequency factors
  #   T: (time x K) the time factors
  #   D: (K x 1) vector of CP-scaling constants
  #   Xhat: (trial x electrode x frequency x time) tensor
  #   objVals: A vector of objective function values
  #
  # References
  # [1] Fred Campbell, ""

  # alter h5
  # add save params to h5 file

  # Load global variable 'rhop_code_dir', the directory containing required rhop Matlab code


  # Save parameters in a temporary HDF5 file
  uuid = uuid::UUIDgenerate()

  filename = paste("tmp_params_", uuid, ".h5", sep="")
  filepath = file.path(rafe_opts$get_options('temp_dir'), filename)
  if (file.exists(filepath)) { file.remove(filepath) }

  save_hdf5(X = X, name = 'X', group = 'ecog', file_name = filename, chunk = c(1, dim(X)[2:3], 1), level = 6)
  save_hdf5(X = maxIterations, name = 'maxIterations', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = K, name = 'K', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = lamu, name = 'lamu', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = lamv, name = 'lamv', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = lamw, name = 'lamw', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = omega, name = 'omega', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = v, name = 'v', group = 'params', file_name = filename, create_new = FALSE)


  #h5write(saveOutputAs, filename, "params/saveOutputAs")

  # Run matlab function
  #cmd = paste('matlab -nodesktop -nosplash -r "cmd_hopca_cptpa_ecog ', filename, '"', sep='')
  # cmd = paste("matlab -nodesktop -nosplash -r 'cmd_hopca_cptpa_ecog(", filename, ")'", sep="")
  # system(cmd)

  system2(
    command = 'matlab',
    args = sprintf("-nodesktop -nosplash -r 'cmd_hopca_cptpa_ecog %s'", filename),
    env = c(
      sprintf("PATH=$PATH:%s", rafe_opts$get_options('matlab_path')),
      sprintf("MATLABPATH=%s:%s",
              tools::file_path_as_absolute(rafe_opts$get_options('temp_dir')),
              tools::file_path_as_absolute('./inst/modules/RHOP/')
      )
    )
  )

  # Load data and delete temporary files
  results <- list(D = rhdf5::h5read(filepath, "/out/D"),
                  Ti = rhdf5::h5read(filepath, "/out/Ti"),
                  U = rhdf5::h5read(filepath, "/out/U"),
                  V = rhdf5::h5read(filepath, "/out/V"),
                  W = rhdf5::h5read(filepath, "/out/W"),
                  objVals = rhdf5::h5read(filepath, "/out/objVals"),
                  Xhat = rhdf5::h5read(filepath, "/out/Xhat"))
  unlink(filepath)
  return (results)
}

make_rhopca_heatmaps <- function(U, V, W, Ti, X, omega, v) {
  # This script serves as an R wrapper for the Matlab function 'cmp_make_ecog_heatmaps.m'. It returns the matrices
  #
  # Args:
  #   U: (trail x K) the trail factors
  #   V: (electrode x K) the node factors
  #   W: (frequency x K) the frequency factors
  #   Ti: (time x K) the time factors
  #   X: (trials x freq x time x electrode) tensor
  #
  # Returns:
  #   Matrices for each factor and pairwise dimensions
  #
  # References
  # [1] Fred Campbell, ""



  # Save parameters in a temporary HDF5 file
  uuid = uuid::UUIDgenerate()

  filename = paste("tmp_params_", uuid, ".h5", sep="")
  filepath = file.path(rafe_opts$get_options('temp_dir'), filename)
  if (file.exists(filepath)) { file.remove(filepath) }

  save_hdf5(X = X, name = 'X', group = 'ecog', file_name = filename, chunk = c(1, dim(X)[2:3], 1), level = 6)
  save_hdf5(X = U, name = 'U', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = V, name = 'V', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = W, name = 'W', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = Ti, name = 'Ti', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = omega, name = 'omega', group = 'params', file_name = filename, create_new = FALSE)
  save_hdf5(X = v, name = 'v', group = 'params', file_name = filename, create_new = FALSE)


  # Run matlab function
  #cmd = paste('matlab -nodesktop -nosplash -r "cmd_make_ecog_heatmaps ', filename, '"', sep='')
  # cmd = paste("matlab -nodesktop -nosplash -r 'cmd_make_ecog_heatmaps(", filename, ")'", sep="")
  # system(cmd)

  system2(
    command = 'matlab',
    args = sprintf("-nodesktop -nosplash -r 'cmd_make_ecog_heatmaps %s'", filename),
    env = c(
      sprintf("PATH=$PATH:%s", rafe_opts$get_options('matlab_path')),
      sprintf("MATLABPATH=%s:%s",
              tools::file_path_as_absolute(rafe_opts$get_options('temp_dir')),
              tools::file_path_as_absolute('./inst/modules/RHOP/')
      )
    )
  )

  # Load data
  K = dim(U)[2]
  tr_x_e = list()
  tr_x_f = list()
  tr_x_t = list()
  e_x_f = list()
  e_x_t = list()
  f_x_t = list()
  for (k in 1:K){
    factor_name = paste("factor", k, sep="")
    tr_x_e[[factor_name]] = rhdf5::h5read(filepath, paste("/out/tr_x_e_", k, sep=""))
    tr_x_f[[factor_name]] = rhdf5::h5read(filepath, paste("/out/tr_x_f_", k, sep=""))
    tr_x_t[[factor_name]] = rhdf5::h5read(filepath, paste("/out/tr_x_t_", k, sep=""))
    e_x_f[[factor_name]] = rhdf5::h5read(filepath, paste("/out/e_x_f_", k, sep=""))
    e_x_t[[factor_name]] = rhdf5::h5read(filepath, paste("/out/e_x_t_", k, sep=""))
    f_x_t[[factor_name]] = rhdf5::h5read(filepath, paste("/out/f_x_t_", k, sep=""))
  }
  results = list("trial_x_electrode" = tr_x_e,
                 "trial_x_frequency" = tr_x_f,
                 "trial_x_time" = tr_x_t,
                 "electrode_x_frequency" = e_x_f,
                 "electrode_x_time" = e_x_t,
                 "frequency_x_time" = f_x_t)

  # delete temporary file
  unlink(filepath)
  return (results)
}


tmp_env = new.env()
tmp_env$signal = 0
result = new.env()
.tmp_file = temp_file()




SHINY_EXECUTE = function(params, ...){
  signal = get_local_var('run_rhop')
  if(length(signal) > 0 && !is.na(signal) && is.numeric(signal) && signal > tmp_env$signal){
    tmp_env$signal = signal
    K = get_local_var('K')
    lamu = as.double(get_local_var('lamu'))
    lamv = as.double(get_local_var('lamv'))
    lamw = as.double(get_local_var('lamw'))
    omega = as.double(get_local_var('omega'))
    v = get_local_var('v')
    maxIterations = get_local_var('maxIterations')
    center = get_local_var('center')
    scaled = get_local_var('scaled')
    removeOutliers = get_local_var('removeOutliers')
    standardizationMethod = get_local_var('standardizationMethod')

    print(params)
    # K = 3,
    # lamu=0,
    # lamv=0.1,
    # lamw=0,
    # omega=0,
    # v=TRUE,
    # maxIterations=1000,
    # standardizationMethod = "flatten_trials",
    # center=TRUE,
    # scaled=TRUE,
    # removeOutliers = TRUE

    async(function(){

      status = "Remove outliers and standardize ECoG data"
      current_step = 1
      save_RData(status, current_step, file = .tmp_file)
      processed.data = detect_trial_outliers(X=data_env$data, standardizationMethod=standardizationMethod, center=center, scaled=scaled, removeOutliers=removeOutliers)

      # 7. Run rhopca_for_ecog function: Perform RHOPCA
      status = "Perform regularized higher-order PCA"
      current_step = 2
      save_RData(status, current_step, file = .tmp_file)
      results = rhopca_for_ecog(X=processed.data$processed.X, K, lamu=lamu, lamv=lamv, lamw=lamw, omega=omega, v=v, maxIterations=maxIterations)

      # 8. Run make_rhopca_heatmaps function: Create ECoG RHOPCA heatmaps
      status = "Create heatmaps"
      current_step = 3
      save_RData(status, current_step, file = .tmp_file)
      out.data.fn = file.path(getwd(), 'heatmaps.mat')
      out = make_rhopca_heatmaps(U=results$U, V=results$V, W=results$W, Ti=results$Ti, X=processed.data$processed.X, omega = omega, v=v)

      status = "DONE!"
      current_step = 4
      save_RData(status, current_step, out, file = .tmp_file)

      return(out)

    }, .tmp_file = .tmp_file, output_envir = result)


    # obs = local(observable({
    #   if(shiny::isolate(w$status()) != 'resolved'){
    #     res = readLines(tmp_file)
    #     tmp_env$message(res)
    #   }else{
    #     tmp_env$message('Done')
    #     obs$observable = FALSE
    #   }
    # }))


  }


  return(list(
    status = function(){
      validate(
        need(exists('.count', envir = result), 'To run this module, please hit button - "Run RHOP"')
      )

      .count = result$.count()
      if(.count == -1){
        cat('Done')
        cat('\nPlease check other tabs.')
      }else{
        cat('Current Step', result$current_step, 'of 4', result$status, sep = ' ')
        cat('\nWARNING: Do Not hit "Run RHOP" button again. You have already launched the program.')
      }
      cat('\n\nParameters:\n')
      for(comp in SHINY_INPUT){
        cat(comp$argList$label, '-', get_local_var(comp$id), '\n', sep = ' ')
      }
    },

    # "trial_x_electrode" = tr_x_e,
    # "trial_x_frequency" = tr_x_f,
    # "trial_x_time" = tr_x_t,
    # "electrode_x_frequency" = e_x_f,
    # "electrode_x_time" = e_x_t,
    # "frequency_x_time" = f_x_t)
    frequency_x_time = function(){
      validate(
        need(exists('.count', envir = result), 'To run this module, please hit button - "Run RHOP"'),
        need(exists('.result', envir = result), 'Please wait for the results')
      )

      K = get_local_var('K')

      par(mfrow = c(1, K), mar = c(5, 4,4,8) + 0.1)
      on.exit({par(mfrow = c(1, 1))})

      time_range = as.numeric(data_env$subject$time_points$Time)
      for(i in 1:K){
        plot_images(
          result$.result$frequency_x_time[[i]], main = sprintf('Factor %d', i),
          xlab = 'Time (s)', ylab = 'Frequency Band',
          xrange = function(len){
            sapply(seq(min(time_range), max(time_range), length.out = len), round, digits = 2)
          },
          yrange = function(len){
            y = rep('', len)
            y[1] = 'Low'
            y[len] = 'High'
            y
          }
        )
      }

    },

    trial_x_time = function(){
      validate(
        need(exists('.count', envir = result), 'To run this module, please hit button - "Run RHOP"'),
        need(exists('.result', envir = result), 'Please wait for the results')
      )

      K = get_local_var('K')

      par(mfrow = c(1, K), mar = c(5, 4,4,8) + 0.1)
      on.exit({par(mfrow = c(1, 1))})

      time_range = as.numeric(data_env$subject$time_points$Time)
      for(i in 1:K){
        plot_images(
          result$.result$trial_x_time[[i]], main = sprintf('Factor %d', i),
          xlab = 'Time (s)', ylab = 'Trial',
          xrange = function(len){
            sapply(seq(min(time_range), max(time_range), length.out = len), round, digits = 2)
          },
          yrange = function(len){
            y = rep('', len);y
          }
        )
      }
    },

    electrode_x_time = function(){
      validate(
        need(exists('.count', envir = result), 'To run this module, please hit button - "Run RHOP"'),
        need(exists('.result', envir = result), 'Please wait for the results')
      )

      K = get_local_var('K')

      par(mfrow = c(1, K), mar = c(5, 4,4,8) + 0.1)
      on.exit({par(mfrow = c(1, 1))})

      time_range = as.numeric(data_env$subject$time_points$Time)
      for(i in 1:K){
        plot_images(
          result$.result$electrode_x_time[[i]], main = sprintf('Factor %d', i),
          xlab = 'Time (s)', ylab = 'Electrodes',
          xrange = function(len){
            sapply(seq(min(time_range), max(time_range), length.out = len), round, digits = 2)
          },
          yrange = function(len){
            y = rep('', len);y
          }
        )
      }
    }
  ))
}


plot_images = function(
  D, main = '', col = fields::tim.colors(64),
  xlab = 'x', ylab = 'y',

  xrange = function(len){
    seq(0, 1, length.out = len)
  },
  yrange = function(len){
    seq(0, 1, length.out = len)
  }){
  image(
    D, main = main, col = col, axes=FALSE, xlab = xlab, ylab = ylab
  )
  axis(1,axTicks(1),lab=xrange(length(axTicks(1))))
  axis(2,axTicks(2),lab=yrange(length(axTicks(2))))

  fields::image.plot(
    D, axes=FALSE, legend.only = T, col = col
  )
}


SHINY_OUTPUT = list(
  Status = list(verbatimtext_output('status', 'Console Information')),
  `Factors over Time` = list(
    plot_output('frequency_x_time', 'Frequency - Time Factors'),
    plot_output('trial_x_time', 'Trial - Time Factors'),
    plot_output('electrode_x_time', 'Electrodes - Time Factors')

  )
)



if(!exists('NOT_RUN')){
  attach_virtualenv('subject_lij118_ChBr', 1:5, module_path = './inst/modules/RHOP/main.R')
  K = 1
  lamu=0
  lamv=0.1
  lamw=0
  omega=0
  v=TRUE
  maxIterations=1000
  standardizationMethod = "flatten_trials"
  center=TRUE
  scaled=TRUE
  removeOutliers = FALSE


  processed.data = detect_trial_outliers(X=data_env$data, standardizationMethod=standardizationMethod, center=center, scaled=scaled, removeOutliers=removeOutliers)

  results = rhopca_for_ecog(X=processed.data$processed.X, K, lamu=lamu, lamv=lamv, lamw=lamw, omega=omega, v=v, maxIterations=maxIterations)

  out = make_rhopca_heatmaps(U=results$U, V=results$V, W=results$W, Ti=results$Ti, X=processed.data$processed.X, omega = omega, v=v)

  rm(get_local_var)
  detach_virtualenv()
}
