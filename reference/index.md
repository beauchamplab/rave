# Package index

## All functions

- [`ECoGRepository`](https://beauchamplab.github.io/rave/reference/ECoGRepository.md)
  :

  R6 class for `iEEG/ECoG` data Repository

- [`Electrode`](https://beauchamplab.github.io/rave/reference/Electrode.md)
  : R6 Class for Electrode

- [`ExecEnvir`](https://beauchamplab.github.io/rave/reference/ExecEnvir.md)
  : Session-based Module Runtime Environment Class

- [`ModuleEnvir`](https://beauchamplab.github.io/rave/reference/ModuleEnvir.md)
  : R6 'RAVE' Module Class

- [`Subject`](https://beauchamplab.github.io/rave/reference/Subject.md)
  : R6 Class for 'RAVE' Subject

- [`any_subject_loaded()`](https://beauchamplab.github.io/rave/reference/any_subject_loaded.md)
  : Function to check if data repository has data

- [`archive_subject()`](https://beauchamplab.github.io/rave/reference/archive_subject.md)
  : Archive Subject into Zipped file

- [`arrange_data_dir()`](https://beauchamplab.github.io/rave/reference/arrange_data_dir.md)
  : Initialize data repository

- [`arrange_modules()`](https://beauchamplab.github.io/rave/reference/arrange_modules.md)
  : Update (optional), and check validity of modules

- [`as_subject()`](https://beauchamplab.github.io/rave/reference/as_subject.md)
  : Make new subject object from character

- [`attachDefaultDataRepository()`](https://beauchamplab.github.io/rave/reference/attachDefaultDataRepository.md)
  : Attach subject data

- [`baseline()`](https://beauchamplab.github.io/rave/reference/baseline.md)
  : Baseline signals

- [`cache_raw_voltage()`](https://beauchamplab.github.io/rave/reference/cache_raw_voltage.md)
  : Import Subject "Matlab" File and Create "HDF5" files

- [`check_data_repo()`](https://beauchamplab.github.io/rave/reference/check_data_repo.md)
  : Check if default data environment has object

- [`check_dependencies()`](https://beauchamplab.github.io/rave/reference/check_dependencies.md)
  : Check and Install RAVE Dependencies

- [`check_epoch()`](https://beauchamplab.github.io/rave/reference/check_epoch.md)
  : Check if epoch file is valid

- [`check_subject()`](https://beauchamplab.github.io/rave/reference/check_subject.md)
  : Complete validity check a RAVE subject

- [`check_subjects2()`](https://beauchamplab.github.io/rave/reference/check_subjects2.md)
  : check subject validity tools

- [`check_subjects_old()`](https://beauchamplab.github.io/rave/reference/check_subjects_old.md)
  : check subject validity tools (use check_subjects2)

- [`create_local_cache()`](https://beauchamplab.github.io/rave/reference/create_local_cache.md)
  : Create local cache to speed up loading speed

- [`customizedUI()`](https://beauchamplab.github.io/rave/reference/customizedUI.md)
  : Customized Shiny Elements

- [`decimate_fir()`](https://beauchamplab.github.io/rave/reference/decimate_fir.md)
  :

  Decimate or Down-sample a Signal using `FIR` Filters

- [`define_initialization()`](https://beauchamplab.github.io/rave/reference/define_initialization.md)
  :

  Defines 'RAVE' Module Initialization Defines the global variables for
  the module. Called along with
  [`define_input`](https://beauchamplab.github.io/rave/reference/define_input.html)
  to define UI initialization actions once a subject is loaded.

- [`define_input()`](https://beauchamplab.github.io/rave/reference/define_input.md)
  : Defines 'RAVE' Module Inputs

- [`define_output()`](https://beauchamplab.github.io/rave/reference/define_output.md)
  : Define 'RAVE' Module Output

- [`detect_modules()`](https://beauchamplab.github.io/rave/reference/detect_modules.md)
  : Check all packages to for new RAVE module packages

- [`diagnose_signal()`](https://beauchamplab.github.io/rave/reference/diagnose_signal.md)
  : Plot and Inspect Signals in Trace, Periodogram, and Histogram

- [`download_sample_data()`](https://beauchamplab.github.io/rave/reference/download_sample_data.md)
  : Function to download demo data to data repository

- [`download_subject_data()`](https://beauchamplab.github.io/rave/reference/download_subject_data.md)
  : Function to download subjects from internet/local

- [`electrode_localization()`](https://beauchamplab.github.io/rave/reference/electrode_localization.md)
  : Electrode localization

- [`eval_when_ready()`](https://beauchamplab.github.io/rave/reference/eval_when_ready.md)
  : Add Function to run once Module is Ready

- [`export_diagnose_voltage()`](https://beauchamplab.github.io/rave/reference/export_diagnose_voltage.md)
  : Export voltage (analog trace) diagnostic plots for each electrode

- [`fake_session()`](https://beauchamplab.github.io/rave/reference/fake_session.md)
  : Fake 'shiny' Session for Debug Purpose

- [`finalize_installation()`](https://beauchamplab.github.io/rave/reference/finalize_installation.md)
  : Finalize installation

- [`getDefaultDataRepository()`](https://beauchamplab.github.io/rave/reference/getDefaultDataRepository.md)
  : Get environment where subject data is loaded

- [`getExecEnvirFromContext()`](https://beauchamplab.github.io/rave/reference/getExecEnvirFromContext.md)
  : Get Module Runtime Environment from Current Context

- [`getModuleEnvirFromContext()`](https://beauchamplab.github.io/rave/reference/getModuleEnvirFromContext.md)
  : Get Module Instance from Current Context

- [`get_content()`](https://beauchamplab.github.io/rave/reference/get_content.md)
  : Parse 'RAVE' Module Contents

- [`get_dir()`](https://beauchamplab.github.io/rave/reference/get_dir.md)
  : Get Directories in \`RAVE\`

- [`get_fake_updated_message()`](https://beauchamplab.github.io/rave/reference/get_fake_updated_message.md)
  : internally used for debugging functions

- [`get_mem_usage()`](https://beauchamplab.github.io/rave/reference/get_mem_usage.md)
  : Get RAM usage

- [`get_module()`](https://beauchamplab.github.io/rave/reference/get_module.md)
  : Function to find modules in packages

- [`get_path()`](https://beauchamplab.github.io/rave/reference/get_path.md)
  : Safe Way to Access Module Package Files Using Relative Path

- [`get_rave_theme`](https://beauchamplab.github.io/rave/reference/get_rave_theme.md)
  : Get RAVE Theme from Package Settings

- [`get_subjects()`](https://beauchamplab.github.io/rave/reference/get_subjects.md)
  : Get all subjects within project

- [`get_val()`](https://beauchamplab.github.io/rave/reference/get_val.md)
  : Get Value or Default

- [`import_electrodes()`](https://beauchamplab.github.io/rave/reference/import_electrodes.md)
  :

  Import `.csv` files that contain electrode information

- [`init_app()`](https://beauchamplab.github.io/rave/reference/init_app.md)
  : Initialize main application for debugging purpose

- [`init_module()`](https://beauchamplab.github.io/rave/reference/init_module.md)
  : Initialize 'RAVE' module for debug purpose

- [`lapply_async()`](https://beauchamplab.github.io/rave/reference/lapply_async.md)
  [`lapply_async3()`](https://beauchamplab.github.io/rave/reference/lapply_async.md)
  :

  `lapply` using future package in asynchronous way

- [`load_local_cache()`](https://beauchamplab.github.io/rave/reference/load_local_cache.md)
  : Load local cache for fast importing voltage, power, and phase

- [`load_meta()`](https://beauchamplab.github.io/rave/reference/load_meta.md)
  : Load subject meta data

- [`load_modules()`](https://beauchamplab.github.io/rave/reference/load_modules.md)
  : Load RAVE Modules

- [`load_rave_module_package()`](https://beauchamplab.github.io/rave/reference/load_rave_module_package.md)
  : Function to load RAVE module package with UI tools

- [`load_scripts()`](https://beauchamplab.github.io/rave/reference/load_scripts.md)
  : Load scripts that cannot put into package R folder

- [`module_analysis_names()`](https://beauchamplab.github.io/rave/reference/module_analysis_names.md)
  : Find module analysis names

- [`mount_demo_subject()`](https://beauchamplab.github.io/rave/reference/mount_demo_subject.md)
  : Load Demo Subject According to Package Configuration File

- [`notch_channel()`](https://beauchamplab.github.io/rave/reference/notch_channel.md)
  : Filter line noise out from ECoG channels

- [`notch_filter()`](https://beauchamplab.github.io/rave/reference/notch_filter.md)
  : Apply Notch Filter to Analog Trace Data

- [`plot_signals()`](https://beauchamplab.github.io/rave/reference/plot_signals.md)
  : Plot signals line by line

- [`progress()`](https://beauchamplab.github.io/rave/reference/progress.md)
  : A wrapper for shiny Progress object

- [`pwelch()`](https://beauchamplab.github.io/rave/reference/pwelch.md)
  : Plot "Welch" Periodogram

- [`r_to_py.Subject()`](https://beauchamplab.github.io/rave/reference/r_to_py.Subject.md)
  : Convert subject to python object

- [`cache()`](https://beauchamplab.github.io/rave/reference/rave-cache.md)
  [`cache_input()`](https://beauchamplab.github.io/rave/reference/rave-cache.md)
  [`clear_cache()`](https://beauchamplab.github.io/rave/reference/rave-cache.md)
  : Cache R Objects with Different levels

- [`close_tab()`](https://beauchamplab.github.io/rave/reference/rave-tabs.md)
  [`open_tab()`](https://beauchamplab.github.io/rave/reference/rave-tabs.md)
  : Open/Close a tab in RAVE main application

- [`rave_brain2()`](https://beauchamplab.github.io/rave/reference/rave_brain2.md)
  : Tools to load and view brain in 3D viewer

- [`rave_checks()`](https://beauchamplab.github.io/rave/reference/rave_checks.md)
  : Check if data is loaded for current module

- [`rave_context()`](https://beauchamplab.github.io/rave/reference/rave_context.md)
  : 'RAVE' Context: Read and Set Context of Environments

- [`rave_context_generics()`](https://beauchamplab.github.io/rave/reference/rave_context_generics.md)
  : Create S3 Generics that Respects 'RAVE' Context

- [`rave_failure()`](https://beauchamplab.github.io/rave/reference/rave_failure.md)
  : RAVE Failure Message

- [`rave_ignore()`](https://beauchamplab.github.io/rave/reference/rave_ignore.md)
  : Functions for development use

- [`rave_import_rawdata()`](https://beauchamplab.github.io/rave/reference/rave_import_rawdata.md)
  : Import Raw Signal from Non-standard Formats

- [`rave_module_tools()`](https://beauchamplab.github.io/rave/reference/rave_module_tools.md)
  : Tools for module writers

- [`rave_options()`](https://beauchamplab.github.io/rave/reference/rave_options.md)
  : Function to change rave-options

- [`rave_prepare()`](https://beauchamplab.github.io/rave/reference/rave_prepare.md)
  :

  Load Subject and Create `iEEG/ECoG` Data Environment

- [`rave_preprocess()`](https://beauchamplab.github.io/rave/reference/rave_preprocess.md)
  : RAVE Preprocess Function

- [`rave_preprocess_tools()`](https://beauchamplab.github.io/rave/reference/rave_preprocess_tools.md)
  : Function to create RAVE preprocess tools

- [`rave_version()`](https://beauchamplab.github.io/rave/reference/rave_version.md)
  : Get RAVE version

- [`read_mgrid()`](https://beauchamplab.github.io/rave/reference/read_mgrid.md)
  :

  Make `iElvis` `mgrid` file

- [`reload_module_package()`](https://beauchamplab.github.io/rave/reference/reload_module_package.md)
  : Reload 'RAVE' module package without restarting 'RStudio'

- [`safe_write_csv()`](https://beauchamplab.github.io/rave/reference/safe_write_csv.md)
  : Save data to "CSV", if file exists, rename old file

- [`save_meta()`](https://beauchamplab.github.io/rave/reference/save_meta.md)
  : Function to save meta data to subject

- [`save_options()`](https://beauchamplab.github.io/rave/reference/save_options.md)
  : Function to locally save options (deprecated)

- [`getDefaultReactiveInput()`](https://beauchamplab.github.io/rave/reference/session-reactives.md)
  [`getDefaultReactiveOutput()`](https://beauchamplab.github.io/rave/reference/session-reactives.md)
  : Get \`shiny' "input" and "output" objects under current context

- [`set_rave_theme()`](https://beauchamplab.github.io/rave/reference/set_rave_theme.md)
  : Set and Return RAVE theme

- [`shinirize()`](https://beauchamplab.github.io/rave/reference/shinirize.md)
  : Convert module to objects used in shiny

- [`start_rave_legacy()`](https://beauchamplab.github.io/rave/reference/start_rave.md)
  [`launch_demo()`](https://beauchamplab.github.io/rave/reference/start_rave.md)
  [`start_rave2()`](https://beauchamplab.github.io/rave/reference/start_rave.md)
  [`start_rave()`](https://beauchamplab.github.io/rave/reference/start_rave.md)
  : Start RAVE main application

- [`start_yael()`](https://beauchamplab.github.io/rave/reference/start_yael.md)
  : Start 'YAEL' electrode localization

- [`subject_tmpfile()`](https://beauchamplab.github.io/rave/reference/subject_tmpfile.md)
  : Create temp file in subject module folder

- [`suma_spec_parse()`](https://beauchamplab.github.io/rave/reference/suma_spec_parse.md)
  :

  Parse `SUMA` `spec` file

- [`suma_surface_volume_parse()`](https://beauchamplab.github.io/rave/reference/suma_surface_volume_parse.md)
  :

  Parse `AFNI` `BRIK/HEAD` file

- [`to_module()`](https://beauchamplab.github.io/rave/reference/to_module.md)
  : Parse 'RAVE' Module and Returns Parsed Content in Environments

- [`view_layout()`](https://beauchamplab.github.io/rave/reference/view_layout.md)
  : Debug-use only, reload package, mount demo subject, and launch shiny
  app

- [`wavelet()`](https://beauchamplab.github.io/rave/reference/wavelet.md)
  : Wavelet Transformation With Phase

- [`wavelet_kernels()`](https://beauchamplab.github.io/rave/reference/wavelet_kernels.md)
  : Returns wavelets to be used for wavelet function
