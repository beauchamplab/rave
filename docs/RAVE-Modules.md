# Module Location

Go to R, and type `rave::rave_options('module_root_dir')`, or if you just installed `RAVE`, go to your home directory `~/`. There should be a folder called `rave_modules`. Open that folder, there should be one `csv` file and a folder called `modules`:

```
rave_modules/
|
|      modules.csv
|
\ ---- modules/
       |
       | ---- blank_template
       | ---- export_template
       |
       + ---- example/
       |      |
       |      \ ...
       |
       + ---- condition_explorer/
       |      |
       |      | ---- main.R
       |      |      plot_funcs.R
       |      \      ...
       |
       ...
```

# Manage your Modules

Open `modules.csv`, there are several columns:

* __ModuleID__: Unique ID for the module (in this column), use letters (lower case, `a-z`) and underscore `_`.
* __Name__: Label for the module, will be displayed in RAVE
* __ScriptPath__: `Absolute path` to module script, usually `main.R`
* __Active__: One of the following: `TRUE`, `FALSE`, `POWER`, or `PHASE`. `TRUE` means enabled for all data types. `FALSE` means this module is disabled. `POWER` means this module will be disabled if you only load phase data. `PHASE` means this module will be disabled if phase data is missing
* __Author__: Who wrote the module?
* __Version__: For example: 1.0.9, use `.` to separate numeric values.




# Default Modules

