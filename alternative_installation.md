# Full RAVE Alternative installation guide

First, install all required prerequisites if they are not alreay installed.



* Option 1: Copy and paste the following command into the RStudio console to install the current version of RAVE or update an existing installation. The script will check to make sure that you have the correct versions of R and RStudio and will prompt you to install them if not. The RStudio installer will ask questions, such as "What CRAN Mirror to use?" (picking a site that is nearby will speed installation) and whether it is OK to install various libraries and packages (answer "Yes"). 
```r
source('https://raw.githubusercontent.com/dipterix/instrave/master/R/hello.R', echo = FALSE)
```
* Option 2: Copy and paste the following commands (one at a time) into the RStudio console to install the current version of RAVE. Answer "Yes" to any questions that appear. In the case of errors, relaunch RStudio and repeat the commands or try Option 3. 
```r
install.packages('devtools')
devtools::install_github('beauchamplab/rave')
rave::check_dependencies()
rave::arrange_modules(TRUE, TRUE)
threeBrain::download_N27(make_default = TRUE)
```
* Option 3: Copy and paste the following commands into the RStudio console, one at a time. Answer "Yes" to any questions that appear. 
```r
install.packages('pak')
pak::pkg_install('beauchamplab/rave')
rave::check_dependencies()
rave::arrange_modules(TRUE, TRUE)
threeBrain::download_N27(make_default = TRUE)
```

To update existing installations of RAVE, use Option 1 or run the following commands after making sure to update R and RStudio. Start RAVE and verify that the newer version loads. RStudio is unable to update packages that are in use, so it may be necessary to restart RStudio and retry the update. As an additional step, use the RStudio package manager to delete all RAVE packages and reinstall.

```r
devtools::install_github('beauchamplab/rave')
rave::check_dependencies()
```

* After installing or updating RAVE, quit and restart RStudio before continuing (Option 1 does this automatically).
