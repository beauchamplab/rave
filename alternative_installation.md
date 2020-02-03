# Full RAVE installation guide

First, buy a computer! For best performance, we recommend a 4-core CPU (e.g. Intel i5) with 64GB RAM (at least 8 GB per core).
RAVE displays results on a web browser, and is tested with Google Chrome. Apple and Microsoft browsers will run with reduced functionality (e.g. shaders will not render brain slices).

Next, install all required prerequisites. RAVE requires the latest versions of R (>= 3.6.0) and RStudio. Trying to install RAVE on older versions of R and RStudio will lead to unpredictable error messages. [Click here for the prerequisite installation guide](./Installation.md).

* After completing the previous step, open the RStudio application using the desktop shortcut; in Mac OSX, RStudio can be found in the Applications folder. Copy and paste the following commands (one at a time) into the RStudio console to install the current version of RAVE. Answer "Yes" to any questions that appear. In the case of errors, relaunch RStudio and repeat the commands. 
```r
# this step requires XCode on mac or RTools on Windows
install.packages('devtools')
devtools::install_github('beauchamplab/rave')

# this step will grab other packages needed by RAVE. If asked to compile packages from source, you can safely so no
rave::check_dependencies()

# this step ensures all available RAVE modules are accessible
rave::arrange_modules(TRUE, TRUE)

# This step ensures you have a template brain already installed.
threeBrain::download_N27(make_default = TRUE)
```

To update existing installations of RAVE, use Option 1 or run the following commands after making sure to update R and RStudio. Start RAVE and verify that the newer version loads. RStudio is unable to update packages that are in use, so it may be necessary to restart RStudio and retry the update. As an additional step, use the RStudio package manager to delete all RAVE packages and reinstall.

```r
devtools::install_github('beauchamplab/rave')
rave::check_dependencies()
```

* After installing or updating RAVE, quit and restart RStudio before continuing (Option 1 does this automatically).

## Download Demo data 

* If you do not have any data in RAVE format, we recommend you download some sample data. Each RStudio command below downloads a dataset from a single subject.
```r
# 500MB ~ 1.5 GB per subject
rave::download_sample_data('KC')
rave::download_sample_data('YAB')
rave::download_sample_data('YAD')
rave::download_sample_data('YAF')
rave::download_sample_data('YAH')
rave::download_sample_data('YAI')
rave::download_sample_data('YAJ')
rave::download_sample_data('YAK')

# download group analysis sample - 72 MB. Please download at least 1 subject above.
rave::download_sample_data('_group_data')
```

Once you see the following message, the subject is downloaded. The directory (XXX) will vary depending on the machine. If a subject previously exists, RAVE will ask you to choose from replacing, creating new or abandon the downloaded subject. 

```
[ INFO ]: Expanding zip file
[ INFO ]: Copy from tempdir to data repository
[ INFO ]: Clean up
[ INFO ]: Done. Subject [sub1] is now at 
[Raw Data]: /XXX/rave_data/raw_dir/KC
[RAVE Data]: /XXX/rave_data/data_dir/demo/KC
```

