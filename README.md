<img src="inst/assets/images/logo-md.jpg" width="20%" align="right" />

# RAVE

__R__ __A__*nalysis and* __V__*isualization of intracranial* __E__*lectroencephalography*


## Easy Installation
For Mac and Windows, you can download a script that will install all requirements for RAVE as well as the RAVE package.
  + For Mac users, click here to download the [installation script](https://github.com/dipterix/instrave/raw/master/rave-installer-macosx.command.zip)
  
  After installation, find the file "rave-installer-macosx.command.zip" in your "Downloads" folder. If it has not automatically been unzipped, double-click to extract the file named "rave-installer-macosx.command". To run the the commands in the script, right click (if you don't have a right mouse button, hold down the control key on the keyboard and click the mouse button or trackpad).  on the file to bring up an actions menu and select "Open". You will receive a warning "rave-installer-macosx.command is from an unidentified developer. Are you sure you want to open it?". Choose "Open" to proceed. If you do not see the "Open" option (only "OK") you probably left-clicked instead of right-clicked. Select "OK" and try again.
  + for Windows (>= 7) users, please save [batch script](https://raw.githubusercontent.com/dipterix/instrave/master/rave-installer-windows.bat) as \*.bat file, double-click on it to install
  + For Linux users, please use the alternative installation guide [put link here]
  
  * This installation script will attempt to install the necessary pre-requisite software. For a list, see: * RAVE runs on Mac, Windows or Linux. [Click here for the prerequisite installation guide](./Installation.md) to install the latest versions of all required tools. RAVE requires the latest versions of the R (>= 3.6.0) and RStudio and trying to install RAVE on older versions will lead to unpredictable error messages. [Click here for suggested system configurations](./Requirements.md).

* After completing the previous step, open the RStudio application using the desktop shortcut; in Mac OSX, RStudio can be found in the Applications folder. 

#### Alternative installation guide

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

## Start RAVE 

* To start RAVE, type (or copy and paste) the following command into the RStudio console:
```r
rave::start_rave()
```
* If installation has proceeded correctly, a new web browser window should open with the RAVE splash screen that shows the current version number (e.g. 1.0).

## Using RAVE

* To use RAVE, you will need to load data by clicking "Select Data". If you do not have any data in RAVE format, you will need to create some. 
* Option 1: You can download demo data (see below).
* Option 2: Create data in RAVE format by preprocessing your existing raw data. To preprocess data 
```r
rave::rave_preprocess()
```
* Option 3: If you have RAVE format data on a server (or anywhere besides the default local directory), point RAVE to it with
```r
rave::rave_options()
```
* For tutorials on how to use RAVE, [click here](https://openwetware.org/wiki/Beauchamp:RAVE#Tutorials)

## Download Demo Data 

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





