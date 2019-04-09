

# RAVE

__R__ __A__*nalysis and* __V__*isualization of intracranial* __E__*lectroencephalography*

## Installation

* RAVE requires the installation of the language "R" and other miscellaneous programming tools, including "RStudio".
Please [click here for the prerequisite installation guide](./Installation.md) to install all needed tools for your operating system. 

* RAVE runs best on modern computers with multicore CPUs, [click here for suggested system configurations](./Requirements.md).

* After the prerequisites are installed, open R-Studio (in Mac OSX, Rstudio icon is installed in your Applications folder). Copy and paste the following commands into the RStudio console to download the current version of RAVE. The commands must be typed one at a time. If packages need to be installed, you may accept the defaults by typing "Yes" to any questions that appear. In the case of errors, relaunch RStudio and repeat the commands.
```r
# Install RAVE
install.packages('remotes')
remotes::install_github('beauchamplab/rave')
```
* Alternative installation (if previous method fails)
```r
install.packages('devtools')
devtools::install_github('beauchamplab/rave')
```

* After installing or updating RAVE, quit and restart RStudio before continuing.

## Download Demo Data 

* If you do not have any data in RAVE format, we recommend you download some sample data. Copy and paste the following commands into the RStudio console:

```r
# load RAVE functions 
library(rave)
# download a demo subject ~ 400MB of data
download_subject_data('https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip')
```

Once you see the folowing message, the subject is downloaded. The directory (XXX) will vary depending on the machine.

```
[ INFO ]: Expanding zip file
[ INFO ]: Copy from tempdir to data repository
[ INFO ]: Clean up
[ INFO ]: Done. Subject [sub1] is now at 
[Raw Data]: /XXX/rave_data/raw_dir/sub1
[RAVE Data]: /XXX/rave_data/raw_dir/demo
```

## Start RAVE 

* To start RAVE, copy and paste the following commands into the RStudio console:
```r
# Launch main app
rave::start_rave()
```

## Using Rave

* For tutorials on how to use RAVE, [click here](https://openwetware.org/wiki/Beauchamp:RAVE#Tutorials)

* To preprocess data, copy and paste the following commands into the RStudio console:
```r
# Preprocess
rave::rave_preprocess()
```



