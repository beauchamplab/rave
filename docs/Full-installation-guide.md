*(table of content)*

* [Prerequisites](#step-1-prerequisites)
* [R Environment Setup](#step-2-r-environment-setup)
* [RAVE install](#step-3-rave-install)
* [Common Questions](#common-questions)

In this section, you'll install R, RStudio, devtools and all other dependencies.

`R` is a functional programming language that `RAVE` uses. `Devtools` are necessary for R to enable advanced features. `RStudio` is an IDE (Intergrated Development Environment) for easy and better code management especially designed for R. If you are hosting a server in RAVE, or prefer to using command lines, RStudio is not necessary. However, it's highly recommended that you have RStudio installed for `RAVE`.

Before installation, you might want to check you system:

# Step 1: Prerequisites

`RAVE` is developed on both MacOS and Windows.

**Hardware Requirement**

*(Recommended)*

* Intel 4th Gen Core i5 CPU
* 64 GB RAM

*(Minimum Requirement)*

* Intel i3 CPU
* 8 GB RAM

# Step 2: R Environment Setup

`RAVE` is developed and built in `R`, therefore, you need to install `R` as its environment.

Here I tested installations on these operating systems: (as of 03/03/2018)

* [MacOS](#macos) (El-Captain or higher)
* [Windows](#windows) (10 or higher, `SUMA` will be disabled on Windows)
* Linux (To be tested)

## MacOS

*(Tested on El-Captain and High-Sierra)*

1. First, go to Cran-R official website and download install the latest R:

[https://cran.r-project.org/bin/macosx/](https://cran.r-project.org/bin/macosx/)

2. After installing R, make sure that you install Xcode from the Mac App Store:

The best way is to google **How to install Xcode**. Moncef Belyamani has a great article [here](https://www.moncefbelyamani.com/how-to-install-xcode-homebrew-git-rvm-ruby-on-mac/).

Or if you have `AFNI` installed, you must have Xcode installed, then you need xcode command line tool. Open terminal (from Application), enter

```
xcode-select --install
```

3. RStudio

[https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)


## Windows

On Windows, RAVE is limited because *AFNI* doesn't support windows. However, most of RAVE functions should work. To install RAVE, you need to have *bash* enabled on Windows 10.

It's easy to install on Windows. 

1. First, go to Cran-R official website and download install the latest R:

[https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)

2. After installing R, download and install *Rtools*. Please install the latest version:

[https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)

3. RStudio

[https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)

# Step 3: RAVE install

**If you have installed RStudio, open it, or if you are using terminal/command line, type `R` to enter R.**

There are two ways to install RAVE. The first method install `RAVE` with a script, if you are not familiar with `R`, this could be a good choice. The second one is recommended for those who has written scripts in `R` before.

### 1. Automatically install recommended version of RAVE 

I have written a very simple and easy script for you. Open R, or RStudio, enter:

```
source('https://raw.githubusercontent.com/dipterix/instrave/master/R/hello.R')
```

If you fail, don't worry, try it several times, since there are so many packages to be installed and "turn R off and on again" is the easiest way to clean the installation environment :) However, if you try more than five times and still get errors, this might be an issue. Please report the issue on Github or contact me via my [email](mailto:dipterix.wang@gmail.com?Subject=[RAVE_Issues_Github]&Body=Hi%20Dipterix).


![Warning Message in RStudio](https://github.com/dipterix/instrave/blob/master/img/installation/restart%20warnings.PNG)

_* You might see the following message several times. Press "Yes". After restarted, run the command above again._

If you see console having the following output, then `RAVE` is installed

```
Loading required package: rave
--------------------
Active modules: 
 - Condition Explorer
Please edit [~/rave_modules/modules.csv]
...
```
Then you will see that `RAVE` is installed and attached. Type 

```
require(rave)
init_app()
```

Enjoy :)

### 2 Manually install RAVE (For `RAVE` module writers)

Inside of R, install `devtools` and `rhdf5` by typing the following commands:

*(Install devtools)*

```
install.packages('devtools')
```

*(Install `rhdf5` and `HDF5Array` from Bioconductor)*

```
source("https://bioconductor.org/biocLite.R")
biocLite(c("rhdf5", "HDF5Array"), suppressUpdates = T, suppressAutoUpdate = T)
```

*(Install `RAVE`)*

```
devtools::install_github('beauchamplab/rave', ref = 'rave-dipterix')
```

After installing `RAVE`, type the following command to test it.

```
require(rave)
init_app()
```


# Common Questions

Here listed some common questions that might answer your questions.

**Q: When I install `RAVE` from script that you wrote, R keeps asking me to restart before installing new packages. What should I do?**

A: Press `YES`, and R will re-launch and install dependencies with clean environment. This might occur several times depending on the number of packages to update (usually two to four). However, after re-launching your R, you need to run the following code again to continue installing `RAVE`.

```
source('https://raw.githubusercontent.com/dipterix/instrave/master/R/hello.R')
```