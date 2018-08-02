# RAVE
R Analysis and Visualization of ECOG/iEEG Data

![RAVE Module - Condition Explorer](https://raw.githubusercontent.com/dipterix/instrave/master/img/mainapp/mainapp_demo.png)


## A. Installation

### 1. Environment Setup

In this section, you'll install R, RStudio, devtools and all other dependencies. To start, we need to know what are R, RStudio, devtools

- **R** is a functional programming language that *RAVE* uses. Similar to Matlab, Python, C++, it's a language.
- **Devtools** are necessary to enable advanced features, especially when you want to compile R packages.
- **RStudio** is an IDE *(Intergrated Development Environment)* for easy and better code management especially designed for R.

You need to check your operating system before installtion:

As of 07/10/2018, the following systems are tested:

+ [Mac OS](#macos)
+ [Windows (Windows 10, with Bash enabled)](#windows)
+ [Ubuntu 16.04+](#ubuntu)
+ Linux (Others)

Minimum system requirement:

- Multicore CPU (2+)
- 8GB RAM
- 128 GB Free Hard Disk Space
- 512 MB Video Memory
- WebGL-enabled Web Browser

Suggested system requirement:

- 8 Core CPU
- 64GB RAM (6-8GB per CPU)
- 1T Hard Disk Space
- 2 GB Video Memory
- WebGL-enabled Web Browser


#### MacOS

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

4. Install `devtools` and `RAVE`

Open RStudio, enter the following command in RStudio **console**

```
install.packages('devtools')
```

To install `RAVE`

```
devtools::install_github('beauchamplab/rave')
```

#### Windows

1. Install R with the **latest** version

[https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)

2. Install *Rtools*. Please install the **latest** version:

[https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)

3. Download and install RStudio for Windows

[https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)

4. Install `devtools` and `RAVE`

Open RStudio, enter the following command in RStudio **console**

```
install.packages('devtools')
```

To install `RAVE`

```
devtools::install_github('beauchamplab/rave')
```

*Little problem you might get*

Sometimes, `devtools` will report errors like

```
WARNING: Rtools is required to build R packages, but no version of Rtools 
compatible with R ... was found. (Only the following incompatible 
version(s) of Rtools were found ...
```

Please make sure that you have the right `RTools` installed. Please check [RTools website](https://cran.r-project.org/bin/windows/Rtools/)

If you install the right version of RTools, then run the following command in RStudio to force it.

```
assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "3.5.999", path = "bin"))), "devtools")
```


#### Ubuntu

1. Add Cran repository to your system

Add R-Cran repository to your app list:

Open terminal (if you don't know how, look at your sidebar in ubuntu, `search your computer` enter "terminal"), type the following code:

```
sudo gedit /etc/apt/sources.list
```

Then there will be a text editor with lots of "deb"s in it. Append the following line at the end of this file


```
deb https://cloud.r-project.org/bin/linux/ubuntu xenial-cran35/
```

**IMPORTANT** Based on you system, you might want to enter different repositories (https://cran.r-project.org/bin/linux/). However, this line should be with a format of `deb` + `[repository URL]` + `xenial` (or `xenial-cran35`) + `/`. The difference between `xenial` and `xenial-cran35` will affect the version of R to be installed.

Save the file and close text editor. In your terminal, update `apt-get` repository:

```
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
sudo apt-get update
```

The first line gives you access to the new repository (it adds keys to your computer so that you can safely download R). 

2. Install R

Copy the following command line by line into your terminal and run

```
sudo apt-get install r-base
```

```
sudo apt-get install r-base-dev
```

3. Install compiling tools and system dependencies

After installing R, copy the following command to your terminal and run:

```
sudo apt-get install libssl-dev libcurl4-openssl-dev libssh2-1-dev libv8-3.14-dev libxml2-dev libfftw3-dev libtiff5-dev libhdf5-dev
```

The first three packages `libssl-dev` `libcurl4-openssl-dev` `libssh2-1-dev` are necessary for `devtools` (compilers). `libv8-3.14-dev` is for 
`V8` package to enable JavaScript. `libxml2-dev` is for `xml2`. `libfftw3-dev` `libtiff5-dev` are necessary for fast-fourier transformations and 
`libhdf5-dev` is for reading and writing data in open data format `HDF5`.

4. Install RStudio

Go to https://www.rstudio.com/products/rstudio/download/#download and download `RStudio 1.1.453 - Ubuntu 16.04+/Debian 9+ (64-bit)` to your **desktop** 
and type the folloing command in your terminal

```
cd ~/Desktop
sudo apt-get install libjpeg62
```

Then we can install RStudio

```
sudo dpkg -i ./rstudio-xenial-1.1.453-amd64.deb 
```

and `RStudio` should be in your application list. Again, if you don't know where it is, look at your sidebar in ubuntu, click `search your computer` and enter "RStudio".

5. Install `RAVE`

Open RStudio,

In your RStudio **console**, run:

```
install.packages('devtools')
devtools::install_github('beauchamplab/rave')
```


## B. Toy example


#### Preprocess

To play with **preprocess**, type the following R command 

```
rave_preprocess()
```

* Set `subject code` to be `Subject` 
* Set `project name` to be `Demo`.
* Set `sample rate` to be `2000`
* Set `electrodes` to be `1-6`.

Press `load subject` button

Next, please go through `Notch filter`, `Wavelet` and `Epoch` modules.


#### Main App

To play with **Main**, type:

```
init_app()
```

