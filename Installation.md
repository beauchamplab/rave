# Prerequisite Installation Guide for RAVE for Mac, Windows and Linux

Click on your operating system.

+ [Mac OS](#macos)
+ [Windows (Windows 10, with Bash enabled)](#windows)
+ [Ubuntu 16.04+ ](#ubuntu)



## MacOS

1. First, go to Cran-R official website and download install the latest version of R:

[https://cran.r-project.org/bin/macosx/](https://cran.r-project.org/bin/macosx/)

2. After installing R, make sure that you install Xcode from the Mac App Store: ( [here](https://www.moncefbelyamani.com/how-to-install-xcode-homebrew-git-rvm-ruby-on-mac/) is a helpful article in case of errors)

Open Terminal (in the Applications folder), enter

```
xcode-select --install
```

Click **yes** to proceed installing command-line tools. On first use, you will br required to read and accept the Apple license.  

*Don't worry if the following error occurs. It just means you have already installed xcode command line tools.*

```javascript
// xcode-select: error: command line tools are already installed, use "Software Update" to install updates
```

3. Install the free version of RStudio Desktop here:

[https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)

## Windows

You will be asked many questions by the installers; the default response is fine for all of them.
1. Install the latest version of R

[https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)

2. Install the latest version of RTools. The version of RTools must match your version of R. 

[https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)

3. Download and install the free version of RStudio Desktop for Windows. 

[https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)

## Ubuntu

1. Add R-Cran repository to your app list:

Open terminal (if you don't know how, look at your sidebar in ubuntu, `search your computer` enter "terminal", and open it), type the following code:

```
sudo gedit /etc/apt/sources.list
```

Then there will be a text editor with lots of "deb"s in it. Append the following line at the end of this file


```
deb https://cloud.r-project.org/bin/linux/ubuntu xenial-cran35/
```

**IMPORTANT** Based on you system, you might want to enter different repositories (https://cran.r-project.org/bin/linux/). However, this line should be with a format of `deb` + `[repository URL]` + `xenial` (or `xenial-cran35`) + `/`. The difference between `xenial` and `xenial-cran35` will affect the version of R to be installed.

Save the file and close text editor. 

2. In your terminal, update `apt-get` repository:

```
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
sudo apt-get update
```

The first line gives you access to the new repository (it adds keys to your computer so that you can safely download R). 

3. Install R

Copy the following command line by line into your terminal and run

```
sudo apt-get install r-base
```

```
sudo apt-get install r-base-dev
```

4. Install compiling tools and system dependencies

After installing R, copy the following command to your terminal and run:

```
sudo apt-get install libssl-dev libcurl4-openssl-dev libssh2-1-dev libv8-3.14-dev libxml2-dev libfftw3-dev libtiff5-dev libhdf5-dev
```

The first three packages `libssl-dev` `libcurl4-openssl-dev` `libssh2-1-dev` are necessary for `devtools` (compilers). `libv8-3.14-dev` is for 
`V8` package to enable JavaScript. `libxml2-dev` is for `xml2`. `libfftw3-dev` `libtiff5-dev` are necessary for fast-fourier transformations and 
`libhdf5-dev` is for reading and writing data in open data format `HDF5`.

5. Install the free version of RStudio Desktop here:

Go to https://www.rstudio.com/products/rstudio/download/#download and download one with keywords "Ubuntu 16.04+/Debian 9+ (64-bit)", move the downloaded file to your **desktop**, rename it "rstudio.deb".

Open terminal, type the folloing command in your terminal

```
cd ~/Desktop
sudo apt-get install libjpeg62
```

Then you can install RStudio.

```
sudo dpkg -i ./rstudio.deb 
```

and `RStudio` should be in your application list. Again, if you don't know where it is, look at your sidebar in ubuntu, click **search your computer** and enter "RStudio".

Other linux systems might need to check [recommended system packages](./inst/markdowns/Installation_questions.md#recommended-settings)

