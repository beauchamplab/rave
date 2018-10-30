# Installation Q&A - Linux Version

*Author: Zhengjia Wang*

*Date: Oct 28,2018*

We need to nail down some terms:

* **Terminal**: system terminal. In MacOS, you can find it via spotlight. In Ubuntu, find it by searching apps. In windows, it's cmd.exe, search it in your startup menu.
* **RStudio console**: Open RStudio, in one of four panels (usually bottom-left one), there is a tab called `Console`. By saying "typing in the R console", I mean entering something here. (NOT system terminal)

Topics:

* [Recommended system packages](#recommended-settings) _*Recommended settings to avoid most of errors_
* [How to install `brew` (optional)](#how-to-install-brew) _*If you miss some system packages but don't know where to find_
* Installation Errors and Solutions
  * [Cannot install `devtools`](#devtools) _*To install RAVE from github_
  * [No package `fftw3` found](#fftw) _*Wavelet, Hilbert, Fourier transforms_
  * [Config error for `V8`](#v8) _*Web Service_
  * [Config error for `xml2`](#xml2) _*Dependencies_
  * [`hdf5` not found](#no-hdf5-detected) _*A commonly used data format_
  * [`hdf5` version is too low](#hdf5-requires-at-least-version-xxxxxx)
  * [`httpuv` compiler failure](#httpuv) _*Web Service_

<hr>

### Recommended Settings

I tested under `Ubuntu 16.04` and `CentOS 7`. 

* For debian-based systems (like ubuntu), open system terminal, copy this line and run

```
sudo apt-get install libssl-dev libcurl4-openssl-dev libssh2-1-dev libfftw3-dev libv8-3.14-dev libxml2-dev libhdf5-dev libtiff5-dev
```

* For rpm-based systems (like centos), open system terminal, copy this line and run

```
sudo yum install -y libcurl-devel openssl-devel libssh2-devel fftw-devel v8-devel libxml2-devel hdf5-devel
```

* In case of failure or you have other linux systems, you might want to [install brew first](#how-to-install-brew). Then, use `Linux-brew` to install part of the packages

```
brew install openssl@1.1 fftw v8@3.15 libxml2 hdf5 libssh2
```

Brew might not install `libcurl`, `fftw3`, `V8` and `libssh2` correctly, so you might want to search online or try the following lines to see if you can install them. (you are more than welcome to post any issues to this github repository)

Try:

```
sudo apt-get install libcurl4 libfftw3-dev libv8-3.14-dev
sudo yum install -y libcurl-devel fftw-devel v8-314-devel
```



<hr>

### How to install `brew`

For linux, we recommend installing `brew`, which helps you save time for 3rd-party softwares. [Here](http://linuxbrew.sh/) is the official document of linux-brew. 

Open system terminal, enter:

For Debian or Ubuntu:

```
sudo apt-get install build-essential curl file git
```

For Fedora

```
sudo dnf groupinstall 'Development Tools' && sudo dnf install curl file git
```

For CentOS or Red Hat,

```
sudo yum groupinstall 'Development Tools' && sudo yum install curl file git
```


Then, run

```
sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
```

to download `brew`.

To enable command, copy and paste:

```
test -d ~/.linuxbrew && PATH="$HOME/.linuxbrew/bin:$HOME/.linuxbrew/sbin:$PATH"
test -d /home/linuxbrew/.linuxbrew && PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$PATH"
test -r ~/.bash_profile && echo "export PATH='$(brew --prefix)/bin:$(brew --prefix)/sbin'":'"$PATH"' >>~/.bash_profile
echo "export PATH='$(brew --prefix)/bin:$(brew --prefix)/sbin'":'"$PATH"' >>~/.profile
echo "export PATH='$(brew --prefix)/bin:$(brew --prefix)/sbin'":'"$PATH"' >>~/.bash_profile
echo "export PATH='$(brew --prefix)/bin:$(brew --prefix)/sbin'":'"$PATH"' >>~/.bashrc
brew install hello
```

<hr>

### devtools

```diff
- ERROR: dependencies ‘git2r’, ‘httr’, ‘usethis’ are not available for package ‘devtools’
```

**_Reason_**

This is because `libcurl` and `openssl` are not installed to your system. 

**_Solution:_**

Open system terminal,

* (Debian, Ubuntu, etc) `sudo apt-get install libssl-dev libcurl4-openssl-dev`
* (Fedora, CentOS, RHEL) `sudo yum install -y libcurl-devel openssl-devel`
* (MacOS) `brew install openssl@1.1`

Restart RStudio and try

```
install.packages('devtools')
```

If passed, continue installing `RAVE`

**_Notes:_**

Detailed errors are provided to you. Instructions has been already included in the error code

```
ERROR: dependencies ‘git2r’, ‘httr’, ‘usethis’ are not available for package ‘devtools’

# Relative informations:

# 1. libssh
 * libssh2-1-dev (package on e.g. Debian and Ubuntu)
 * libssh2-devel (package on e.g. Fedora, CentOS and RHEL)
 * libssh2 (Homebrew package on OS X)

# 2. libcurl
 * deb: libcurl4-openssl-dev (Debian, Ubuntu, etc)
 * rpm: libcurl-devel (Fedora, CentOS, RHEL)
 * csw: libcurl_dev (Solaris)

# 3. openssl
 * deb: libssl-dev (Debian, Ubuntu, etc)
 * rpm: openssl-devel (Fedora, CentOS, RHEL)
 * csw: libssl_dev (Solaris)
 * brew: openssl@1.1 (Mac OSX)

# 4. libgit2
 * libgit2-dev   (package on e.g. Debian and Ubuntu)
 * libgit2-devel (package on e.g. Fedora, CentOS and RHEL)
 * libgit2       (Homebrew package on OS X)

```

<hr>

### fftw

```diff
- Error: No package `fftw3` found
```

**_Reason_**

RAVE requires `fftw` package for faster wavelet transforms. Some systems might not have the library.

**_Solution:_**

Open system terminal,

* (Debian, Ubuntu, etc) `sudo apt-get install libfftw3-dev`
* (Fedora, CentOS, RHEL) `sudo yum install -y fftw-devel`

In RStudio console, enter

```
install.packages('fftw')
```

If passed, continue installing `RAVE`

**_Notes:_**

Alternative might be installing `brew` first (see [instructions](#how-to-install-brew)), or if you have brew installed,

```
brew install fftw
```

<hr>

### V8

```diff
- ERROR: configuration failed for package ‘V8’
```

**_Solution:_**

Open system terminal,

* (Debian, Ubuntu, etc) `sudo apt-get install libv8-3.14-dev`
* (Fedora, CentOS, RHEL) `sudo yum install -y v8-devel`
* (MacOS) `brew install v8@3.15`

In RStudio console, 

```
install.packages('V8')
```

If passed, continue installing `RAVE`

**_Notes:_**

Here's detailed error code when I first install this package

```
ERROR: configuration failed for package ‘V8’

------------------------- ANTICONF ERROR ---------------------------
Configuration failed because  was not found. Try installing:
 * deb: libv8-3.14-dev (formerly: libv8-dev) (Debian, Ubuntu)
 * rpm: v8-314-devel (formerly: v8-devel) (Fedora, EPEL)
 * brew: v8@3.15 (OSX) -- NOT regular v8! Tap from homebrew/versions
 * csw: libv8_dev (Solaris)
To use a custom libv8, set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
------------------------- ANTICONF ERROR ---------------------------
```

<hr>

### xml2


```diff
- ERROR: configuration failed for package ‘xml2’
```

**_Solution:_**

Open system terminal,

* (Debian, Ubuntu, etc) `sudo apt-get install libxml2-dev`
* (Fedora, CentOS, RHEL) `sudo yum install -y libxml2-devel`

In RStudio console,

```
install.packages('xml2')
```

If passed, continue installing `RAVE`

**_Notes:_**

Here's detailed error code when I first install this package

```
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because libxml-2.0 was not found. Try installing:
 * deb: libxml2-dev (Debian, Ubuntu, etc)
 * rpm: libxml2-devel (Fedora, CentOS, RHEL)
 * csw: libxml2_dev (Solaris)
If libxml-2.0 is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a libxml-2.0.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘xml2’
```

<hr>

### hdf5r

There might be two errors for this dependency

#### No `hdf5` detected

```diff
- Error: hdf5 does not seem to be installed on your platform. Please install the hdf5 library.
```

**_Reason_**

`hdf5` is a data file format that shares across platform and can be loaded via `R`, `Python`, `Matlab`, `C`, `Julia` and `Java`. `RAVE` uses `hdf5` to store data, which requires `hdf5` library to be installed.

**_Solution:_**

Open system terminal,

* (Debian, Ubuntu, etc) `sudo apt-get install libhdf5-dev`
* (Fedora, RHEL) `sudo yum install -y hdf5-devel`
* (MacOS) `brew install hdf5`

In RStudio console

```
install.packages('hdf5r')
```

If passed, continue installing `RAVE`

**_Notes:_**

Original error message has some misleading information:

1. Using `brew install hdf5` instead of instructed
2. On CentOS simply yum install won't help because rpm only has old `hdf5-1.8.12` for this system.

```
configure: error: hdf5 does not seem to be installed on your platform.
  Please install the hdf5 library.
  The required HDF5 library files can be installed as follows:
      - Debian-based (e.g. Debian >= 8.0, Ubuntu >= 15.04): 'sudo apt-get install libhdf5-dev'
        - Old Debian-based (e.g Debian < 8.0, Ubuntu < 15.04): Install from source (see INSTALL)
        - OS X using Homebrew: 'brew install homebrew/science/hdf5'
        - RPM-based (e.g Fedora): 'sudo yum install hdf5-devel'
ERROR: configuration failed for package ‘hdf5r’
```


#### `hdf5` requires at least version XX.XX.XX

```diff
- configure: error: The version of hdf5 installed on your system is not sufficient. Please ensure that at least version 1.8.13 is installed
```

**_Reason_**

Your system `hdf5` version is too old. To fix it, you need to install `brew`. You might also have `gcc` version that is too low.


**_Solution:_**

Install brew first (see [here](#how-to-install-brew), or reference [official docs](http://linuxbrew.sh/)).

Open your system terminal, check gcc version

```
gcc --version
```

If the version is below 5, install `gcc@5` first

```
brew install gcc@5
```

Next, install hdf5

```
brew install hdf5
```

**Restart** RStudio and try

```
install.packages('hdf5r')
```

If passed, continue installing `RAVE`. 

If fails, you might need to specify `hdf5` library path.

Find `hdf5` path by typing in system terminal:

```
which h5cc
```

or

```
which h5pcc
```

I got result on my computer: `home/linuxbrew/.linuxbrew/Cellar/hdf5/1.10.4/bin/h5cc`

Replace `configure.vars` below with your results.

```
# One of the following two works
install.packages(
  'hdf5r',
  configure.args = '--with-hdf5 [YOUR h5cc PATH]',
  type = 'source'
)

install.packages(
  'hdf5r',
  configure.args = 'with-hdf5',
  configure.vars = '[YOUR h5cc PATH]'
)
```

### httpuv

```diff
- g++: internal compiler error: Killed (program cc1plus).
- compilation failed for package ‘httpuv’
```


**_Reason_**

Usually this is because memory (RAM) is too low and you might want to increase your `RAM` to at least 4GB.

**_Solution:_**

Increase RAM to at lease 4GB. 

If still fail, check [this](https://github.com/rstudio/httpuv/issues/175).








