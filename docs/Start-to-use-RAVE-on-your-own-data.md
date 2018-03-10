**This section tells you how to configure `RAVE` for your own projects.**

* [Data Repository](#data-repository) (Add your own data to RAVE repository)
* [Preprocess](#preprocess) (Convert raw data to RAVE format)

After installing `RAVE`, if you type `init_app()`, `RAVE` will launch an example module called `Condition Explore` in your default browser, with a demo subject `Subject_RAVE_Demo`. If you ask questions such as "how can I use RAVE on my data?" or "How am I going to mount a module?". Then you probably want to read this section.

# Data Repository

### Step 1: Find your data repository

Go to your RAVE root folder. If you just installed `RAVE` It's usually under `~/`

* in MacOS, you can type `Command`(&#8984;)+`Space` to open your spotlight, type `~/` and press `Enter`. 
* In Windows, it's probably located at `C:\Users\[username]\` or `C:\Users\[username]\Documents\`)

There are two folders `rave_modules` and `rave_data`. `rave_data` is your data repository.

### Step 2: File Hierarchy

Open `rave_data`, and you will see two sub-folders `data_dir`, `raw_dir`. 

#### `data_dir` - RAVE Data Directory

This folder stores subject processed data, ready for `RAVE` to use. It will be handled by RAVE and you don't need to worry too much about it. The only two situations when you will need to change it is when you want to add `SUMA` files, or when you are adding epoch files. There are two articles telling you how to [enable SUMA](rave-for-suma) and how to [create epochs](preprocess#epoching-raw-data).

#### `raw_dir` - Raw Data Directory

`raw_dir` is the folder where you put your raw subject files. You need to manage your original ECoG data here with proper file hierarchy. The following paragraphs will be introducing you how to manage this folder.

If you look inside, there should be a `Subject` folder in it. The folder name is your subject code. Inside of this folder, there is an block folder `008` and within block folder, you have six sample files with name `SubjectDatafile008_ch[1-6].mat`

Let's have an example, say you have a subject `YAY`, it has three experiment blocks (`008`, `010`, and `012`) and 126 electrodes (channels):

* Project name: `Demo`
* Subject code: `YAY`
* Experiment blocks: `008`, `010`, `012`
* Number of channels: 126

*Folder structure (you need to change some of the names):*

```
  raw_data/
  |      ... (other subjects)
  |
  + ---- YAY/
         |
         + ---- 008/
         |      |
         |      \ ---- YAYDatafile008_ch1.mat
         |             YAYDatafile008_ch2.mat
         |             ...
         |             YAYDatafile008_ch126.mat
         |
         + ---- 010/
         |      |
         |      \ ---- YAYDatafile010_ch1.mat
         |             YAYDatafile008_ch2.mat
         |             ...
         |             YAYDatafile008_ch126.mat
         |
         + ---- 012/
                |
                \ ---- YAYDatafile008_ch1.mat
                       YAYDatafile008_ch2.mat
                       ...
                       YAYDatafile008_ch126.mat
          

```

**Important**: 

* Your Matlab file format should be `[SUBJECT_CODE]Datafile[BLOCK]_ch[CHANNEL].mat`
* Channel number must starts from `1`

Once settled, open preprocess module by type 

```
rave::rave_pre_process()
```

Enter your project name `Demo`, and subject code `YAY` and press button `Load`.

# Preprocess

You can use R script `rave::rave_pre_process()`, or

```
require(rave)
rave_pre_process()
```

to enter preprocess app. There are three major steps here:

* [Notch filter](#notch-filter): filter out 60 Hz (&plusmn;1), 120 Hz (&plusmn;2), 180 Hz (&plusmn;2)
* [Common average reference](#common-average-reference): Select channels that are valid, calculate mean as reference signal
* [Wavelet](#wavelet) transformation: Perform wavelet, generating both power and phase blocks
* [Epoching](#epoching): Generate epoch files, telling RAVE when the trails start

Before starting any process, you need to load subject data first:

> Step 1: Enter subject code and project name

![Enter subject code and project name](https://github.com/dipterix/instrave/blob/master/img/preprocess/1.PNG)

> Step 2: Press "Load" button

![Load button](https://github.com/dipterix/instrave/blob/master/img/preprocess/2.PNG)

> Step 3: Enter channel, blocks, sample rate

![Enter channel, blocks, sample rate](https://github.com/dipterix/instrave/blob/master/img/preprocess/3.PNG)

This information will freeze once Notch filter is applied. Please double check, and press "Update Changes" button.

**Important**: channels must go from `1` consecutively to the maximum channel number. 

### Notch filter

Go to `Notch Filter` section, press "Apply Notch Filter" once you saved channel & block information in the previous steps. Confirm the dialogue, and have half cup of coffee, then you will see the following UI. You can go through all the blocks and channels to find bad/epilepsy channels. If you run `RAVE` on a local machine, you can also export all the channels as PDF files.

![Notch filter signal plots](https://github.com/dipterix/instrave/blob/master/img/preprocess/notch1.PNG)

Once finished preliminary screening, go to the next step (CAR)

### Common average reference

Load notch filtered signals (this might take a while for the first time), and you will see the CAR plot with every channels lined up:

![CAR plot](https://github.com/dipterix/instrave/blob/master/img/preprocess/car1.PNG)

_TODO: Add descriptions about this module_

Once finished screening, you can press button `Calculate Average` to generate CAR signal, then you will see a select box appear, with your current reference channels as follows. Also this CAR signal will be rendered in the CAR plot area, with **azure color**.

![CAR plot](https://github.com/dipterix/instrave/blob/master/img/preprocess/car2.PNG)

_* In this case, there are only seven channels and I didn't exclude any one_

After `Apply CAR`, go to `Post-CAR` tab, and you can see pre- vs. post-CAR comparisons

![CAR plot](https://github.com/dipterix/instrave/blob/master/img/preprocess/car3.PNG)

_* If you are unsatisfied with some channels, you can always go to `Globals` and change settings. However, you need to **Re-run** CAR to calculate again after changing settings._

Once you are satisfied with CAR results, go to `Wavelet` and perform wavelet in preprocessing module.

### Wavelet

Before applying wavelet, you need to check the channels to be applied by looking at top part of wavelet input box. 

![Wavelet sidebar](https://github.com/dipterix/instrave/blob/master/img/preprocess/wavelet1.PNG)

There are a few options for wavelet:

* __Frequency range__: Range of frequencies to apply wavelet
* __Frequency step size__: By default 2, this means if your frequency range is [2,200], your final power frequencies will be 2, 4, 6, ..., 200
* __Number of wavelet cycles__: Default 7, for phase resolution
* __Target sample rate__: Default 100. The result will be down-sampled to 100 Hz. 
* __Parallel, number of cores__: Default is number of cores your CPU has, you can adjust this number to any integer from 1 to the default number. For example, your CPU has 4 cores, you can set 3 cores here and leave one core to edit your paper, watch movies etc.

After all set, press `Run Wavelet` and wait, or go home :) Once wavelet is finished, a pop-up message will inform you. Then you can go to `Post-Wavelet` tab to view the results

![Wavelet sidebar](https://github.com/dipterix/instrave/blob/master/img/preprocess/wavelet2.PNG)

### Epoching

_You need to edit epoching files on your own. TODO: define epoching file format here, and write example script(?)_
