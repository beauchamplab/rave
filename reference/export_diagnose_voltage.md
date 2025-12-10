# Export voltage (analog trace) diagnostic plots for each electrode

You must import subject through
[`rave_preprocess()`](https://beauchamplab.github.io/rave/reference/rave_preprocess.md)
and then run this function

## Usage

``` r
export_diagnose_voltage(
  subject,
  electrodes,
  blocks,
  save_dir = "./export",
  width = 12,
  height = 7,
  useDingbats = FALSE,
  onefile = TRUE,
  winlen,
  freq_lim,
  nclass = 50,
  fore_col = "black",
  back_col = "grey80",
  ...
)
```

## Arguments

- subject:

  character with the following format: `"project_name/subject_code"`

- electrodes:

  a integer vector. For example: `c(1,3,4,10:15)`

- blocks:

  the blocks to include. Default is all blocks of this subject

- save_dir:

  the directory you want to save the files

- width, height, useDingbats:

  passed to [`pdf`](https://rdrr.io/r/grDevices/pdf.html)

- onefile:

  collect images within one file?

- winlen:

  window length, default is twice of the subject sampling rate

- freq_lim:

  default is half of voltage sampling rate

- nclass:

  number of classes in histogram plot, default is 50

- fore_col:

  Periodogram color for Notch filtered signals

- back_col:

  Periodogram color for raw signals

- ...:

  All other parameters passed to
  [`pdf`](https://rdrr.io/r/grDevices/pdf.html)

## See also

[`pdf`](https://rdrr.io/r/grDevices/pdf.html),
[`diagnose_signal`](https://beauchamplab.github.io/rave/reference/diagnose_signal.md),
[`pwelch`](https://beauchamplab.github.io/rave/reference/pwelch.md)
