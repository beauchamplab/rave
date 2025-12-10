# Apply Notch Filter to Analog Trace Data

Apply Notch Filter to Analog Trace Data

## Usage

``` r
notch_filter(s, sample_rate, lb, ub, domain = 1)
```

## Arguments

- s:

  signal in time or frequency domain

- sample_rate:

  signal sample rate

- lb:

  filter lower bound (Hz)

- ub:

  filter upper bound (Hz)

- domain:

  1 if the input signal is in the time domain, 0 if it is in the
  frequency domain

## Value

filtered signal in time domain

## Details

This function is alternative R version of notch filter
