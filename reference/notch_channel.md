# Filter line noise out from ECoG channels

Filter line noise out from ECoG channels

## Usage

``` r
notch_channel(s, sample_rate, bands = c(60, 120, 180), width = c(1, 2, 2))
```

## Arguments

- s:

  signal, time domain

- sample_rate:

  signal sample rate

- bands:

  bands that will be filtered out

- width:

  along with bands, half of the filter width. For example,if bands is
  60Hz and width is 1Hz, then the notch filter lower bound is 60-1=59Hz
  and upper bound is 60+1=61Hz.
