# Start 'YAEL' electrode localization

Start 'YAEL' electrode localization

## Usage

``` r
start_yael(
  host = "127.0.0.1",
  port = NULL,
  launch.browser = TRUE,
  as_job = FALSE,
  ...
)
```

## Arguments

- host:

  host IP address

- port:

  integer port number; default is random

- launch.browser:

  whether to launch browsers

- as_job:

  whether to launch in background; available only in 'RStudio'

- ...:

  passed to
  [`start_session`](https://dipterix.org/ravedash/reference/rave-session.html)
