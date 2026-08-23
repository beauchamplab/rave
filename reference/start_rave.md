# Start 'RAVE' main application

Start 'RAVE' main application

## Usage

``` r
start_rave2(
  host = "127.0.0.1",
  port = NULL,
  launch.browser = TRUE,
  jupyter = FALSE,
  as_job = FALSE,
  ...
)

start_rave(
  host = "127.0.0.1",
  port = NULL,
  launch.browser = TRUE,
  jupyter = FALSE,
  as_job = FALSE,
  ...
)
```

## Arguments

- host:

  host IP address; default is `"127.0.0.1"`

- port:

  integer port number; default is random

- launch.browser:

  whether to launch browser; default is true

- jupyter:

  whether to launch the 'Jupyter' server; default is false

- as_job:

  whether to launch in the background as an 'RStudio' job; available
  only in 'RStudio'

- ...:

  passed to
  [`start_session`](https://dipterix.org/ravedash/reference/rave-session.html)

## Value

A 'shiny' application object (invisibly when launched as a job).

## See also

[`start_session`](https://dipterix.org/ravedash/reference/rave-session.html)

## Examples

``` r

if (interactive()) {
 
start_rave()

}

```
