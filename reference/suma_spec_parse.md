# Parse `SUMA` `spec` file

Parse `SUMA` `spec` file

## Usage

``` r
suma_spec_parse(subject, spec_file)
```

## Arguments

- subject:

  either characters in format like `'Project/Subject'` or
  [`Subject`](https://beauchamplab.github.io/rave/reference/Subject.md)
  object created by `Subject$new(...)`

- spec_file:

  default decided by `rave_options('suma_spec_file')`, depending on
  subjects

## Examples

``` r
if (FALSE) { # \dontrun{
subject = 'Demo/YAB'
# or create subject object
subject = Subject$new('Demo', 'YAB')

# Please download sample subjects first to run
suma_spec_parse(subject)
} # }
```
