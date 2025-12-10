# Electrode localization

Electrode localization

## Usage

``` r
electrode_localization(subject_code, freesurfer_path, ct_path, ...)
```

## Arguments

- subject_code:

  'RAVE' subject code

- freesurfer_path:

  'FreeSurfer' folder path that points to reconstructed subject brain

- ct_path:

  'CT' path (in 'Nifti' format). The 'CT' has to be aligned to 'T1-MRI'.
  Please check [this
  tutorial](https://dipterix.org/threeBrain/articles/B-electrode-localization.html#ct-co-registration).

- ...:

  other parameters passing to
  [`localization_module`](https://dipterix.org/threeBrain/reference/localization_module.html)

## Value

This function will launch a shiny application.
