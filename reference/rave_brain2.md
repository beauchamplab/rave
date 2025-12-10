# Tools to load and view brain in 3D viewer

Tools to load and view brain in 3D viewer

## Usage

``` r
rave_brain2(
  subject,
  surfaces = "pial",
  use_141 = TRUE,
  recache = FALSE,
  clean_before_cache = FALSE,
  compute_template = FALSE,
  usetemplateifmissing = FALSE
)
```

## Arguments

- subject:

  character or \`RAVE\` subject instance

- surfaces:

  one or more from `"pial"`, `"white"`, `"smoothwm"`, brain surface
  types

- use_141:

  logical, whether to use standard 141 brain

- recache:

  whether to force cache data, default is false

- clean_before_cache:

  whether to clean cache before redo cache, default is false

- compute_template:

  logical whether to compute nearest 141 node. Please also check
  `freesurfer_brain`.

- usetemplateifmissing:

  whether logical, to display template brain if subject brain not found,
  default is false
