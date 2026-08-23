# RAVE 1.0 → RAVE 2.0 Migration Plan

> Status: **IMPLEMENTED** (R-code migration + consolidation complete; only
> `_pkgdown.yml` / Title-Description cosmetic cleanup remains — see §10/§11).
> Scope: strip the legacy `rave` (RAVE 1.0) package down to the thin set of
> functions that the modern RAVE 2.0 ecosystem still relies on, delete the rest,
> and repair the helper closure so the package still builds and loads.

---

## 1. Background

The original `beauchamplab/rave` package (RAVE 1.0) has been split into a set of
modern, mostly-CRAN packages:

| New package | Owner | Role |
|-------------|-------|------|
| `dipsaus` | dipterix | low-level utilities |
| `filearray` | dipterix | on-disk arrays |
| `ieegio` | dipterix | neuro file IO |
| `ravetools` | dipterix | signal processing |
| `ravepipeline` | dipterix | options, pipelines, logging |
| `ravecore` | rave-ieeg | subjects, repositories, brain |
| `ravedash` | dipterix | shiny dashboard / app runtime |
| `threeBrain` | dipterix | 3D viewer |
| `ravemanager` | dipterix | installer / launcher |

`rave` (this package) is **not on CRAN** and is **not a declared dependency** of
any of the packages above. As a result, a plain text search for `rave::` does
**not** reveal how the ecosystem still uses this package. The remaining couplings
are deliberately *hidden* (soft / optional), via:

- `asNamespace("rave")$<fun>()` — call into `rave` only if it is installed
- `package_installed("rave")` / `system.file(package = "rave")` — feature gates
- `rave::start_rave(...)` inside CLI launchers and help strings

## 2. Methodology

Usage was determined by scanning the GitHub orgs `rave-ieeg` and `dipterix` for
hidden-coupling patterns (`asNamespace("rave")`, `package_installed("rave")`,
`system.file(package = "rave")`, `R_user_dir("rave", ...)`, `rave::`,
`loadNamespace`/`requireNamespace`) rather than only `rave::`. Matches were then
filtered to **package runtime code**, discarding `adhoc/`, `src/` comments,
scratch scripts, gists, and documentation.

## 3. External usage inventory (the *only* real couplings)

### 3.1 Runtime couplings — MUST keep

| Consumer | Location | Symbol used | Notes |
|----------|----------|-------------|-------|
| `ravecore` | `R/workflow-archive-subject.R` | `asNamespace("rave")$download_sample_data(subject, replace_if_exists=TRUE)` | Only when installing a RAVE 1.0 (`version == 1`) template subject. Guarded by `package_installed("rave")`. |
| `ravemanager` | `inst/shell/launcher.R` | `rave::start_rave(host, port, launch.browser, new, as_job=FALSE)` | RAVE CLI entry point. |
| `ravemanager` | `R/profiles.R` | `rave::start_rave(...)` | Only inside `cli` `.run` help strings (not executed by the package). |
| `ravemanager` | `R/install.R`, `R/check.R` | treats `"rave"` as an installable / version-checked package | `rave` stays a normal installable package; needs to build & load cleanly. |

**User-mandated keepers:** `start_rave`, `start_rave2`, `start_yael`.

### 3.2 Non-runtime usages — NOT required (informational)

These reference `rave` but are **test-only or scratch**, are skipped when `rave`
is absent, and do **not** constrain this migration:

- `rave-ieeg/rave-pipelines` `tests/testthat/*`: `rave::rave_prepare`,
  `rave::rave_preprocess_tools` (RAVE 1.0-vs-2.0 comparison tests; skipped when
  `rave` is not installed).
- `dipterix/rave-gists/*.R`, `*/adhoc/*`, `*/src/*` comments: `rave::start_rave`,
  `rave_prepare`, `rave:::rave_brain2`, `Subject`, `ModuleEnvir`, `comp_parser`,
  `fake_session`, `afni_tools`, `wavelet`, `baseline`, `init_app`, … — all in
  scratch/scripts/comments.

> Decision: do **not** retain `rave_prepare` / `rave_preprocess_tools` for the
> sake of `rave-pipelines` legacy comparison tests. Those tests already require a
> manual RAVE 1.0 install and are not part of CRAN CI. This is called out as a
> risk in §8.

### 3.3 Confirmed NON-dependents

`ravedash`, `ravepipeline`, `ravetools`, `ieegio`, `filearray`, `threeBrain`,
`dipsaus` contain **no** runtime reference to the `rave` namespace.

### 3.4 `rutabaga` and `ravebuiltins` (extended audit)

- **`dipterix/rutabaga`** — fully decoupled. `Imports: utils, stats, grDevices,
  graphics, methods, magrittr, stringr, digest, dipsaus`. No reference to `rave`
  anywhere. No impact on this plan.
- **`beauchamplab/ravebuiltins`** (v0.1.1, RAVE 1.0 "Builtin Modules") — declares
  `@import rave` in its `NAMESPACE`, and its `R/aaa.R` / `R/utils.R` call
  `rave::reload_module_package`, `rave::load_rave_module_package`,
  `rave::rave_context`, `rave::rave_options`, `rave::getDefaultDataRepository`
  (all inside function bodies / lazy default args, resolved at call-time, not at
  load-time). Its `inst/tools/` and `inst/modules/` are the RAVE 1.0 module
  runtime (using `get_module`, `customizedUI`, `define_input`, `progress`,
  `diagnose_signal`, `pwelch`, `rave_brain2`, `parse_components`,
  `mount_demo_subject`, `safe_write_csv`, `check_subjects2`, `lapply_async`,
  `init_app`, `close_tab`, `get_rave_theme`, …).
  - **`ravebuiltins/inst/tools` is NOT used by `rave-ieeg/rave-pipelines`**
    (search returns empty; `define_output_3d_viewer` unused). The modern pipelines
    fully switched to `ravepipeline` / `ravedash`.
  - The **only** `ravebuiltins` usage in modern `rave-pipelines` is three *pure*
    exported palette helpers from `ravebuiltins/R/common_plotting_functions.R` —
    `get_heatmap_palette`, `expand_heatmap`, `get_palette` — none of which touch
    `rave`.
  - **Consequence for this plan:** no additional functions need to be retained.
    The single requirement is that `rave` remains a **loadable / installable**
    package so that `loadNamespace("ravebuiltins")` (which does `import(rave)`)
    keeps succeeding. The keep-set in §4 already satisfies this. The RAVE 1.0
    module machinery that `ravebuiltins/inst/*` would need is intentionally
    dropped (those legacy modules are superseded by `rave-pipelines`).

## 4. Keep-set (target surface)

After migration the package exposes a thin surface. Dependency closures were
verified to be light (no legacy machinery).

| Exported symbol | Source file (target) | Closure |
|-----------------|----------------------|---------|
| `start_rave`, `start_rave2`, `start_yael` | `R/start.R` (renamed from `shinyUI-main_app.R`) | `ravedash::start_session` only |
| `download_sample_data`, `download_subject_data` | `R/download.R` (extracted from `io-subject.R`) | `catgl`, `dir_create`, `rave_options`, `%?<-%`, `ravepipeline::load_yaml` |
| `rave_options`, `save_options` | `R/options.R` (from `core-options.R`) | `ravepipeline::raveio_getopt/setopt` (GUI branch removed) |
| `rave_version` | `R/zzz.R` | `utils::packageVersion` |
| `arrange_data_dir` | `R/options.R` | `rave_options`, `catgl` (light dir check used by `.onAttach`) |

Internal helpers retained: `dir_create`, `get_val` (`R/utils.R`),
`stopifnot2`, `%within%` (if still referenced), the roxygen `@import` block.

Optional small keepers (no heavy deps; keep only if desired): `open_tab`,
`close_tab`.

## 5. Delete-set

### 5.1 Whole files to delete (legacy RAVE 1.0 machinery)

Classes & runtime:
`class-ECoGRepository.R`, `class-Electrode.R`, `class-ExecEnvir.R`,
`class-ModuleEnvir.R`, `class-RaveFinalizer.R`, `class-RaveHistory.R`,
`class-RawSubject.R`, `class-Subject.R`, `core-data_repo.R`,
`legacy-cls_module.R`, `legacy-func_rave_prepare.R`.

Modules / parsing / config:
`core-modules.R`, `core-module_tools.R`, `module-conf.R`, `module-misc.R`,
`module-parse.R`, `module-viewdebug.R`, `rstudio-template.R`.

Generics (S3 context system):
`generics-core.R`, `generics-default.R`, `generics-define_initialization.R`,
`generics-define_input.R`, `generics-define_output.R`,
`generics-eval_when_ready.R`, `generics-input.R`, `generics-load_scripts.R`,
`generics-output.R`, `generics-shiny.R`.

Signal processing (now `ravetools`):
`signal-decimate.R`, `signal-misc.R`, `signal-notch.R`, `signal-plots.R`,
`signal-pwelch.R`, `signal-wavelet.R`, `core-baseline.R`, `export-plots.R`.

Imaging / brain (now `ravecore` / `threeBrain`):
`core-brain_model.R`, `core-localization.R`.

Pre-processing (now `ravecore`):
`core-preprocess.R`, `core-shinirize.R`.

Shiny UI / modules (now `ravedash` / `rave-pipelines`):
`shinyModule-preprocess-epoch.R`, `shinyModule-preprocess-notch.R`,
`shinyModule-preprocess-overview.R`, `shinyModule-preprocess-wavelet.R`,
`shinyUI-data_selector.R`, `shinyUI-debug_app.R`, `shinyUI-preprocess.R`,
`shinyUI-rave_options.R`.

IO (now `ieegio` / `ravecore`):
`io-afnisuma.R`, `io-csv.R`, `io-matlab.R`, `io-misc.R`.

Checks & misc utils (now `dipsaus` / `ravepipeline`):
`checks.R`, `checks2.R`, `import-pryr.R`, `utils-cache.R`, `utils-parallel.R`,
`utils-print.R`, `utils-progress.R`, `utils-rstudio.R`, `utils-shiny.R`,
`utils-shinydashboard.R`.

### 5.2 Files to split (keep a few functions, delete the rest)

| File | Keep | Delete |
|------|------|--------|
| `shinyUI-main_app.R` → `R/start.R` | `start_rave`, `start_rave2`, `start_yael` (+ optional `open_tab`, `close_tab`) | `app_controller`, `app_ui`, `app_server`, `launch_demo`, `start_rave_legacy` |
| `io-subject.R` → `R/download.R` | `download_sample_data`, `download_subject_data` | `load_meta`, `save_meta`, `archive_subject`, all importers / subject IO |
| `core-options.R` → `R/options.R` | `rave_options`, `save_options` | `rave_options_gui` usage, `ugly_sample`, `rave_setup_workers` |
| `core-file_structure.R` → `R/options.R` | `arrange_data_dir` | `arrange_modules`, `get_subjects`, `get_dir`, `test_hdspeed` |
| `utils-misc.R` → `R/utils.R` | `dir_create`, `get_val` | `is.blank`, `is_invalid`, `zero_length`, `try_normalizePath`, … (unless referenced by keep-set) |
| `aaa.R` | roxygen `@import` block, `stopifnot2`/`%within%` *iff* referenced | `rave_context*`, `rave_debug`, `get_conf`/`set_conf`, `toggle_debug`, `soft/hard_deprecated`, `subject_cache_dir`, `MNI305_to_MNI152`, … |
| `zzz.R` | `.onLoad`, `.onAttach`, `.onUnload`, `rave_version`, `latest_version`, `restart_r` | `check_dependencies`, `check_dependencies2`, `finalize_installation`, `finalize_installation_internal_demo` |

### 5.3 Exports to drop

All `S3method(...)` for the deleted generics (`define_input`, `define_output`,
`define_initialization`, `eval_when_ready`, `getDefaultReactive*`,
`get_rave_theme`, `init_module`, `load_scripts`, `mount_demo_subject`,
`rave_checks`, `reload_module_package`, `cache`, …) and every `export(...)`
except the keep-set in §4 (plus `download_subject_data`). NAMESPACE is
regenerated, never hand-edited.

## 6. Helper fixes ("missing helpers" after deletion)

Deleting the files above leaves dangling references in the retained load hooks.
Each must be repaired:

1. **`.onLoad`** currently calls `rave_hist()` (from deleted `class-RaveHistory.R`)
   and the version-bump bookkeeping. → Strip to a minimal/no-op body (option
   management now belongs to `ravepipeline`).
2. **`.onUnload`** currently calls `clear_env(data_repository)` (from deleted
   `legacy-cls_module.R`). → Remove the body (no-op) or drop `.onUnload`.
3. **`rave_options(launch_gui = TRUE)`** dispatches to `rave_options_gui` (from
   deleted `shinyUI-rave_options.R`). → Remove the GUI branch; when called with
   no key it returns invisibly / emits a short message pointing to
   `ravemanager::version_info()`.
4. **`.onAttach`** references `arrange_data_dir`, `latest_version`,
   `rave_version`, `rave_options` — all retained and light. Guard
   `latest_version` (uses `raveio::load_json`, `raveio` is Suggests) in
   `tryCatch`.
5. Confirm `download_subject_data` references only retained/imported helpers
   (`catgl`, `dir_create`, `rave_options`, `%?<-%`, `ravepipeline::load_yaml`) —
   verified, no fix needed.
6. **`catgl` (discovered during implementation).** The old code called `catgl`
   unqualified under `@import dipsaus`, but current `dipsaus` no longer exports
   `catgl` (it now lives *internal* to `ravepipeline` and exported by `raveio`).
   `download_subject_data` (the function `ravecore` calls) uses `catgl`
   throughout, so a missing `catgl` would break it at the first message. Fix: a
   small local `catgl` in `R/utils.R` that delegates to the exported
   `ravepipeline::glue` + `ravepipeline::logger` and preserves the original
   `FATAL -> stop` semantics. `%?<-%` still resolves via `@import dipsaus`.

## 7. Package metadata changes

- **DESCRIPTION** — drop now-unused `Imports`: `signal`, `fftwtools`,
  `shinyFiles`, `shinyWidgets`, `DT`, `startup`, `digest`, `shinyjs`, `grid`,
  `devtools`. Retain: `ravecore`, `ravepipeline`, `ravedash`, `dipsaus`,
  `stringr`, `rlang`, `future`, `stats`, `utils`. Move `threeBrain`, `raveio`
  to `Suggests` (used by `start_yael` / `latest_version`). Re-audit `Suggests`.
- **NAMESPACE** — regenerate with `devtools::document()` after edits.
- **man/** — delete `.Rd` for removed exports (regenerated; never hand-edited).
- **tests/** — delete all of `tests/testthat/*` (they exercise removed
  machinery: `test.tensor.R`, `test.preprocess.R`, `test.subjectLoad.R`,
  `test.checks.R`, `test.moduledev.R`, `test.aaa.R`) and replace with a minimal
  smoke test (package loads; `start_rave`, `download_sample_data` exist).
  Keep `tests/testthat.R`, `tests/spelling.R`.
- **inst/** — remove RAVE 1.0 scaffolding no longer shipped:
  `default_module.R`, `module_addons/`, `template/`, `rstudio/`, `markdowns/`,
  `third_party/`, `utils/`, `hdf5_installer.sh`, `packages.txt`,
  `palettes.yaml`, `settings.yaml`. Keep `CITATION`, `WORDLIST`, and any asset
  still referenced by `start_*`. (Audit `assets/`, `webr-rave/` before removal.)
- **api/**, `adhoc/`, `docker/`, `*.md` install guides — review/trim separately
  (out of scope for the R surface; flagged for a follow-up pass).

## 8. Risks & validation

**Risks**
- `rave-pipelines` legacy comparison tests call `rave::rave_prepare` /
  `rave::rave_preprocess_tools`. Removing them disables those (already-skipped,
  non-CI) tests. Accepted; documented here.
- Any private consumer using unexported internals (`rave:::…`) will break. Only
  observed in scratch/`adhoc` code — accepted.
- `download_subject_data` writes into `rave_options('data_dir')`; ensure
  `ravepipeline` option keys (`data_dir`, `raw_data_dir`) resolve at runtime.

**Validation steps**
1. `devtools::document()` — regenerate NAMESPACE + man/.
2. `devtools::load_all()` — package loads with no missing-symbol errors.
3. `R CMD check` (or `devtools::check()`) — no errors from dangling refs.
4. `lintr::lint_package()` — style (semicolons, no dot-prefixed names).
5. Smoke: `exists("start_rave")`, `exists("download_sample_data")`;
   `asNamespace("rave")$download_sample_data` resolves;
   `rave::start_rave` signature accepts `host, port, launch.browser, as_job, ...`.

## 9. Execution order

1. Write this plan (done).
2. Create `R/start.R`, `R/download.R`, `R/options.R`, `R/utils.R` with the
   extracted keep-set functions.
3. Trim `aaa.R` and `zzz.R`; apply the §6 helper fixes.
4. Delete the §5.1 files and the leftover originals that were split.
5. Delete `tests/testthat/*`; add minimal smoke test.
6. Prune `inst/` (§7) after auditing asset references.
7. Update `DESCRIPTION`.
8. `devtools::document()` → regenerate NAMESPACE + man/ (prune stale `.Rd`).
9. `devtools::load_all()` / `devtools::check()` / `lintr::lint_package()`.

## 10. Implementation status

**Done**
- `R/` reduced from 70 files to 6: `start.R`, `download.R`, `options.R`,
  `utils.R`, `aaa.R`, `zzz.R` (69 obsolete files deleted).
- Keep-set in place: `start_rave`, `start_rave2`, `start_yael`, `open_tab`,
  `close_tab`, `download_sample_data`, `download_subject_data`, `rave_options`,
  `save_options`, `arrange_data_dir`, `rave_version`, `get_val` (+ internal
  `dir_create`, `is_invalid`, `catgl`, `latest_version`).
- Load hooks repaired: `.onLoad` / `.onUnload` removed (they referenced the
  deleted `rave_hist` / `data_repository` / `clear_env`); `.onAttach` trimmed of
  its `rave_hist` block and the removed-GUI hint; `rave_options` GUI branch
  replaced (§6.3); local `catgl` added (§6.6).
- `tests/testthat/*` (RAVE 1.0 machinery) removed; `test-smoke.R` added.
- `DESCRIPTION` trimmed to `Imports: utils, dipsaus, ravedash, ravepipeline,
  shiny, stringr`; `Suggests: threeBrain, raveio, spelling, testthat`.
- `NAMESPACE` + `man/*.Rd` regenerated via roxygen (stale `.Rd` pruned; 11 topics
  remain).
- Verified: `roxygenise()` OK, `devtools::test()` 11/11 pass,
  `codetools::checkUsagePackage` reports no undefined globals,
  `R CMD INSTALL` succeeds and the installed package loads.

**Remaining (optional follow-ups, not yet done)**
- Update `_pkgdown.yml` reference index (still lists removed topics; only affects
  pkgdown site generation, not `R CMD check`).
- Optionally refresh the `Title`/`Description` (still describe RAVE 1.0 HDF5/Matlab
  capabilities).

## 11. Round 2 — consolidation, `dipsaus` removal, and minimal exports

Follow-up requested after §10: hide as much surface as possible, drop the
`dipsaus` dependency, prune `inst/`, and collapse `R/` to a single file.

### 11.1 Extended `inst/` / dependency audit
- `rutabaga` and `ravebuiltins` were re-audited (see §3.4). `ravebuiltins`'s
  `rave::rave_options` lives only in a dead RAVE 1.0 demo-path helper, not in the
  live palette functions `rave-pipelines` uses.
- No live package references `rave`'s `inst/` (no `system.file(package = "rave")`;
  `inst/webr-rave` — a committed local experiment — is referenced nowhere).

### 11.2 `inst/` pruned
Deleted all RAVE 1.0 scaffolding: `assets/`, `default_module.R`,
`hdf5_installer.sh`, `markdowns/`, `module_addons/`, `packages.txt`,
`palettes.yaml`, `rstudio/`, `settings.yaml`, `template/`, `third_party/`,
`utils/`, `webr-rave/`. **Kept** `CITATION` and `WORDLIST`.

### 11.3 `dipsaus` removed from `DESCRIPTION`
- The only `dipsaus` usage left in `R/` was the `%?<-%` operator (plus the
  `@import dipsaus` tag). `%?<-%` was copied verbatim from `dipsaus`
  `R/language.R` into the package as an internal operator (the author maintains
  `dipsaus`; no license/authorship concern). `catgl` was already a local helper.
- Every remaining external call is namespace-qualified (`ravedash::`,
  `ravepipeline::`, `shiny::`, `stringr::`, `utils::`), so **no** `@import` /
  `@importFrom` is needed. `Imports:` is now `utils, ravedash, ravepipeline,
  shiny, stringr`. (`dipsaus` is still loaded transitively via `ravedash` /
  `ravepipeline`, but is no longer a direct dependency.)

### 11.4 Exports reduced to three
- Only `start_rave`, `start_rave2`, `start_yael` remain exported (the `rave::`
  entry points used by `ravemanager`, `rave-pipelines`, `rpyANTs`).
- Everything else is now **internal** (no `@export`). This is safe because:
  `ravecore` reaches `download_sample_data` via `asNamespace("rave")$...`, which
  resolves internal objects; and the other historical `rave::` calls
  (`rave_options`, `close_tab`, …) exist only in dead RAVE 1.0 module code.
- Internal functions use plain `#` comments (not roxygen `#'`), so no `man/*.Rd`
  is generated for them. `man/` is now just `rave-package.Rd`, `start_rave.Rd`,
  `start_yael.Rd`.

### 11.5 `R/` collapsed to one file
All six files were merged into a single [`R/obsolete.R`](R/obsolete.R) (the code
is expected to phase out over time). `tests/testthat/test-smoke.R` was updated to
assert the 3-export surface, the namespace-internal helpers, and the
`asNamespace("rave")$download_sample_data` access pattern.

### 11.6 Validation
`roxygenise()` → `NAMESPACE` has exactly three `export()` lines and no `import()`;
`devtools::test()` 15/15 pass; `codetools::checkUsagePackage` reports only benign
style notes (no undefined globals); `R CMD INSTALL` succeeds; post-install
`getNamespaceExports("rave")` == the three launchers, `dipsaus` absent from
`Imports`, and `asNamespace("rave")$download_sample_data` resolves.
