
# `proj.lapply`

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/patter)](https://CRAN.R-project.org/package=patter)

`proj.lapply` is a package in the
[`proj.verse`](https://github.com/edwardlavender/proj.verse) ecosystem.

# Installation

The [`proj.verse`](https://github.com/edwardlavender/proj.verse) family
of packages can be installed with:

``` r
install.packages("devtools")
devtools::install_github("edwardlavender/proj.verse", 
                         dependencies = TRUE)
```

# Functionality

In base `R`, the function `lapply(list, function)` is a key routine that
applies a function iteratively, returning a `list`. This is conveniently
extended by `pbapply::pblapply()` with parallelisation and a progress
bar. The core routines in this package extend `pbapply::pblapply()`.

**`cl_lapply()`** extends `pbapply::pblapply()` by:

- Handling cluster set up, implementation and closure;
- Optional chunk-wise parallelisation for improved efficiency;

**`cl_lapply_workflow()`** extends `cl_lapply()` with additional
functionality for iterative applications of more complicated workflows,
by:

- Sending user output to the console or a `log.txt` file;
- Error handling around the user-defined `function`;
- Recording call statistics (such as success and computation time);
- Optionally writing `function` outputs to disk to handle memory
  requirements;
- Implementing optional coffee breaks that give your computer a rest;

# Code of conduct

Please note that the
[`proj.verse`](https://github.com/edwardlavender/proj.verse) project is
released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
