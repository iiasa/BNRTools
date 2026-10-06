# BNRTools

This repository contains a common set of functions and scripts used by
BNR Researchers. It’s purpose is to avoid replication of efforts across
different groups and maintain common scripts in a structured way. Use
cases might for example include the reprojection to certain grids, the
conversion of model output formats to different format types or generic
commonly used helper functions.

# House rules

Every BNR researcher can suggest and add new functions to the package.
To maximize understanding and usability, please stick to the following
house rules:

- The `main` branch contains stable functions. The `dev` branch hot
  updates. To avoid merging conflicts preferably create a new branch
  named after yourself and then a pull request to `dev`.
- Functions should be stored in the `R` folder and one script per file.
- New scripts and functions should be named in camelCase with a prefix
  suggesting content, e.g. for a spatial file `sp_aggregateNUTS`.
- Each script should have at least a minimal documentation with title,
  description, parameters, example and output function.
- Where possible add unit tests to each function to catch wrong inputs
  early on.

See this [website](https://r-pkgs.org/) for more general help and
examples in developing content for R-packages.

## Installation

The `BNRTools` package can be installed either through the `remotes`
R-package or directly from r-universe.

``` r

# Install from source
remotes::install_github("iiasa/BNRTools")
# Or for the current development branch
remotes::install_github("iiasa/BNRTools", "dev")

# Installation 
install.packages('BNRTools', repos = "https://iiasa.r-universe.dev")
```

## Code of Conduct

Please note that the BNRTools project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Contributors

All contributions to this project are gratefully acknowledged using the
[`allcontributors` package](https://github.com/ropensci/allcontributors)
following the [all-contributors](https://allcontributors.org)
specification. Contributions of any kind are welcome!

[TABLE]
