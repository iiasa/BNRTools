# Development guidance

BNRTools is an R package with functions to support research projects and
activities. Its covers miscellaneous utilities (`misc_*` and `%notin%`), 
conversion functions such as from GLOBIOM outputs to other models, NetCDF 
conversion (`conv_*`), and spatial vector and raster processing (`spl_*`).

## R code

- **Docs**: [README.md](README.md) | [pkgdown site](https://iiasa.github.io/ibis.SPOP/) | [NEWS.md](NEWS.md)
- **Style**: camelCase for functions/variables. One main function per file. Name the file
  after the function, following the existing prefix groups (`misc_`, `conv_`,
  or `spl_`).
- Keep a function focused and consistent with the nearest existing function's
  inputs, return type, naming, and side effects. Validate inputs at the
  function boundary and report invalid inputs or failed operations clearly.
- Do not rely on objects in the interactive workspace, the current working
  directory, machine-specific paths, or hidden global state. Pass inputs and
  outputs explicitly, and use package-qualified calls such as `terra::rast()`
  rather than attaching dependencies in package code.
- Add dependencies only when needed. Record runtime dependencies in
  `Imports`; use `Suggests` for optional tools and test-only dependencies.

## Non-negotiables

- Make minimal, safe changes; avoid broad refactors unless absolutely necessary.
- Focus on clear code and re-useability throughout. Where code base or functions exists, 
  try to re-use and where needed add parameters and functionalities.
- Functions should be self-contained and not query other functions.
- Use explicit namespacing (pkg::fun); no library()/require() in package code.

## Documentation and examples

- Document exported functions with roxygen comments in their `R/` source.
  Include a concise title and description, an `@param` for every argument, a
  correct `@return`, and useful `@examples`; add details, keywords, and
  cross-references when they help users.
- Make examples reproducible from a clean R session: create their own small
  inputs, set a seed when randomness matters, and avoid local paths or
  interactive-only objects. Use `\dontrun{}` only when an example genuinely
  requires external data, credentials, or an otherwise unavailable resource.
- Regenerate `NAMESPACE` and `man/*.Rd` from roxygen comments with
  `devtools::document()`; do not edit generated files by hand. Treat `docs/`
  as pkgdown output rather than the source for function documentation.
- Keep any standalone analysis or reproduction script self-contained: state
  its inputs and required packages, make outputs explicit, and do not assume
  objects were created by earlier interactive commands.

## Tests

- Add unit tests for new behavior and regression tests for bug fixes under
  `tests/testthat/`.
- Name new test files `test-<function>.R` and organize cases with
  `test_that()` and `expect_*()` assertions.
- Test observable results, important edge cases, and invalid inputs. Keep tests
  deterministic and fast; use small in-memory data (including small `terra`
  rasters for spatial behavior) rather than large external datasets.
- Tests and examples must not require private files, network access, a
  particular working directory, or pre-existing user data. Use temporary
  files/directories for I/O and clean them up.
- If a test needs an optional package, skip it explicitly when that package is
  unavailable rather than suppressing dependency or test failures.
- Run the focused tests while developing, then run `R CMD check .` before
  submitting when the environment permits. The GitHub Actions workflow also
  runs `R CMD check` on Windows, macOS, and Linux.