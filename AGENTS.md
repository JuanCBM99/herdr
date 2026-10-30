# Project Guidelines and Standards: herdr

This document defines the guidelines and best practices for developing
the **`herdr`** package in collaboration with **Antigravity** and AI
coding assistants.

------------------------------------------------------------------------

## 1. Project Overview

- **Package**: `herdr`
- **Type**: R Package (\>= 4.1.0) with Shiny GUI support
  ([`run_app()`](https://juancbm99.github.io/herdr/reference/run_herdr_app.md)).
- **Domain**: Livestock greenhouse gas (GHG) emissions and feed-related
  land use footprint accounting based on **IPCC Tier 2** methodologies
  and nutritional guidelines (ruminants and monogastrics).
- **Core Branches**:
  - `main`: Production-ready, stable releases.
  - `dev`: Active integration and daily development branch.

------------------------------------------------------------------------

## 2. R Coding Standards

1.  **Code Style**:
    - Follow the **Tidyverse** style guide (`styler` / `lintr`).
    - Use meaningful, descriptive `snake_case` for all function and
      variable names.
    - Never use [`library()`](https://rdrr.io/r/base/library.html) or
      [`require()`](https://rdrr.io/r/base/library.html) within `R/`
      scripts. Use `@importFrom package function` in roxygen2 headers or
      explicit `package::function()` calls.
    - Any non-standard evaluation (NSE) column names or global symbols
      used in `dplyr`/`ggplot2` pipelines must be declared in
      [`R/globals.R`](file:///c:/Users/juant/Desktop/herdr/R/globals.R)
      via
      [`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
      to prevent `R CMD check` notes.
2.  **Documentation (`roxygen2`)**:
    - All exported functions must have complete roxygen2 documentation
      blocks including:
      - Clear `@title` and `@description`.
      - Explicit `@param` descriptions with data types and physical
        units (e.g., `kg CH4/head/year`, `MJ/day`).
      - Concrete `@return` defining output data types and structures.
      - `@export` only for public-facing API functions.
      - Reproducible `@examples` (wrap in `\dontrun{}` if requiring
        heavy external data downloads).
3.  **Methodological & Mathematical Rigor**:
    - Calculations must strictly adhere to IPCC guidelines (2006 / 2019
      Refinement) and referenced animal nutrition literature.
    - Document specific equations in source code comments (e.g.,
      `# IPCC 2019, Vol 4, Ch 10, Eq. 10.21`).
    - Implement defensive programming and input validation using
      [`assertthat::assert_that`](https://rdrr.io/pkg/assertthat/man/assert_that.html)
      or explicit condition checks on physical boundaries.

------------------------------------------------------------------------

## 3. Testing and Verification (`testthat`)

- Every calculation or functional module in `R/` must have a
  corresponding test suite in `tests/testthat/test_*.R`.

- Functional code must never be merged without verifying that existing
  and new tests pass.

- Run tests in PowerShell via:

  ``` powershell
  & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test_NAME.R')"
  ```

- Run the full test suite:

  ``` powershell
  & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e "pkgload::load_all('.'); devtools::test()"
  ```

- Regenerate documentation and `NAMESPACE`:

  ``` powershell
  & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e "devtools::document()"
  ```

------------------------------------------------------------------------

## 4. Git Workflow and Conventional Commits

- **Base Development Branch**: `dev`.
- **Feature Branches**: `feature/<name>`, `fix/<name>`, `docs/<name>`.
- **Commit Messages**: Follow **Conventional Commits**:
  - `feat(...)`: New feature or calculation.
  - `fix(...)`: Bug fix or calculation correction.
  - `docs(...)`: Documentation, pkgdown, or vignette updates.
  - `test(...)`: Unit test additions or updates.
  - `perf(...)`: Performance optimizations.
  - `refactor(...)`: Code cleanup without functional changes.
- Maintain a clean working directory (no untracked R artifacts such as
  `.Rhistory`, `.RData`, or `.Rcheck/`).
