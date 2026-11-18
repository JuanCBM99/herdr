# Calculate Monogastric Metabolizable Energy Requirements (FEDNA)

Calculates daily metabolizable energy requirements (kcal/day) for
poultry and swine using direct mathematical switches.

## Usage

``` r
calculate_monogastric_energy(saveoutput = TRUE, data_dir = "user_data")
```

## Arguments

- saveoutput:

  If TRUE (default), saves the results to
  \`output/monogastric_metabolizable_energy.csv\`.

- data_dir:

  Character. Path to the folder containing input CSV files. Default is
  `"user_data"`.

## Value

A tibble with daily metabolizable energy requirements
(`ME_total_kcal_day`, kcal/day) and phase components for swine and
poultry cohorts.

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_monogastric_energy(saveoutput = FALSE, data_dir = "user_data")
} # }
```
