# Calculate Dry Matter Intake (DMI)

Computes daily Dry Matter Intake (kg DM/day) based on metabolic demand
(GE/ED) for ruminants and metabolizable energy parameters (FEDNA) for
poultry.

## Usage

``` r
calculate_DMI(saveoutput = TRUE, data_dir = "user_data")
```

## Arguments

- saveoutput:

  If TRUE (default) the results are saved in the output folder.

- data_dir:

  Character. Path to the folder containing input CSV files. Default is
  `"user_data"`.
