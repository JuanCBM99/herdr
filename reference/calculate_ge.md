# Calculate Gross Energy (GE)

Computes gross energy requirements by aggregating Net Energy (NE)
components.

## Usage

``` r
calculate_ge(
  saveoutput = TRUE,
  data_dir = "user_data",
  de_df = NULL,
  NEm_df = NULL,
  NEa_df = NULL,
  NEg_df = NULL,
  NEl_df = NULL,
  NE_work_df = NULL,
  NE_pregnancy_df = NULL,
  NE_wool_df = NULL
)
```

## Arguments

- saveoutput:

  If TRUE (default) the results are saved in the output folder.

- data_dir:

  Character. Path to the folder containing input CSV files. Default is
  `"user_data"`.

- de_df:

  Optional precalculated weighted variable dataframe (with DE_pct).

- NEm_df:

  Optional precalculated NEm dataframe.

- NEa_df:

  Optional precalculated NEa dataframe.

- NEg_df:

  Optional precalculated NEg dataframe.

- NEl_df:

  Optional precalculated NEl dataframe.

- NE_work_df:

  Optional precalculated NE_work dataframe.

- NE_pregnancy_df:

  Optional precalculated NE_pregnancy dataframe.

- NE_wool_df:

  Optional precalculated NE_wool dataframe.
