# Adding a New Ingredient

## Adding a New Ingredient

Sooner or later you’ll want to feed your animals something that isn’t in
herdr’s default database. This guide walks through adding a brand-new
ingredient end to end, using **wet brewers grains**
(`brewers_grains_wet`) as a worked example — a common dairy by-product
feed that isn’t included out of the box.

Three files are involved, always in this order:

1.  `feed_characteristics.csv` — the ingredient’s nutritional profile.
2.  `mapping.csv` — how it connects to land-use and LCA databases.
3.  `diet_ingredients.csv` — where you actually use it in a project.

> 💡 If you only need this ingredient for one project and don’t care
> about its land-use footprint, you can skip most of this by setting
> `custom_yield_kg_ha` directly in `diet_ingredients.csv` (see Step 3).
> This guide covers the full, reusable setup.

------------------------------------------------------------------------

### Step 0 — Check it doesn’t already exist

Before adding anything, search `feed_characteristics.csv` for the
ingredient under a different name or spelling — herdr’s matching is
exact and case-sensitive.

``` r

library(herdr)

feed_db <- read.csv("user_data/feed_characteristics.csv")
matches <- grepl("brewer", feed_db$ingredient, ignore.case = TRUE)
feed_db[matches, ]
```

If nothing comes back, it’s genuinely new — continue to Step 1.

------------------------------------------------------------------------

### Step 1 — Add it to `feed_characteristics.csv`

This is herdr’s nutritional database — every ingredient referenced
anywhere in a diet must have a row here first. Add one, filling in every
column:

| ingredient | ingredient_type | land_type | DM_pct | ASH_pct | CP_pct | EE_pct | NDF_pct | DE_pct | swine_DE_kcal_kg | GE_feed_kcal_kg | source_DE |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|:---|---:|---:|
| `brewers_grains_wet` | `concentrate` | `cropland` | 24.0 | 3.8 | 25.5 | 8.0 | 46.0 | 68.0 | `NA` | 4,566 | `Feedipedia` |

*(Note: table truncated for display purposes. Make sure all monogastric
energy columns are also included in your actual file.)*

A few notes on where these numbers come from and how herdr uses them:

- Use **lowercase with underscores** for `ingredient`
  (`brewers_grains_wet`, not `Brewers Grains Wet`) — this is the
  identifier every other file will reference.
- **`land_type` (Agroecological Classification)**: This column is
  required by
  [`calculate_land_use()`](https://juancbm99.github.io/herdr/reference/calculate_land_use.md)
  to classify the agricultural competition of the feed:
  - `cropland`: Feed grown on arable land suitable for human food crops
    (cereal grains, oilseeds, pulses, and agro-industrial by-products
    such as brewers grains).
  - `grassland_convertible`: Productive grasslands with high agronomic
    potential that could be converted to arable cropland.
  - `grassland_unconvertible`: Native pastures, steep slopes, mountain,
    or marginal rangelands unsuitable for cultivation.
  - `none` (or `no_land`): Inputs requiring zero direct agricultural
    land (e.g. synthetic amino acids, urea, minerals, vitamins, water,
    or technological additives). When set to `none`,
    [`calculate_land_use()`](https://juancbm99.github.io/herdr/reference/calculate_land_use.md)
    immediately assigns 0 land use (`ha_kg_allocated = 0`), bypassing
    all yield queries. *(Note: if `land_type` is omitted from the file,
    herdr issues a warning and defaults to `NA`.)*
- **`DM_pct` (Dry Matter %)**: Beyond nutritional calculations,
  [`calculate_land_use()`](https://juancbm99.github.io/herdr/reference/calculate_land_use.md)
  directly uses `DM_pct` to convert raw FAOSTAT yields to dry matter
  yield ($`dm\_yield = raw\_yield \times \frac{DM\_pct}{100}`$). If
  `DM_pct` is missing, herdr issues a warning and assumes 100%.
- `ASH_pct`, `CP_pct`, `EE_pct`, and `NDF_pct` should come from a lab
  analysis or a recognized feed table (FEDNA is herdr’s default source
  for most of its bundled ingredients; Feedipedia is a good alternative
  for by-products like this one).
- **`source_DE`**: this column documents the origin of the Digestible
  Energy (`DE_pct`) value.
- **Forages (`DE_pct`)**: If a measured value is not available, Energy
  Digestibility ($`Ed`$) is derived from Organic Matter digestibility
  ($`OMd`$) and proximate analysis ($`CP`$, $`EE`$, $`NDF`$, $`Ash`$).
  See the exact regression formulas in the [Technical Reference: Feed
  Characteristics](https://juancbm99.github.io/herdr/articles/Technical_reference.html#feed_characteristics.csv--nutritional-values).
- **Gross Energy (`GE_feed_kcal_kg`)**: If unmeasured by bomb
  calorimetry, calculate using the NRC (1989) Ewan equation documented
  in the [Technical
  Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.html#feed_characteristics.csv--nutritional-values).
- Leave the swine/poultry energy columns as `NA` if the ingredient is
  only fed to ruminants (as here) — herdr only needs them for
  monogastric diets.

See the [Technical
Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.html#feed_characteristics.csv--nutritional-values)
vignette for the full column definitions.

------------------------------------------------------------------------

### Step 2 — Add it to `mapping.csv`

`feed_characteristics.csv` tells herdr *what’s in* the ingredient.
`mapping.csv` tells herdr *what land it comes from*, which is what
powers the land-use calculation. Add a row here too:

| ingredient | yield_name | agribalyse_name | economic_allocation |
|:---|:---|:---|---:|
| `brewers_grains_wet` | `barley` | `Barley, feed grade, national average, at farm gate` | 0.15 |

Two of these columns need a bit more care:

#### `economic_allocation`

Wet brewers grains is a **co-product** of barley malting for brewing —
the barley crop’s environmental burden has to be split between the beer
(the main product) and the spent grains (the by-product) somehow.
`economic_allocation` is that split, expressed as the share (0–1) of the
crop’s total economic value attributed to *this* ingredient. Brewing
by-products are typically low-value relative to the main product, so
`0.15` here means 15% of the barley’s land-use footprint is attributed
to the grains, and the rest to the beer itself. For a primary crop with
no co-products (e.g. plain `barley_grain` used directly as feed), this
would simply be `1.0`.

In
[`calculate_land_use()`](https://juancbm99.github.io/herdr/reference/calculate_land_use.md),
this factor directly scales the land requirement:

``` math
\text{ha\_kg\_allocated} = \text{ha\_per\_kg} \times \text{economic\_allocation}
```

If left blank or `NA`, herdr defaults `economic_allocation` to `1.0`.

#### `yield_name`

This must match, **exactly**, an `Item` name already present in herdr’s
bundled yield databases — `fao_crops.parquet` for arable crops or
`fao_forages.parquet` for grasses and silages — since that’s where the
actual kg/ha figure is pulled from. Herdr combines both datasets and
filters out aggregate areas (using `Area Code < 5000` to keep only
genuine countries).

Don’t guess or type a name from the live FAOSTAT website: the local
`.parquet` files are what the model actually reads, and their naming
doesn’t always match FAOSTAT’s current item names exactly. Check them
directly instead:

``` r

library(arrow)

crops <- read_parquet("user_data/fao_crops.parquet")
unique(crops$Item)[grepl("barley", unique(crops$Item), ignore.case = TRUE)]
```

If the crop your ingredient derives from genuinely isn’t in either file,
you have two options: add a row for it to the appropriate `.parquet`
file yourself (advanced — see the [Land Use
Methodology](https://juancbm99.github.io/herdr/articles/land_use.md)
vignette for the schema), or skip `yield_name` resolution entirely by
supplying `custom_yield_kg_ha` per-project instead (Step 3).

------------------------------------------------------------------------

### Step 3 — Use it in `diet_ingredients.csv`

With both reference files updated, the ingredient behaves exactly like
any bundled one. Add it to a diet as usual:

| diet_tag | region | subregion | class_flex | ingredient | ingredient_share | ingredient_type | country_of_origin | custom_yield_kg_ha |
|:---|:---|:---|:---|:---|---:|:---|:---|---:|
| `diet_dairy_mature` | `spain` | `all` | `all` | `brewers_grains_wet` | 15 | `concentrate` | `spain` | `NA` |

Keep these practical rules in mind when entering ingredients:

- **`ingredient_type`**: Must match one of four exact categories:
  `"forage"`, `"concentrate"`, `"milk"`, or `"milk_replacer"`. This
  connects the ingredient to the corresponding category share in
  `diet_profiles.csv`. If misspelled or unrecognized, herdr sets the
  intake share factor to 0, resulting in zero consumption and zero land
  use.

- **`ingredient_share` Validation**: Within any given diet cohort and
  `ingredient_type`, the sum of `ingredient_share` values must total
  100% ($`\pm 0.1\%`$). Herdr validates this before computing and throws
  a warning if shares do not total 100%:

      ⚠️ Ingredient shares do not sum to 100% in: diet_dairy_mature

- **Country of Origin & Yield Resolution**:

  - **Explicit country** (e.g. `"spain"`): herdr resolves yields for
    that specific country from `fao_crops.parquet` /
    `fao_forages.parquet`.
  - **Bypassing FAO lookups (`custom_yield_kg_ha`)**: If you already
    know the specific crop yield (e.g. from local farm records), enter
    it in `custom_yield_kg_ha` (in kg DM/ha). Herdr will use this number
    directly and label the origin as `"Custom Data"`.
  - **Leaving it blank or `NA`**: If `country_of_origin` is `NA`, herdr
    automatically estimates the genuine producer country through dynamic
    international trade flows (see the [Land Use
    Methodology](https://juancbm99.github.io/herdr/articles/land_use.html#feed-origin--dynamic-trade-allocation)
    vignette for how trade flows and re-export hubs are traced).

------------------------------------------------------------------------

### Common Issues

| Symptom | Likely cause | Fix |
|:---|:---|:---|
| “Unrecognized ingredient” error | Ingredient name in `diet_ingredients.csv` doesn’t exactly match `feed_characteristics.csv` (spacing, casing, typo) | Copy the name directly from `feed_characteristics.csv`, don’t retype it. |
| Warning: `Column 'land_type' not found` | `feed_characteristics.csv` is missing the `land_type` column | Add `land_type` and specify `cropland`, `grassland_convertible`, `grassland_unconvertible`, or `none`. |
| Land-use result of zero for this ingredient | `land_type` was set to `none`/`no_land`, `yield_name` doesn’t match any `Item` in FAO data, `economic_allocation` is 0, or `ingredient_type` was misspelled | Check `land_type`; re-check the exact `Item` name with [`arrow::read_parquet()`](https://arrow.apache.org/docs/r/reference/read_parquet.html); verify `economic_allocation`; ensure `ingredient_type` is one of `forage`, `concentrate`, `milk`, `milk_replacer`. |
| Warning: `Ingredient shares do not sum to 100%` | Values in `ingredient_share` within that diet cohort and `ingredient_type` do not total 100 | Check `diet_ingredients.csv` and adjust shares so they sum to 100% for each category. |
| Warning: `Missing yield for: ...` | Country specified in `country_of_origin` has no recorded yield in FAO data for that year | Check the spelling of `Area` in `fao_crops.parquet`, leave `country_of_origin` blank to use trade tracing, or provide `custom_yield_kg_ha`. |
| 187 MB download begins unexpectedly | `country_of_origin` was left `NA`, triggering trade matrix download | Let the download finish once, or explicitly provide a country in `country_of_origin` / fill in `custom_yield_kg_ha`. |
| GHG values look off for the ingredient | `GE_feed_kcal_kg` wasn’t calculated, or nutritional values were entered as % as-fed instead of % DM | Recalculate GE with the Ewan equation; double-check units against the [Technical Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md). |
| Ingredient works for one project but not another | It was only added to that project’s local `user_data/`, not to the shared database used elsewhere | Copy the new rows from `feed_characteristics.csv` and `mapping.csv` into every project that needs the ingredient, or maintain one shared `user_data/` template. |

------------------------------------------------------------------------

### Next steps

- [Technical
  Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
  — full column-by-column definitions for every file touched here.
- [Land Use
  Methodology](https://juancbm99.github.io/herdr/articles/land_use.md) —
  mathematical formulation of land occupation, agroecological land
  competition framework, and dynamic trade tracing.
- [General
  Workflow](https://juancbm99.github.io/herdr/articles/Workflow.md) —
  where this step fits into a full project setup.
