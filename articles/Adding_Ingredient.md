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
grepl("brewer", feed_db$ingredient, ignore.case = TRUE) |> feed_db[_, ]
```

If nothing comes back, it’s genuinely new — continue to Step 1.

------------------------------------------------------------------------

### Step 1 — Add it to `feed_characteristics.csv`

This is herdr’s nutritional database — every ingredient referenced
anywhere in a diet must have a row here first. Add one, filling in every
column:

| ingredient | feed_category | DM_pct | ASH_pct | CP_pct | EE_pct | NDF_pct | DE_pct | GE_feed_kcal_kg | swine_DE_kcal_kg | swine_ME_kcal_kg | poultry_ME_kcal_kg |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `brewers_grains_wet` | `by_product` | 24.0 | 3.8 | 25.5 | 8.0 | 46.0 | 68.0 | 4,566 | `NA` | `NA` | `NA` |

A few notes on where these numbers come from:

- Use **lowercase with underscores** for `ingredient`
  (`brewers_grains_wet`, not `Brewers Grains Wet`) — this is the
  identifier every other file will reference.

- `DM_pct`, `ASH_pct`, `CP_pct`, `EE_pct`, and `NDF_pct` should come
  from a lab analysis or a recognized feed table (FEDNA is herdr’s
  default source for most of its bundled ingredients; Feedipedia is a
  good alternative for by-products like this one).

- If you don’t have a directly measured `GE_feed_kcal_kg`, calculate it
  with the NRC (1998) Ewan equation already used throughout herdr’s own
  database:

  ``` math
  GE\;(\text{kcal/kg DM}) = 4140 + (56 \times EE\%) + (15 \times CP\%) - (44 \times ASH\%)
  ```

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

#### `yield_name`

This must match, **exactly**, an `Item` name already present in herdr’s
bundled yield databases — `fao_crops.parquet` for arable crops or
`fao_forages.parquet` for grasses and silages — since that’s where the
actual kg/ha figure is pulled from. Don’t guess or type a name from the
live FAOSTAT website: the local `.parquet` files are what the model
actually reads, and their naming doesn’t always match FAOSTAT’s current
item names exactly. Check them directly instead:

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

| diet_tag | region | feed_category | ingredient | percentage | country_of_origin | custom_yield_kg_ha |
|:---|:---|:---|:---|---:|:---|---:|
| `diet_dairy_mature` | `spain` | `concentrate` | `brewers_grains_wet` | 15 | `spain` | `NA` |

With `custom_yield_kg_ha` left as `NA`, herdr resolves the land-use
footprint automatically: it looks up `barley` in `fao_crops.parquet` for
the given `country_of_origin`, applies the `economic_allocation` of
`0.15` from `mapping.csv`, and uses the nutritional values from
`feed_characteristics.csv` for the GHG side of the assessment.

If you’d rather bypass the yield lookup — say, you know the specific
brewery’s sourcing — just fill in `custom_yield_kg_ha` directly and skip
needing a valid `yield_name` altogether.

------------------------------------------------------------------------

### Common Issues

| Symptom | Likely cause | Fix |
|:---|:---|:---|
| “Unrecognized ingredient” error | Ingredient name in `diet_ingredients.csv` doesn’t exactly match `feed_characteristics.csv` (spacing, casing, typo) | Copy the name directly from `feed_characteristics.csv`, don’t retype it. |
| Land-use result of zero for this ingredient | `yield_name` in `mapping.csv` doesn’t match any `Item` in the `.parquet` files, or `economic_allocation` was left blank | Re-check the exact `Item` name with the [`arrow::read_parquet()`](https://arrow.apache.org/docs/r/reference/read_parquet.html) lookup above; make sure `economic_allocation` is a number between 0 and 1. |
| GHG values look off for the ingredient | `GE_feed_kcal_kg` wasn’t calculated, or nutritional values were entered as % as-fed instead of % DM | Recalculate GE with the Ewan equation; double-check units against the [Technical Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md). |
| Ingredient works for one project but not another | It was only added to that project’s local `user_data/`, not to the shared database used elsewhere | Copy the new rows from `feed_characteristics.csv` and `mapping.csv` into every project that needs the ingredient, or maintain one shared `user_data/` template. |

------------------------------------------------------------------------

### Next steps

- [Technical
  Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
  — full column-by-column definitions for every file touched here.
- [Land Use
  Methodology](https://juancbm99.github.io/herdr/articles/land_use.md) —
  how `yield_name` and `economic_allocation` feed into the land-use
  calculation, and the schema for adding new crops to
  `fao_crops.parquet` / `fao_forages.parquet`.
- [General
  Workflow](https://juancbm99.github.io/herdr/articles/Workflow.md) —
  where this step fits into a full project setup.
