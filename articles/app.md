# Using the herdr App: A Step-by-Step Workflow

## Using the herdr App

The **herdr** Shiny app is an interactive front end for the package’s
livestock greenhouse gas (GHG) emissions model. Instead of editing CSV
files by hand and calling R functions directly, you can load your data,
edit it in spreadsheet-style tables, and run the full IPCC-based
calculation from a single screen.

This guide walks through that workflow from start to finish. It applies
whether you’re running the app **locally** on your own machine or using
the **hosted web version** — the interface is identical either way.

### Two ways to open the app

#### Option A — Run it locally

Install the package from GitHub, initialize its data folders, and launch
the app:

``` r

if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")
remotes::install_github("JuanCBM99/herdr")

library(herdr)
herdr_init() # sets up the examples/ and user_data/ folders
run_app()    # launches the app locally
```

[`herdr_init()`](https://juancbm99.github.io/herdr/reference/herdr_init.md)
only needs to run once per project — it copies over the bundled example
datasets and creates the `user_data/` folder the app reads from and
saves to. `run_app()` opens the app in your default browser.

#### Option B — Use the hosted web app

No installation required — just open the app directly in your browser:

👉
**[juancbm99.shinyapps.io/herdr](https://juancbm99.shinyapps.io/herdr/)**

The workflow below is exactly the same as running it locally — the only
difference is that your session (and any data you load or edit) lives on
the server for the duration of your browser tab, rather than on your own
machine.

> **Tip:** if you’re using the hosted version for a real project, make
> sure to **download your results** (Step 7) before closing the tab —
> unsaved edits don’t persist between sessions on a shared web instance.

### The workflow

The app is organized as a guided sequence: a sidebar on the left walks
you through four steps, and the tabs across the top hold your data.

#### Step 1 — Load your data

In the sidebar’s **“Data Source”** panel, you have three ways to get
started — and which one you pick decides how much you’ll need to do in
Step 3:

- **Load Example** — pick one of the bundled example datasets from the
  *Load Package Example* dropdown and click **Load Example**. Every
  table is pre-filled with realistic sample data, so Step 3 becomes
  optional review/tweaking rather than data entry from scratch — handy
  for exploring the app or using it as a starting template.
- **Upload CSV Files** — if you already have herdr-formatted CSVs, fully
  filled in (from a previous session, or prepared/edited outside the
  app), drag them into the *Or Upload CSV Files* box. The app matches
  each file to the right table automatically by filename, so keep the
  original names (e.g. `livestock_census.csv`). **This effectively does
  Step 3 for you** — your tables arrive already complete, so you can
  skip straight to reviewing the validation banners and running the
  model. You’d still open a tab to fix something (a typo, a red cell) if
  the app flags it, but there’s no data entry left to do.
- **Clear All Data** — wipes every table back to empty (keeping the
  correct columns), if you’d rather start from a blank slate. This is
  the path where Step 3 is genuinely mandatory: every table needs
  filling in by hand.

Any of these actions takes you straight to the **Census** tab.

#### Step 2 — Set the general configuration

Still in the sidebar, under **“Configuration”**:

- **Use automatic herd cycle** — toggle this on if you want herdr to
  compute herd/reproduction dynamics automatically. Leaving it off
  reveals a **Reproduction** tab where you can enter those parameters
  yourself.
- **Farm Country / Area** — the country used as the reference for FAO
  trade and production data. This matters most if any ingredient in your
  diet tables has an unknown origin: herdr will estimate it using this
  country’s FAO statistics.
- **FAO Reference Year** — the year of FAO data to use for that
  estimation.

Hover the small ⓘ icon next to any of these fields for a one-line
explanation if you’re not sure what it affects.

#### Step 3 — Fill in (or review) your data tables

If you started blank, this is where you enter everything by hand. If you
loaded the example data, this is where you’d tweak it to match your own
herd. And if you uploaded already-completed CSVs, your tables arrive
fully populated — so treat this step as a quick pass to confirm nothing
looks off, rather than data entry.

Work through the tabs left to right: **Census → Diet Profiles →
Ingredients → Definitions → Monogastrics → Weights → Manure →
Reproduction** (if enabled). A few things make this easier, whether
you’re entering data or just reviewing it:

- **Edit cells directly**, like a spreadsheet. New rows appear
  automatically as you fill in the last one.
- **Dropdown-driven columns** (e.g. IPCC coefficient descriptions in
  Definitions, or manure system fields in Manure) restrict you to valid
  options pulled straight from the IPCC reference tables, so you can’t
  mistype a category.
- **Numeric cells turn red** if you type text or a negative number where
  a real, non-negative number is expected. The **Run herdr Model**
  button stays disabled — with a note listing which tables need
  attention — until every red cell is fixed.
- A small **dot next to a tab’s name** means that table has unsaved
  edits (they’re saved automatically the moment you run the model).
- **Cross-checks against Census**: tabs like Definitions, Monogastrics
  and Manure show a banner if an animal you declared in Census hasn’t
  been defined yet there, or if a row doesn’t match anything in Census.
  Use this to catch typos in `animal_tag` before running the model.
- At the bottom of each tab, a **“Next Step”** button takes you straight
  to the next relevant tab — on the last one, it becomes **“Ready? Click
  ‘Run herdr Model’”**, which runs the calculation for you.

#### Step 4 — (Optional) Review Advanced / IPCC tables

The **Advanced (IPCC)** tab holds the underlying IPCC default values and
technical characteristics (feed characteristics, forage yields, IPCC
coefficients, manure management factors, internal mapping). A warning
banner reminds you these are expert-level settings — the model works
fine with the defaults, so only edit these if you have specific regional
data to substitute.

#### Step 5 — Run the model

Click **Run herdr Model** in the sidebar (or the “Ready?” button on the
last data tab). While it runs:

- The button shows a spinner and disables itself, so you can’t trigger
  it twice.
- A progress bar reports what’s happening (saving your tables →
  calculating emissions → done).
- Any warnings the model raises appear as on-screen notifications
  instead of being silently swallowed.

You’re taken straight to the **Results** tab when it’s done.

#### Step 6 — Review the results

The **Results** tab shows:

- A row of **KPI cards** summarizing the headline emissions totals at a
  glance.
- The full results table underneath, for the complete breakdown.

If you’d rather work in a darker interface (useful in low light, or just
preference), use the **light/dark toggle** in the top-right corner of
the app — it applies to the whole app, including the data tables.

#### Step 7 — Download your results

Click **Download Results** in the sidebar at any time after a successful
run to save the full results table as a CSV
(`herdr_impact_assessment_YYYY-MM-DD.csv`).

### Troubleshooting

| Symptom | Likely cause | What to do |
|----|----|----|
| A red “Missing Cohorts” banner | An `animal_tag` (+ region/subregion/class) in Census isn’t defined in another table yet | Add a matching row in the tab shown, or correct the tag if it was a typo. |
| An amber “Unrecognized Cohorts” banner | A row exists in a table but doesn’t match any Census entry | Either add it to Census, or delete/correct the row. |
| Uploaded CSV was ignored | The filename didn’t match herdr’s expected names | Rename it to match the original export (e.g. `livestock_census.csv`) and upload again. |
| Model errors after clicking Run | Usually a data issue the validator didn’t catch (e.g. logical inconsistency) | Read the error notification — it comes directly from the model — and check the tables it references. |

### Summary

1.  Load example data, upload your own already-completed CSVs, or start
    blank.
2.  Set the general configuration (herd cycle, country, reference year).
3.  Fill in the standard tables by hand (blank start), tweak the example
    data, or just review it — this step is only real “data entry” if you
    didn’t already upload complete CSVs.
4.  Optionally review the Advanced/IPCC tables.
5.  Run the model.
6.  Review the KPI summary and full results table.
7.  Download your results as CSV.

The same steps work identically whether you’re running herdr on your own
machine or through the hosted web app.
