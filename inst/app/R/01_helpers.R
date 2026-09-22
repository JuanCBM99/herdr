# ==============================================================================
# herdr — Data I/O & Handsontable Helpers
# ==============================================================================

# --- Data I/O Sanitization ---

enforce_column_order <- function(df, id) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(df)

  desired_lead <- switch(id,
    "def" = c("animal_tag", "region", "subregion", "class_flex", "animal_type", "animal_subtype", "production_role"),
    "mono" = c("animal_tag", "region", "subregion", "class_flex", "animal_type", "animal_subtype", "production_role"),
    "diet_ingr" = c("diet_tag", "ingredient_type", "ingredient", "ingredient_share", "country_of_origin", "custom_yield_kg_ha"),
    NULL
  )

  if (!is.null(desired_lead)) {
    present_lead <- intersect(desired_lead, names(df))
    remaining <- setdiff(names(df), present_lead)
    df <- df[, c(present_lead, remaining), drop = FALSE]
  }
  df
}

read_clean <- function(path) {
  if (!file.exists(path)) return(data.frame())
  df <- readr::read_csv(path, show_col_types = FALSE, lazy = FALSE, progress = FALSE) %>%
    dplyr::mutate(dplyr::across(where(is.logical), as.character))

  fname <- basename(path)
  if (fname == "ruminant_definitions.csv") {
    df <- enforce_column_order(df, "def")
  } else if (fname == "monogastric_definitions.csv") {
    df <- enforce_column_order(df, "mono")
  } else if (fname == "diet_ingredients.csv") {
    df <- enforce_column_order(df, "diet_ingr")
  }
  df
}

save_clean <- function(df, path) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  fname <- basename(path)
  if (fname == "ruminant_definitions.csv") {
    df <- enforce_column_order(df, "def")
  } else if (fname == "monogastric_definitions.csv") {
    df <- enforce_column_order(df, "mono")
  } else if (fname == "diet_ingredients.csv") {
    df <- enforce_column_order(df, "diet_ingr")
  }
  key_column <- intersect(c("animal_tag", "diet_tag"), names(df))[1]
  if (!is.na(key_column)) {
    df <- df %>% dplyr::filter(!is.na(.data[[key_column]]) & .data[[key_column]] != "")
  }
  df %>%
    dplyr::mutate(dplyr::across(where(is.logical), ~ NA_character_)) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., ""))) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., "NA"))) %>%
    readr::write_csv(path, na = "")
}

build_keys <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !"animal_tag" %in% names(df)) return(character(0))

  clean_col <- function(col_name) {
    if (col_name %in% names(df)) {
      v <- trimws(as.character(df[[col_name]]))
      v[is.na(v) | v == "NA"] <- ""
      v
    } else {
      rep("", nrow(df))
    }
  }

  tag <- clean_col("animal_tag")
  reg <- clean_col("region")
  subreg <- clean_col("subregion")
  cflex <- clean_col("class_flex")

  keys <- paste(tag, reg, subreg, cflex, sep = " | ")
  keys[tag == ""] <- ""
  keys
}

format_cohort_label <- function(key) {
  if (is.null(key) || length(key) == 0 || trimws(key) == "") return("")
  parts <- strsplit(key, " \\| ")[[1]]
  tag <- parts[1]
  if (is.na(tag) || trimws(tag) == "") return("")
  extras <- parts[-1]
  extras <- extras[extras != ""]
  if (length(extras) > 0) {
    paste0(tag, " (", paste(extras, collapse = " | "), ")")
  } else {
    tag
  }
}

# --- Canonical Demographic Cohorts Catalog ---
DEMOGRAPHIC_SPECIES_MAP <- list(
  cattle = c(
    "dairy_calves_female_replacement",
    "dairy_yearlings_female_replacement",
    "feedlot_calves_male",
    "feedlot_calves_female",
    "beef_calves_male",
    "beef_calves_female",
    "beef_calves_male_replacement",
    "beef_calves_female_replacement",
    "beef_yearlings_male_replacement",
    "beef_yearlings_female_replacement"
  ),
  sheep = c(
    "lamb_female_dairy_replacement",
    "lamb_male_dairy_replacement",
    "lamb_female_meat_replacement",
    "lamb_male_meat_replacement",
    "lamb_dairy_slaughter",
    "lamb_meat_slaughter"
  ),
  goat = c(
    "kid_goat_female_dairy_replacement",
    "kid_goat_male_dairy_replacement",
    "kid_goat_female_meat_replacement",
    "kid_goat_male_meat_replacement",
    "kid_goat_dairy_slaughter",
    "kid_goat_meat_slaughter"
  ),
  swine = c(
    "replacement_sows",
    "fattening_pigs"
  ),
  poultry = c(
    "replacement_meat_pullets",
    "replacement_layer_pullets",
    "broilers"
  )
)

ALL_DEMOGRAPHIC_TAGS <- unname(unlist(DEMOGRAPHIC_SPECIES_MAP))

# --- Dynamic & Cascading Dropdown Application ---

get_dropdown_options <- function(id, col_name, rv) {
  # Cascading manure columns handled by specialized cascade
  if (id == "manure" && col_name %in% MANURE_CASCADE_COLUMNS) {
    if (col_name == MANURE_CASCADE_COLUMNS[1] && !is.null(rv$ipcc_mm)) {
      opts <- as.character(unique(na.omit(rv$ipcc_mm[[col_name]])))
      return(sort(opts[trimws(opts) != ""]))
    }
    return(character(0))
  }

  rule <- if (id %in% names(dynamic_dropdowns)) dynamic_dropdowns[[id]][[col_name]] else NULL

  options <- character(0)
  if (!is.null(rule) && !is.null(rule$choices)) {
    options <- rule$choices
  } else if (!is.null(rule) && !is.null(rule$source_table)) {
    source_df <- rv[[rule$source_table]]
    if (!is.null(source_df) && nrow(source_df) > 0 && rule$extract_column %in% names(source_df)) {
      if (!is.null(rule$filter_column) && !is.null(rule$filter_value) && rule$filter_column %in% names(source_df)) {
        options <- source_df[[rule$extract_column]][source_df[[rule$filter_column]] == rule$filter_value]
      } else {
        options <- source_df[[rule$extract_column]]
      }
    }
  }

  # Multi-table cross-table pooling for key linking columns:
  if (col_name == "diet_tag") {
    options <- c(
      options,
      if (!is.null(rv$diet_prof) && "diet_tag" %in% names(rv$diet_prof)) rv$diet_prof$diet_tag,
      if (!is.null(rv$diet_ingr) && "diet_tag" %in% names(rv$diet_ingr)) rv$diet_ingr$diet_tag,
      if (!is.null(rv$def) && "diet_tag" %in% names(rv$def)) rv$def$diet_tag,
      if (!is.null(rv$mono) && "diet_tag" %in% names(rv$mono)) rv$mono$diet_tag
    )
  } else if (col_name == "animal_tag") {
    options <- c(
      options,
      ALL_DEMOGRAPHIC_TAGS,
      if (!is.null(rv$census) && "animal_tag" %in% names(rv$census)) rv$census$animal_tag,
      if (!is.null(rv$def) && "animal_tag" %in% names(rv$def)) rv$def$animal_tag,
      if (!is.null(rv$mono) && "animal_tag" %in% names(rv$mono)) rv$mono$animal_tag,
      if (!is.null(rv$weights) && "animal_tag" %in% names(rv$weights)) rv$weights$animal_tag,
      if (!is.null(rv$manure) && "animal_tag" %in% names(rv$manure)) rv$manure$animal_tag,
      if (!is.null(rv$repro) && "animal_tag" %in% names(rv$repro)) rv$repro$animal_tag
    )
  } else if (col_name == "ingredient") {
    options <- c(
      options,
      if (!is.null(rv$feed_char) && "ingredient" %in% names(rv$feed_char)) rv$feed_char$ingredient,
      if (!is.null(rv$mapping) && "ingredient" %in% names(rv$mapping)) rv$mapping$ingredient,
      if (!is.null(rv$diet_ingr) && "ingredient" %in% names(rv$diet_ingr)) rv$diet_ingr$ingredient
    )
  }

  options <- as.character(unique(na.omit(options)))
  options <- options[trimws(options) != ""]
  sort(options)
}

apply_dynamic_dropdowns <- function(tbl, id, rv) {
  if (!id %in% names(dynamic_dropdowns)) return(tbl)
  for (target_col in names(dynamic_dropdowns[[id]])) {
    rule <- dynamic_dropdowns[[id]][[target_col]]
    if (!target_col %in% names(rv[[id]])) next

    options <- get_dropdown_options(id, target_col, rv)

    if (length(options) > 0) {
      col_type <- if (!is.null(rule$type)) rule$type else "dropdown"
      tbl <- rhandsontable::hot_col(
        tbl,
        col = target_col,
        type = col_type,
        source = options,
        strict = FALSE,
        allowInvalid = TRUE,
        validator = htmlwidgets::JS("function(value, callback) { callback(true); }"),
        filter = FALSE
      )
    }
  }
  tbl
}

CASCADE_AUTOCOMPLETE_JS_TEMPLATE <- "
function(query, process) {
  try {
    var db = %s; var colIndices = %s; var dbColNames = %s;
    var hot = this.instance; var row = this.row;
    if (row === null || row === undefined) {
      var sel = hot.getSelected();
      if (sel && sel.length > 0) row = sel[0][0];
    }
    if (row === null || row === undefined) return process([]);

    var myDbName = '%s'; var myStepIdx = dbColNames.indexOf(myDbName);
    function cln(s) { return (s == null) ? '' : String(s).trim().toLowerCase().replace(/[^a-z0-9]/g, ''); }

    var valid = db;
    for (var i = 0; i < myStepIdx; i++) {
      var dbName = dbColNames[i]; var visualColIdx = colIndices[i];
      if (visualColIdx < 0) continue;
      var val = hot.getDataAtCell(row, visualColIdx);
      var cleanVal = cln(val);
      if (cleanVal !== '' && cleanVal !== 'na') {
        valid = valid.filter(function(r) { return cln(r[dbName]) === cleanVal; });
      }
    }
    var out = [];
    valid.forEach(function(r) {
      var x = r[myDbName];
      if (cln(x) !== '' && cln(x) !== 'na') {
        var strX = String(x).trim();
        if (out.indexOf(strX) === -1) out.push(strX);
      }
    });
    process(out.sort());
  } catch(err) { console.error('Cascade Error:', err); process([]); }
}
"

apply_manure_cascade_dropdowns_fast <- function(tbl, current_manure_df, cached_json) {
  if (is.null(cached_json)) return(tbl)

  col_indices <- match(MANURE_CASCADE_COLUMNS, names(current_manure_df)) - 1
  col_indices[is.na(col_indices)] <- -1

  indices_json <- jsonlite::toJSON(col_indices, auto_unbox = TRUE)
  db_names_json <- jsonlite::toJSON(MANURE_CASCADE_COLUMNS, auto_unbox = TRUE)

  for (i in seq_along(MANURE_CASCADE_COLUMNS)) {
    col_name <- MANURE_CASCADE_COLUMNS[i]
    if (col_indices[i] >= 0) {
      js <- sprintf(CASCADE_AUTOCOMPLETE_JS_TEMPLATE, cached_json, indices_json, db_names_json, col_name)
      tbl <- rhandsontable::hot_col(
        tbl,
        col = col_name,
        type = "dropdown",
        source = htmlwidgets::JS(js),
        strict = FALSE,
        allowInvalid = TRUE,
        validator = htmlwidgets::JS("function(value, callback) { callback(true); }"),
        filter = FALSE
      )
    }
  }

  reset_js <- sprintf("
    function(el, x) {
      var hot = this.hot; var colIndices = %s;
      hot.addHook('afterChange', function(changes, source) {
        if (!changes || source === 'cascade' || source === 'loadData') return;
        changes.forEach(function(change) {
          var row = change[0], prop = change[1];
          var changedVisualCol = hot.propToCol(prop) ?? prop;
          var stepIdx = colIndices.indexOf(changedVisualCol);
          if (stepIdx < 0) return;
          for (var i = stepIdx + 1; i < colIndices.length; i++) {
            var targetVisualCol = colIndices[i];
            if (targetVisualCol >= 0) hot.setDataAtCell(row, targetVisualCol, '', 'cascade');
          }
        });
      });
    }
  ", indices_json)
  htmlwidgets::onRender(tbl, reset_js)
}

apply_diet_ingredient_cascade <- function(tbl, df, feed_char_df) {
  if (is.null(feed_char_df) || nrow(feed_char_df) == 0 ||
      !"ingredient_type" %in% names(feed_char_df) ||
      !"ingredient" %in% names(feed_char_df) ||
      !"ingredient_type" %in% names(df) ||
      !"ingredient" %in% names(df)) {
    return(tbl)
  }

  types <- unique(na.omit(as.character(feed_char_df$ingredient_type)))
  types <- types[trimws(types) != ""]
  lookup <- list()
  for (tp in types) {
    ingrs <- sort(unique(na.omit(as.character(feed_char_df$ingredient[feed_char_df$ingredient_type == tp]))))
    lookup[[tp]] <- ingrs[trimws(ingrs) != ""]
  }
  all_ingrs <- sort(unique(na.omit(as.character(feed_char_df$ingredient))))
  lookup[["all"]] <- all_ingrs[trimws(all_ingrs) != ""]

  lookup_json <- jsonlite::toJSON(lookup, auto_unbox = FALSE)
  type_col_idx <- match("ingredient_type", names(df)) - 1
  ingr_col_idx <- match("ingredient", names(df)) - 1

  cascade_js <- sprintf("
    function(query, process) {
      try {
        var lookup = %s;
        var hot = this.instance; var row = this.row;
        if (row === null || row === undefined) {
          var sel = hot.getSelected();
          if (sel && sel.length > 0) row = sel[0][0];
        }
        if (row === null || row === undefined) return process(lookup.all || []);
        var typeVal = hot.getDataAtCell(row, %d);
        if (typeVal && lookup[typeVal]) {
          process(lookup[typeVal]);
        } else {
          process(lookup.all || []);
        }
      } catch(err) {
        console.error('Diet Ingredient Cascade Error:', err);
        process([]);
      }
    }
  ", lookup_json, type_col_idx)

  tbl <- rhandsontable::hot_col(
    tbl,
    col = "ingredient",
    type = "dropdown",
    source = htmlwidgets::JS(cascade_js),
    strict = FALSE,
    allowInvalid = TRUE,
    validator = htmlwidgets::JS("function(value, callback) { callback(true); }"),
    filter = FALSE
  )

  reset_js <- sprintf("
    function(el, x) {
      var hot = this.hot;
      var typeCol = %d;
      var ingrCol = %d;
      var lookup = %s;
      hot.addHook('afterChange', function(changes, source) {
        if (!changes || source === 'cascade' || source === 'loadData') return;
        changes.forEach(function(change) {
          var row = change[0], prop = change[1], oldVal = change[2], newVal = change[3];
          var visualCol = hot.propToCol(prop) ?? prop;
          if (visualCol === typeCol && oldVal !== newVal) {
            var currentIngr = hot.getDataAtCell(row, ingrCol);
            if (currentIngr && lookup[newVal] && lookup[newVal].indexOf(currentIngr) === -1) {
              hot.setDataAtCell(row, ingrCol, '', 'cascade');
            }
          }
        });
      });
    }
  ", type_col_idx, ingr_col_idx, lookup_json)

  htmlwidgets::onRender(tbl, reset_js)
}

# Helper to build 7-part manure combination key for IPCC verification
build_manure_combo_key <- function(df) {
  cols <- c("system_base", "management_months", "system_climate", "system_subclimate", "system_variant", "climate_zone", "climate_moisture")
  parts <- lapply(cols, function(cname) {
    if (cname %in% names(df)) {
      v <- trimws(as.character(df[[cname]]))
      v[is.na(v) | v == "NA"] <- ""
      v
    } else {
      rep("", nrow(df))
    }
  })
  do.call(paste, c(parts, list(sep = " | ")))
}

# --- Pre-Flight Data Health Validation Engine ---

validate_project_data <- function(rv, automatic_cycle = FALSE) {
  issues <- list()

  add_issue <- function(severity, table_id, tab_label, title, message) {
    issues[[length(issues) + 1]] <<- list(
      severity  = severity,   # "error" (blocking) or "warning" (advisory)
      table_id  = table_id,
      tab_label = tab_label,
      title     = title,
      message   = message
    )
  }

  # --- 1. Biological Animal Type Validity ---
  valid_rum_types <- c("cattle", "sheep", "goat")
  if (!is.null(rv$def) && "animal_type" %in% names(rv$def)) {
    def_types <- tolower(trimws(as.character(rv$def$animal_type)))
    def_types <- def_types[!is.na(def_types) & def_types != ""]
    bad_rum <- setdiff(unique(def_types), valid_rum_types)
    if (length(bad_rum) > 0) {
      add_issue("error", "def", "Ruminants", "Invalid Animal Type",
                paste0("Unsupported animal_type in Ruminants: '", paste(bad_rum, collapse = "', '"), "'. Must be: cattle, sheep, or goat."))
    }
  }

  valid_mono_types <- c("swine", "poultry")
  if (!is.null(rv$mono) && "animal_type" %in% names(rv$mono)) {
    mono_types <- tolower(trimws(as.character(rv$mono$animal_type)))
    mono_types <- mono_types[!is.na(mono_types) & mono_types != ""]
    bad_mono <- setdiff(unique(mono_types), valid_mono_types)
    if (length(bad_mono) > 0) {
      add_issue("error", "mono", "Monogastrics", "Invalid Animal Type",
                paste0("Unsupported animal_type in Monogastrics: '", paste(bad_mono, collapse = "', '"), "'. Must be: swine or poultry."))
    }
  }

  def_keys <- if (!is.null(rv$def)) build_keys(rv$def) else character(0)
  mono_keys <- if (!is.null(rv$mono)) build_keys(rv$mono) else character(0)
  all_bio_keys <- union(def_keys, mono_keys)

  # --- 2. Census Validation (Census is the Source of Truth) ---
  if (is.null(rv$census) || nrow(rv$census) == 0) {
    add_issue("error", "census", "Census", "Census is Empty", "You must configure at least one livestock cohort in Census.")
    return(issues)
  }

  valid_census <- rv$census[!is.na(rv$census$animal_tag) & trimws(as.character(rv$census$animal_tag)) != "", , drop = FALSE]
  if (nrow(valid_census) == 0) {
    add_issue("error", "census", "Census", "No Animal Tags Defined", "No valid animal_tag found in Census.")
    return(issues)
  }

  all_census_keys <- build_keys(valid_census)

  # Negative population check
  if ("population" %in% names(valid_census)) {
    neg_pop <- valid_census[!is.na(valid_census$population) & as.numeric(valid_census$population) < 0, ]
    if (nrow(neg_pop) > 0) {
      tags_bad <- unique(na.omit(neg_pop$animal_tag))
      add_issue("error", "census", "Census", "Negative Population Count",
                paste0("Population cannot be negative for: ", paste(tags_bad, collapse = ", "), "."))
    }
  }

  # Active cohorts in Census (population > 0) identified by 4-key: animal_tag, region, subregion, class_flex
  active_census <- if ("population" %in% names(valid_census)) {
    valid_census[!is.na(valid_census$population) & as.numeric(valid_census$population) > 0, , drop = FALSE]
  } else {
    valid_census
  }

  if (nrow(active_census) == 0) {
    add_issue("warning", "census", "Census", "Zero Animals in Census",
              "All animals in Census have a population of 0. Enter a head count (> 0) to calculate emissions.")
    return(issues)
  }

  census_keys <- build_keys(active_census)

  # Helper to extract the animal_tag from a 4-key
  extract_key_tag <- function(keys) {
    vapply(strsplit(keys, " \\| "), function(p) if (length(p) > 0) p[1] else "", character(1), USE.NAMES = FALSE)
  }

  # Infer active livestock species from Census cohorts
  census_animal_tags <- unique(valid_census$animal_tag)
  census_species <- character(0)
  if (!is.null(rv$def) && nrow(rv$def) > 0 && "animal_type" %in% names(rv$def)) {
    m_def <- rv$def$animal_type[rv$def$animal_tag %in% census_animal_tags]
    census_species <- union(census_species, tolower(na.omit(as.character(m_def))))
  }
  if (!is.null(rv$mono) && nrow(rv$mono) > 0 && "animal_type" %in% names(rv$mono)) {
    m_mono <- rv$mono$animal_type[rv$mono$animal_tag %in% census_animal_tags]
    census_species <- union(census_species, tolower(na.omit(as.character(m_mono))))
  }
  if (any(grepl("cattle|bull|cow", census_animal_tags, ignore.case = TRUE))) census_species <- union(census_species, "cattle")
  if (any(grepl("sheep|ewe|ram", census_animal_tags, ignore.case = TRUE))) census_species <- union(census_species, "sheep")
  if (any(grepl("goat", census_animal_tags, ignore.case = TRUE))) census_species <- union(census_species, "goat")
  if (any(grepl("sow|boar|pig", census_animal_tags, ignore.case = TRUE))) census_species <- union(census_species, "swine")
  if (any(grepl("hen|poultry|chicken", census_animal_tags, ignore.case = TRUE))) census_species <- union(census_species, "poultry")

  valid_demo_tags <- if (length(census_species) > 0) {
    unname(unlist(DEMOGRAPHIC_SPECIES_MAP[census_species]))
  } else {
    ALL_DEMOGRAPHIC_TAGS
  }

  check_orphan_cohorts <- function(table_id, tab_label, candidate_keys) {
    if (length(candidate_keys) == 0) return()
    raw_orphans <- setdiff(candidate_keys, all_census_keys)
    raw_orphans <- raw_orphans[raw_orphans != ""]
    if (length(raw_orphans) == 0) return()

    orphan_tags <- extract_key_tag(raw_orphans)

    is_valid_demo <- orphan_tags %in% valid_demo_tags
    demo_keys <- raw_orphans[is_valid_demo]

    is_wrong_spec <- (!is_valid_demo) & (orphan_tags %in% ALL_DEMOGRAPHIC_TAGS)
    wrong_spec_keys <- raw_orphans[is_wrong_spec]

    true_orphans <- raw_orphans[!is_valid_demo & !is_wrong_spec]

    if (length(true_orphans) > 0) {
      fmt_orphans <- vapply(true_orphans, format_cohort_label, character(1), USE.NAMES = FALSE)
      fmt_orphans <- fmt_orphans[fmt_orphans != ""]
      if (length(fmt_orphans) > 0) {
        add_issue("error", table_id, tab_label, "Cohort Not in Census",
                  paste0("Cohort(s) in ", tab_label, " do not exist in Census: ", paste(fmt_orphans, collapse = ", "),
                         ". Census is the source of truth for livestock. Register this cohort in Census or check for typos in animal_tag, region, subregion, or class_flex."))
      }
    }

    if (length(wrong_spec_keys) > 0) {
      fmt_wrong <- vapply(wrong_spec_keys, format_cohort_label, character(1), USE.NAMES = FALSE)
      fmt_wrong <- fmt_wrong[fmt_wrong != ""]
      if (length(fmt_wrong) > 0) {
        add_issue("error", table_id, tab_label, "Missing Breeding Adults in Census",
                  paste0("Cohort(s) in ", tab_label, " (", paste(fmt_wrong, collapse = ", "),
                         ") are demographic offspring categories, but their parent breeding species is not present in Census."))
      }
    }

    if (!isTRUE(automatic_cycle) && length(demo_keys) > 0) {
      fmt_demo <- vapply(demo_keys, format_cohort_label, character(1), USE.NAMES = FALSE)
      fmt_demo <- fmt_demo[fmt_demo != ""]
      if (length(fmt_demo) > 0) {
        add_issue("warning", table_id, tab_label, "Demographic Cohort Without Automatic Demography",
                  paste0("Cohort(s) in ", tab_label, " (", paste(fmt_demo, collapse = ", "),
                         ") are offspring/replacement categories not listed in Census. Check 'Automatic Herd Demography' in the sidebar to calculate their populations automatically, or add them to Census for direct headcount."))
      }
    }
  }

  # --- 3. Biological Definitions: Ruminants & Monogastrics must define Census cohorts ---
  # Check for cohorts in Ruminants (def) not in Census
  if (!is.null(rv$def) && nrow(rv$def) > 0) {
    valid_def <- rv$def[!is.na(rv$def$animal_tag) & trimws(as.character(rv$def$animal_tag)) != "", , drop = FALSE]
    if (nrow(valid_def) > 0) {
      check_orphan_cohorts("def", "Ruminants", build_keys(valid_def))
    }
  }

  # Check for cohorts in Monogastrics (mono) not in Census
  if (!is.null(rv$mono) && nrow(rv$mono) > 0) {
    valid_mono <- rv$mono[!is.na(rv$mono$animal_tag) & trimws(as.character(rv$mono$animal_tag)) != "", , drop = FALSE]
    if (nrow(valid_mono) > 0) {
      check_orphan_cohorts("mono", "Monogastrics", build_keys(valid_mono))
    }
  }

  # Active cohorts from Census missing in Ruminants or Monogastrics
  missing_bio_keys <- setdiff(census_keys, all_bio_keys)
  if (length(missing_bio_keys) > 0) {
    fmt_bio <- vapply(missing_bio_keys, format_cohort_label, character(1), USE.NAMES = FALSE)
    add_issue("error", "def", "Ruminants", "Missing Biological Definition",
              paste0("Active cohort(s) from Census missing in Ruminants or Monogastrics: ", paste(fmt_bio, collapse = ", "),
                     ". Ensure animal_tag, region, subregion, and class_flex match exactly."))
  }

  # --- 4. Weights Validation (4-Key matching) ---
  # Check for cohorts in weights not in Census
  if (!is.null(rv$weights) && nrow(rv$weights) > 0) {
    valid_w <- rv$weights[!is.na(rv$weights$animal_tag) & trimws(as.character(rv$weights$animal_tag)) != "", , drop = FALSE]
    if (nrow(valid_w) > 0) {
      check_orphan_cohorts("weights", "Weights", build_keys(valid_w))
    }
  }

  weights_keys <- if (!is.null(rv$weights)) build_keys(rv$weights) else character(0)
  missing_weights_keys <- setdiff(census_keys, weights_keys)
  if (length(missing_weights_keys) > 0) {
    fmt_weights <- vapply(missing_weights_keys, format_cohort_label, character(1), USE.NAMES = FALSE)
    add_issue("error", "weights", "Weights", "Missing Animal Weights",
              paste0("Active cohort(s) from Census missing in Weights: ", paste(fmt_weights, collapse = ", "),
                     ". Ensure animal_tag, region, subregion, and class_flex match exactly."))
  }

  if (!is.null(rv$weights) && nrow(rv$weights) > 0) {
    clean_weights <- rv$weights[!is.na(rv$weights$animal_tag) & trimws(as.character(rv$weights$animal_tag)) != "", , drop = FALSE]
    if (nrow(clean_weights) > 0) {
      w_keys <- build_keys(clean_weights)
      act_weights <- clean_weights[w_keys %in% census_keys, , drop = FALSE]

      if (nrow(act_weights) > 0 && "live_weight_kg" %in% names(act_weights)) {
        bad_lw <- act_weights[is.na(act_weights$live_weight_kg) | as.numeric(act_weights$live_weight_kg) <= 0, , drop = FALSE]
        if (nrow(bad_lw) > 0) {
          fmt_lw <- vapply(build_keys(bad_lw), format_cohort_label, character(1), USE.NAMES = FALSE)
          fmt_lw <- fmt_lw[fmt_lw != ""]
          if (length(fmt_lw) > 0) {
            add_issue("error", "weights", "Weights", "Missing Live Weight",
                      paste0("live_weight_kg must be > 0 for: ", paste(unique(fmt_lw), collapse = ", "), "."))
          }
        }
      }

      if (nrow(act_weights) > 0 && "productive_period_days" %in% names(act_weights)) {
        bad_ppd <- act_weights[is.na(act_weights$productive_period_days) | as.numeric(act_weights$productive_period_days) <= 0, , drop = FALSE]
        if (nrow(bad_ppd) > 0) {
          fmt_ppd <- vapply(build_keys(bad_ppd), format_cohort_label, character(1), USE.NAMES = FALSE)
          fmt_ppd <- fmt_ppd[fmt_ppd != ""]
          if (length(fmt_ppd) > 0) {
            add_issue("error", "weights", "Weights", "Missing Productive Period",
                      paste0("productive_period_days must be > 0 for: ", paste(unique(fmt_ppd), collapse = ", "), "."))
          }
        }
      }
    }
  }

  # --- 5. Manure Management Validation (4-Key matching, Allocations, Whitespace & System Variants) ---
  # Check for cohorts in manure not in Census
  if (!is.null(rv$manure) && nrow(rv$manure) > 0) {
    valid_m <- rv$manure[!is.na(rv$manure$animal_tag) & trimws(as.character(rv$manure$animal_tag)) != "", , drop = FALSE]
    if (nrow(valid_m) > 0) {
      check_orphan_cohorts("manure", "Manure", build_keys(valid_m))
    }
  }
  manure_keys <- if (!is.null(rv$manure)) build_keys(rv$manure) else character(0)
  missing_manure_keys <- setdiff(census_keys, manure_keys)
  missing_manure_keys <- missing_manure_keys[missing_manure_keys != ""]
  if (length(missing_manure_keys) > 0) {
    fmt_manure <- vapply(missing_manure_keys, format_cohort_label, character(1), USE.NAMES = FALSE)
    fmt_manure <- fmt_manure[fmt_manure != ""]
    if (length(fmt_manure) > 0) {
      add_issue("error", "manure", "Manure", "Missing Manure System",
                paste0("Active cohort(s) from Census missing in Manure: ", paste(fmt_manure, collapse = ", "),
                       ". Ensure animal_tag, region, subregion, and class_flex match exactly."))
    }
  }

  if (!is.null(rv$manure) && nrow(rv$manure) > 0) {
    clean_manure <- rv$manure[!is.na(rv$manure$animal_tag) & trimws(as.character(rv$manure$animal_tag)) != "", , drop = FALSE]
    if (nrow(clean_manure) > 0) {
      m_keys <- build_keys(clean_manure)
      m_active <- clean_manure[m_keys %in% census_keys, , drop = FALSE]

      # Allocations sum check per active cohort (sum should equal 1.0 / 100%)
      if (nrow(m_active) > 0 && "allocation" %in% names(m_active)) {
        m_clean_alloc <- m_active[!is.na(m_active$allocation), , drop = FALSE]
        if (nrow(m_clean_alloc) > 0) {
          m_cohort_tags <- build_keys(m_clean_alloc)
          alloc_sums <- tapply(as.numeric(m_clean_alloc$allocation), m_cohort_tags, sum, na.rm = TRUE)
          bad_allocs <- alloc_sums[!is.na(alloc_sums) & abs(alloc_sums - 1.0) > 0.05]
          if (length(bad_allocs) > 0) {
            bad_desc <- vapply(names(bad_allocs), function(k) {
              lbl <- format_cohort_label(k)
              if (lbl != "") paste0(lbl, " (", round(bad_allocs[[k]] * 100, 1), "%)") else ""
            }, character(1), USE.NAMES = FALSE)
            bad_desc <- bad_desc[bad_desc != ""]
            if (length(bad_desc) > 0) {
              add_issue("warning", "manure", "Manure", "Manure Allocations != 100%",
                        paste0("Manure system allocations should sum to 100% (1.0). Current sums: ", paste(bad_desc, collapse = ", "), "."))
            }
          }
        }
      }
    }

    # Manure System Options, Whitespace & Variant Validity across ALL rows in rv$manure
    # 1. Whitespace check on system and cohort columns across all rows
    sys_cols <- intersect(names(rv$manure), c(MANURE_CASCADE_COLUMNS, "animal_tag", "region", "subregion", "class_flex"))
    for (sc in sys_cols) {
      vals <- as.character(rv$manure[[sc]])
      bad_ws <- which(!is.na(vals) & vals != "" & (vals != trimws(vals) | trimws(vals) == ""))
      if (length(bad_ws) > 0) {
        bad_ws_vals <- unique(vals[bad_ws])
        bad_ws_vals <- bad_ws_vals[bad_ws_vals != ""]
        val_disp <- if (length(bad_ws_vals) > 0) paste0("'", paste(bad_ws_vals, collapse = "', '"), "'") else "(empty spaces)"
        add_issue("error", "manure", "Manure", "Whitespace in Manure System",
                  paste0("Column '", sc, "' in Manure has leading, trailing, or blank spaces: ", val_disp, ". Please remove spaces."))
      }
    }

    # 2. System base & system_variant validity on ALL rows where either is entered
    if (!is.null(rv$ipcc_mm) && nrow(rv$ipcc_mm) > 0) {
      valid_bases <- unique(na.omit(trimws(as.character(rv$ipcc_mm$system_base))))
      for (r in seq_len(nrow(rv$manure))) {
        s_base_raw <- as.character(rv$manure$system_base[r])
        s_base <- if (!is.na(s_base_raw)) trimws(s_base_raw) else ""
        if (s_base == "NA") s_base <- ""

        s_var_raw <- if ("system_variant" %in% names(rv$manure)) as.character(rv$manure$system_variant[r]) else ""
        s_var <- if (!is.na(s_var_raw)) trimws(s_var_raw) else ""
        if (s_var == "NA") s_var <- ""

        if (s_base == "" && s_var == "") next

        cohort_lbl <- format_cohort_label(build_keys(rv$manure[r, , drop = FALSE]))
        if (cohort_lbl == "") cohort_lbl <- paste("Row", r)

        if (s_var != "" && s_base == "") {
          add_issue("error", "manure", "Manure", "Invalid System Variant",
                    paste0("In Manure (", cohort_lbl, "): system_variant '", s_var, "' is specified without a system_base. Select system_base first."))
        } else if (s_base != "") {
          if (!s_base %in% valid_bases) {
            add_issue("error", "manure", "Manure", "Invalid System Base",
                      paste0("In Manure (", cohort_lbl, "): '", s_base, "' is not a recognized system_base in IPCC library."))
          } else if (s_var != "") {
            valid_vars <- unique(na.omit(trimws(as.character(rv$ipcc_mm$system_variant[rv$ipcc_mm$system_base == s_base]))))
            valid_vars <- valid_vars[valid_vars != "" & valid_vars != "NA"]

            if (!s_var %in% valid_vars) {
              add_issue("error", "manure", "Manure", "Invalid System Variant",
                        paste0("In Manure (", cohort_lbl, "): '", s_var, "' is not a valid system_variant for system_base '", s_base,
                               "'. Valid options: ", if (length(valid_vars) > 0) paste(valid_vars, collapse = ", ") else "None (leave blank)", "."))
            }
          }
        }
      }

      # 3. Master combination check for rows with a valid system_base
      m_has_base <- rv$manure[!is.na(rv$manure$system_base) & trimws(as.character(rv$manure$system_base)) %in% valid_bases, , drop = FALSE]
      if (nrow(m_has_base) > 0) {
        user_combos <- build_manure_combo_key(m_has_base)
        master_combos <- build_manure_combo_key(rv$ipcc_mm)
        invalid_combos <- setdiff(user_combos, master_combos)
        if (length(invalid_combos) > 0) {
          # Check if error wasn't already reported for base or variant
          reported_titles <- sapply(issues, function(x) x$title)
          if (!any(c("Invalid System Base", "Invalid System Variant", "Whitespace in Manure System") %in% reported_titles)) {
            add_issue("error", "manure", "Manure", "Invalid Climate/System Combination",
                      paste0("Invalid manure combination detected in Manure: '", invalid_combos[1], "'. Does not exist in IPCC Manure library."))
          }
        }
      }
    }
  }

  # --- 6. Diet Cross-References ---
  act_def <- if (!is.null(rv$def) && nrow(rv$def) > 0) {
    clean_def <- rv$def[!is.na(rv$def$animal_tag) & trimws(as.character(rv$def$animal_tag)) != "", , drop = FALSE]
    clean_def[build_keys(clean_def) %in% census_keys, , drop = FALSE]
  } else NULL

  act_mono <- if (!is.null(rv$mono) && nrow(rv$mono) > 0) {
    clean_mono <- rv$mono[!is.na(rv$mono$animal_tag) & trimws(as.character(rv$mono$animal_tag)) != "", , drop = FALSE]
    clean_mono[build_keys(clean_mono) %in% census_keys, , drop = FALSE]
  } else NULL

  assigned_diets <- c(
    if (!is.null(act_def) && "diet_tag" %in% names(act_def)) act_def$diet_tag,
    if (!is.null(act_mono) && "diet_tag" %in% names(act_mono)) act_mono$diet_tag
  )
  assigned_diets <- unique(na.omit(trimws(as.character(assigned_diets))))
  assigned_diets <- assigned_diets[assigned_diets != ""]

  defined_diets <- if (!is.null(rv$diet_prof) && "diet_tag" %in% names(rv$diet_prof)) unique(na.omit(trimws(as.character(rv$diet_prof$diet_tag)))) else character(0)
  defined_diets <- defined_diets[defined_diets != ""]

  missing_diets <- setdiff(assigned_diets, defined_diets)
  if (length(missing_diets) > 0) {
    add_issue("error", "diet_prof", "Diet Profiles", "Undefined Diet Profile",
              paste0("Diets assigned to active animals but not defined in Diet Profiles: ", paste(missing_diets, collapse = ", "), "."))
  }

  # --- 7. Diet Profiles Macro Shares Sum (100%) ---
  if (!is.null(rv$diet_prof) && nrow(rv$diet_prof) > 0) {
    share_cols <- intersect(c("forage_share", "concentrate_share", "milk_share", "milk_replacer_share"), names(rv$diet_prof))
    if (length(share_cols) > 0) {
      active_target_diets <- if (length(assigned_diets) > 0) assigned_diets else defined_diets
      dp_clean <- rv$diet_prof[!is.na(rv$diet_prof$diet_tag) & rv$diet_prof$diet_tag %in% active_target_diets, , drop = FALSE]
      if (nrow(dp_clean) > 0) {
        for (r in seq_len(nrow(dp_clean))) {
          d_tag <- dp_clean$diet_tag[r]
          row_shares <- as.numeric(dp_clean[r, share_cols])
          total_share <- sum(row_shares, na.rm = TRUE)
          if (abs(total_share - 100) > 1.0) {
            add_issue("error", "diet_prof", "Diet Profiles", "Diet Shares != 100%",
                      paste0("Diet '", d_tag, "' macro shares sum to ", round(total_share, 1), "% (must sum to 100%)."))
          }
        }
      }
    }
  }

  # --- 8. Diet Ingredients Validation (Shares 100% per Category & Unknown Ingredients) ---
  if (!is.null(rv$diet_ingr) && nrow(rv$diet_ingr) > 0) {
    di_clean <- rv$diet_ingr[!is.na(rv$diet_ingr$diet_tag) & rv$diet_ingr$diet_tag != "", , drop = FALSE]

    # Check unknown ingredients against feed_characteristics
    if (!is.null(rv$feed_char) && "ingredient" %in% names(rv$feed_char) && "ingredient" %in% names(di_clean)) {
      known_ingrs <- unique(na.omit(trimws(as.character(rv$feed_char$ingredient))))
      used_ingrs <- unique(na.omit(trimws(as.character(di_clean$ingredient))))
      used_ingrs <- used_ingrs[used_ingrs != ""]
      unknown_ingrs <- setdiff(used_ingrs, known_ingrs)
      if (length(unknown_ingrs) > 0) {
        add_issue("error", "diet_ingr", "Ingredients", "Unknown Ingredient",
                  paste0("Ingredient(s) not found in Feed Characteristics database: '", paste(unknown_ingrs, collapse = "', '"), "'."))
      }
    }

    # Check shares sum to 100% within each diet_tag + ingredient_type category
    if (all(c("diet_tag", "ingredient_type", "ingredient_share") %in% names(di_clean))) {
      target_diets <- if (length(assigned_diets) > 0) intersect(assigned_diets, unique(di_clean$diet_tag)) else unique(di_clean$diet_tag)
      di_target <- di_clean[di_clean$diet_tag %in% target_diets & !is.na(di_clean$ingredient_type) & di_clean$ingredient_type != "", , drop = FALSE]
      if (nrow(di_target) > 0) {
        di_target$ing_share_num <- suppressWarnings(as.numeric(di_target$ingredient_share))
        sums_df <- stats::aggregate(ing_share_num ~ diet_tag + ingredient_type, data = di_target, FUN = sum, na.rm = TRUE)
        bad_sums <- sums_df[abs(sums_df$ing_share_num - 100) > 0.5, ]
        if (nrow(bad_sums) > 0) {
          for (b in seq_len(nrow(bad_sums))) {
            add_issue("error", "diet_ingr", "Ingredients", "Ingredient Shares != 100%",
                      paste0("In diet '", bad_sums$diet_tag[b], "', '", bad_sums$ingredient_type[b], "' ingredient shares sum to ",
                             round(bad_sums$ing_share_num[b], 1), "% (must sum to 100%)."))
          }
        }
      }
    }

    # Missing ingredients for active diets
    ingr_diets <- unique(na.omit(trimws(as.character(di_clean$diet_tag))))
    missing_ingr_diets <- setdiff(assigned_diets, ingr_diets)
    if (length(missing_ingr_diets) > 0) {
      add_issue("warning", "diet_ingr", "Ingredients", "Diet Without Ingredients",
                paste0("Active diet profiles with no ingredients configured: ", paste(missing_ingr_diets, collapse = ", "), "."))
    }
  }

  issues
}

