# Step0 Resolve selected scenario name from SimulationYear ----
# The NACE, Taxpayer and ETR aggregations need the snapshot dataset
# that corresponds to the selected simulation year, not always t0.
# shiny_scenario_lookup_pit maps year -> scenario label (t0, t1, t2, ...).

selected_scenario_pd <- pd_pit_results$scenarios_pd[
  match(
    as.integer(SimulationYear),
    pd_pit_results$scenario_years_pd
  )
]

if (is.na(selected_scenario_pd) || length(selected_scenario_pd) == 0L)
  stop(paste0(
    "SimulationYear ", SimulationYear,
    " was not found in pd_pit_results$scenario_years_pd. ",
    "Available years: ",
    paste(pd_pit_results$scenario_years_pd, collapse = ", ")
  ))

cat("Selected scenario for cross-section aggregation:", selected_scenario_pd, "\n")


# I. Aggregation of revenue ----


# 1. Standard PIT column names ----

standard_pit_columns <- c(
  "year",
  "Current law (LCU Mil)",
  "Simulation (LCU Mil)",
  "Fiscal impact (LCU Mil)"
)


# 2. Standardize PIT summary tables ----

standardize_pit_summary <- function(
    summary_table,
    module_name
) {
  
  summary_table <- data.table::as.data.table(
    data.table::copy(summary_table)
  )
  
  if (ncol(summary_table) < 4L) {
    stop(
      paste0(
        "The summary table for module ",
        module_name,
        " must contain at least four columns."
      )
    )
  }
  
  summary_table <- summary_table[, 1:4, with = FALSE]
  
  data.table::setnames(
    summary_table,
    old = names(summary_table),
    new = standard_pit_columns
  )
  
  summary_table[, Module := module_name]
  
  data.table::setcolorder(
    summary_table,
    c(
      "year",
      "Module",
      "Current law (LCU Mil)",
      "Simulation (LCU Mil)",
      "Fiscal impact (LCU Mil)"
    )
  )
  
  return(summary_table[])
}


# 3. Combine PIT module summaries ----
#
# CORRECTION - PD fiscal output:
#   pd_pit_results$pit_summary_pd now uses amount_due_calc as its
#   primary output (columns 1-4). This is net of withholding credits
#   AND advance payments. Using gross bracket tax caused double
#   counting with WM (~1.6 LCU Mil) and IS (~17 LCU Mil).
#   The standardize_pit_summary() function takes columns 1-4 of each
#   summary table, so no change is needed here - the fix is upstream
#   in TaxCalculator_v1_PD.R Step15.

pit_summary_df <- data.table::rbindlist(
  list(
    standardize_pit_summary(pd_pit_results$pit_summary_pd, "PD"),
    standardize_pit_summary(wm_pit_results$pit_summary_wm, "WM"),
    standardize_pit_summary(wr_pit_results$pit_summary_wr, "WR"),
    standardize_pit_summary(qs_pit_results$pit_summary_qs, "QS"),
    standardize_pit_summary(is_pit_results$pit_summary_is, "IS")
  ),
  use.names = TRUE,
  fill      = FALSE
)


# 4. Order combined PIT summary ----

data.table::setorder(pit_summary_df, year, Module)


# 5. Print combined PIT summary ----

print(pit_summary_df)


# 6. Aggregate Fiscal Impact by year ----
# Sum across all modules for each year to produce the top-level aggregate table.
# This is the table shown under "Aggregate Fiscal Impact" in the Shiny app.

pit_summary_agg_df <- pit_summary_df[
  ,
  .(
    `Current law (LCU Mil)`   = round(sum(`Current law (LCU Mil)`,   na.rm = TRUE), 1),
    `Simulation (LCU Mil)`    = round(sum(`Simulation (LCU Mil)`,    na.rm = TRUE), 1),
    `Fiscal impact (LCU Mil)` = round(sum(`Fiscal impact (LCU Mil)`, na.rm = TRUE), 1)
  ),
  by = .(year)
]

data.table::setorder(pit_summary_agg_df, year)

cat("\n=== Aggregate Fiscal Impact by Year ===\n")
print(pit_summary_agg_df)



# II. Aggregation by NACE ----


# 1. Module configuration ----
#
# CORRECTION - PD col:
#   OLD: tax_on_tax_inc_bracket_calc  (gross bracket tax - double counted)
#   NEW: amount_due_calc              (net after withholding credits and
#                                      advance payments - no double count)

pit_module_config <- list(
  
  list(
    module   = "PD",
    data_bu  = pd_pit_results$PIT_BU_list_pd_selected[[selected_scenario_pd]],
    data_sim = pd_pit_results$PIT_SIM_list_pd_selected[[selected_scenario_pd]],
    col      = "amount_due_calc"              # CORRECTED: was tax_on_tax_inc_bracket_calc
  ),
  
  list(
    module   = "WM",
    data_bu  = wm_pit_results$PIT_BU_list_wm_selected[[selected_scenario_pd]],
    data_sim = wm_pit_results$PIT_SIM_list_wm_selected[[selected_scenario_pd]],
    col      = "pitax_w_calc"
  ),
  
  list(
    module   = "WR",
    data_bu  = wr_pit_results$PIT_BU_list_wr_selected[[selected_scenario_pd]],
    data_sim = wr_pit_results$PIT_SIM_list_wr_selected[[selected_scenario_pd]],
    col      = "totalWithholdingTax_calc"
  ),
  
  list(
    module   = "QS",
    data_bu  = qs_pit_results$PIT_BU_list_qs_selected[[selected_scenario_pd]],
    data_sim = qs_pit_results$PIT_SIM_list_qs_selected[[selected_scenario_pd]],
    col      = "total_pit_qs_calc"
  ),
  
  list(
    module   = "IS",
    data_bu  = is_pit_results$PIT_BU_list_is_selected[[selected_scenario_pd]],
    data_sim = is_pit_results$PIT_SIM_list_is_selected[[selected_scenario_pd]],
    col      = "pit_commercial_services_calc"
  )
)


# 2. Helper: aggregate by section + description_en ----

aggregate_pit_module <- function(cfg) {
  
  dt_bu  <- data.table::as.data.table(data.table::copy(cfg$data_bu))
  dt_sim <- data.table::as.data.table(data.table::copy(cfg$data_sim))
  
  if (!all(c("section", "description_en", cfg$col) %in% names(dt_bu))) {
    stop(paste0("Module ", cfg$module, ": missing columns in BU dataset. Found: ",
                paste(names(dt_bu), collapse = ", ")))
  }
  if (!all(c("section", "description_en", cfg$col) %in% names(dt_sim))) {
    stop(paste0("Module ", cfg$module, ": missing columns in SIM dataset. Found: ",
                paste(names(dt_sim), collapse = ", ")))
  }
  
  agg_bu <- dt_bu[
    ,
    .(`Current law (LCU Mil)` = sum(get(cfg$col), na.rm = TRUE) / 1e6),
    by = .(section, description_en)
  ]
  
  agg_sim <- dt_sim[
    ,
    .(`Simulation (LCU Mil)` = sum(get(cfg$col), na.rm = TRUE) / 1e6),
    by = .(section, description_en)
  ]
  
  dt_agg <- merge(agg_bu, agg_sim, by = c("section", "description_en"), all = TRUE)
  
  dt_agg[
    ,
    `:=`(
      `Fiscal impact (LCU Mil)` = `Simulation (LCU Mil)` - `Current law (LCU Mil)`,
      Module                    = cfg$module
    )
  ]
  
  data.table::setcolorder(
    dt_agg,
    c(
      "Module", "section", "description_en",
      "Current law (LCU Mil)", "Simulation (LCU Mil)",
      "Fiscal impact (LCU Mil)"
    )
  )
  
  return(dt_agg[])
}


# 3. Apply to all modules and combine ----

pit_section_summary <- data.table::rbindlist(
  lapply(pit_module_config, aggregate_pit_module),
  use.names = TRUE,
  fill      = FALSE
)


# 4. Order ----

data.table::setorder(pit_section_summary, Module, section, description_en)


# 5. Print ----

print(pit_section_summary)


# III. Aggregation by Taxpayer Type ----


# 1. Module configuration ----
#
# CORRECTION - PD col:
#   OLD: tax_on_tax_inc_bracket_calc  (gross bracket tax - double counted)
#   NEW: amount_due_calc              (net after withholding credits and
#                                      advance payments - no double count)

pit_module_config_taxpayer <- list(
  
  list(
    module        = "PD",
    data_bu       = pd_pit_results$PIT_BU_list_pd_selected[[selected_scenario_pd]],
    data_sim      = pd_pit_results$PIT_SIM_list_pd_selected[[selected_scenario_pd]],
    col           = "amount_due_calc",        # CORRECTED: was tax_on_tax_inc_bracket_calc
    taxpayer_col  = "TaxpayerType"
  ),
  
  list(
    module        = "WM",
    data_bu       = wm_pit_results$PIT_BU_list_wm_selected[[selected_scenario_pd]],
    data_sim      = wm_pit_results$PIT_SIM_list_wm_selected[[selected_scenario_pd]],
    col           = "pitax_w_calc",
    taxpayer_col  = "TaxpayerType"
  ),
  
  list(
    module        = "WR",
    data_bu       = wr_pit_results$PIT_BU_list_wr_selected[[selected_scenario_pd]],
    data_sim      = wr_pit_results$PIT_SIM_list_wr_selected[[selected_scenario_pd]],
    col           = "totalWithholdingTax_calc",
    taxpayer_col  = "TaxpayerType"
  ),
  
  list(
    module        = "QS",
    data_bu       = qs_pit_results$PIT_BU_list_qs_selected[[selected_scenario_pd]],
    data_sim      = qs_pit_results$PIT_SIM_list_qs_selected[[selected_scenario_pd]],
    col           = "total_pit_qs_calc",
    taxpayer_col  = NULL,
    fixed_taxpayer = "Individual"
  ),
  
  list(
    module        = "IS",
    data_bu       = is_pit_results$PIT_BU_list_is_selected[[selected_scenario_pd]],
    data_sim      = is_pit_results$PIT_SIM_list_is_selected[[selected_scenario_pd]],
    col           = "pit_commercial_services_calc",
    taxpayer_col  = "TaxpayerType"
  )
)


# 2. Helper ----

aggregate_pit_module_by_taxpayer <- function(cfg) {
  
  dt_bu  <- data.table::as.data.table(data.table::copy(cfg$data_bu))
  dt_sim <- data.table::as.data.table(data.table::copy(cfg$data_sim))
  
  if (!is.null(cfg$fixed_taxpayer)) {
    
    agg_bu  <- dt_bu[,  .(`Current law (LCU Mil)` = sum(get(cfg$col), na.rm = TRUE) / 1e6)]
    agg_sim <- dt_sim[, .(`Simulation (LCU Mil)`  = sum(get(cfg$col), na.rm = TRUE) / 1e6)]
    
    dt_agg <- data.table::data.table(
      TaxpayerType            = cfg$fixed_taxpayer,
      `Current law (LCU Mil)` = agg_bu$`Current law (LCU Mil)`,
      `Simulation (LCU Mil)`  = agg_sim$`Simulation (LCU Mil)`
    )
    
  } else {
    
    tc <- cfg$taxpayer_col
    
    if (!all(c(tc, cfg$col) %in% names(dt_bu))) {
      stop(paste0("Module ", cfg$module, ": missing columns in BU dataset. Found: ",
                  paste(names(dt_bu), collapse = ", ")))
    }
    if (!all(c(tc, cfg$col) %in% names(dt_sim))) {
      stop(paste0("Module ", cfg$module, ": missing columns in SIM dataset. Found: ",
                  paste(names(dt_sim), collapse = ", ")))
    }
    
    agg_bu <- dt_bu[
      ,
      .(`Current law (LCU Mil)` = sum(get(cfg$col), na.rm = TRUE) / 1e6),
      by = .(TaxpayerType = get(tc))
    ]
    
    agg_sim <- dt_sim[
      ,
      .(`Simulation (LCU Mil)` = sum(get(cfg$col), na.rm = TRUE) / 1e6),
      by = .(TaxpayerType = get(tc))
    ]
    
    dt_agg <- merge(agg_bu, agg_sim, by = "TaxpayerType", all = TRUE)
  }
  
  dt_agg[
    ,
    `:=`(
      `Fiscal impact (LCU Mil)` = `Simulation (LCU Mil)` - `Current law (LCU Mil)`,
      Module                    = cfg$module
    )
  ]
  
  data.table::setcolorder(
    dt_agg,
    c(
      "Module", "TaxpayerType",
      "Current law (LCU Mil)", "Simulation (LCU Mil)",
      "Fiscal impact (LCU Mil)"
    )
  )
  
  return(dt_agg[])
}


# 3. Apply to all modules and combine ----
# Always rebuilt from the module results, so the source table has the
# original (local language) names and the dictionary merge works on every run.

pit_by_taxpayer_type <- data.table::rbindlist(
  lapply(pit_module_config_taxpayer, aggregate_pit_module_by_taxpayer),
  use.names = TRUE,
  fill      = FALSE
)


# 4. Translate TaxpayerType to English ----

dict_tp <- data.table::as.data.table(dictionary_taxpayers_type)
dict_tp <- unique(dict_tp, by = "TaxpayerType")

pit_by_taxpayer_type_en <- merge(
  data.table::copy(pit_by_taxpayer_type),
  dict_tp,
  by    = "TaxpayerType",
  all.x = TRUE,
  sort  = FALSE
)

unmatched <- unique(pit_by_taxpayer_type_en[is.na(TaxpayerType_en), TaxpayerType])
if (length(unmatched) > 0L) {
  warning("TaxpayerType values without English translation: ",
          paste(unmatched, collapse = ", "))
}

# Fall back to the original name when there is no translation
pit_by_taxpayer_type_en[
  , TaxpayerType := data.table::fcoalesce(TaxpayerType_en, TaxpayerType)
]
pit_by_taxpayer_type_en[, TaxpayerType_en := NULL]

# Several local names can map to one English name, so aggregate
pit_by_taxpayer_type_en <- pit_by_taxpayer_type_en[
  ,
  .(
    `Current law (LCU Mil)`   = round(sum(`Current law (LCU Mil)`,   na.rm = TRUE), 1),
    `Simulation (LCU Mil)`    = round(sum(`Simulation (LCU Mil)`,    na.rm = TRUE), 1),
    `Fiscal impact (LCU Mil)` = round(sum(`Fiscal impact (LCU Mil)`, na.rm = TRUE), 1)
  ),
  by = .(Module, TaxpayerType)
]


# 5. Use English version downstream ----
# Charts-PIT_Revenues.R and the export read pit_by_taxpayer_type,
# so it is overwritten with the English version.

pit_by_taxpayer_type <- data.table::copy(pit_by_taxpayer_type_en)

data.table::setcolorder(pit_by_taxpayer_type, c("Module", "TaxpayerType"))
data.table::setorder(pit_by_taxpayer_type, Module, TaxpayerType)


# 6. Print ----

print(pit_by_taxpayer_type)


# IV. Aggregation NACE ETR ----


# 1. Module configuration ----
#
# CORRECTIONS APPLIED:
#
#   PD col        : amount_due_calc         (was tax_on_tax_inc_bracket_calc)
#   PD income_col : total_inc_rank_calc     (was taxable_inc_before_tax_calc)
#
#   WR col        : pit_distribution_calc   (was totalWithholdingTax_calc)
#   WR income_col : income_distribution_calc (was totalMonthlyPayments_calc + ...)
#
#   WR CORRECTION DETAIL:
#     totalWithholdingTax_calc includes standardWithholdingTax_calc (D12)
#     which is tax on bank interest (73,533 LCU Mil). Using this as the
#     tax numerator while income_distribution_calc excludes bank interest
#     from the denominator produced impossible ETRs > 10% at NACE sector
#     level (e.g. sector R = 11.6%, sector S = 16.5%).
#
#     pit_distribution_calc = D14 + D17 + D20 only - exactly matched
#     to the income components in income_distribution_calc (D13+D15+D16+D19).
#     Maximum possible ETR = 9% (D14 rent rate). No sector can exceed this.

pit_module_config_etr <- list(
  
  list(
    module     = "PD",
    data_bu    = pd_pit_results$PIT_BU_list_pd_selected[[selected_scenario_pd]],
    data_sim   = pd_pit_results$PIT_SIM_list_pd_selected[[selected_scenario_pd]],
    col        = "amount_due_calc",           # CORRECTED: was tax_on_tax_inc_bracket_calc
    income_col = "total_inc_rank_calc"        # CORRECTED: was taxable_inc_before_tax_calc
  ),
  
  list(
    module     = "WM",
    data_bu    = wm_pit_results$PIT_BU_list_wm_selected[[selected_scenario_pd]],
    data_sim   = wm_pit_results$PIT_SIM_list_wm_selected[[selected_scenario_pd]],
    col        = "pitax_w_calc",
    income_col = "inc_bef_tax_withheld_calc"
  ),
  
  list(
    module     = "WR",
    data_bu    = wr_pit_results$PIT_BU_list_wr_selected[[selected_scenario_pd]],
    data_sim   = wr_pit_results$PIT_SIM_list_wr_selected[[selected_scenario_pd]],
    col        = "pit_distribution_calc",     # CORRECTED: was totalWithholdingTax_calc
    # pit_distribution_calc = D14 + D17 + D20 (excludes D12 bank interest tax)
    # Matched to income_distribution_calc which excludes bank interest income.
    # Prevents impossible ETRs > 9% at NACE sector level.
    income_col = "income_distribution_calc"   # CORRECTED: was totalMonthlyPayments_calc + ...
  ),
  
  list(
    module     = "QS",
    data_bu    = qs_pit_results$PIT_BU_list_qs_selected[[selected_scenario_pd]],
    data_sim   = qs_pit_results$PIT_SIM_list_qs_selected[[selected_scenario_pd]],
    col        = "total_pit_qs_calc",
    income_col = c(
      "grossIncomeCommercial",
      "grossIncomeServices",
      "gross_rent_income"
    )
  ),
  
  list(
    module     = "IS",
    data_bu    = is_pit_results$PIT_BU_list_is_selected[[selected_scenario_pd]],
    data_sim   = is_pit_results$PIT_SIM_list_is_selected[[selected_scenario_pd]],
    col        = "pit_commercial_services_calc",
    income_col = c(
      "grossIncomeCommercial",
      "grossIncomeServices"
    )
  )
)


# 2. Helper: compute ETR by section + description_en ----

aggregate_pit_etr <- function(cfg) {
  
  dt_bu  <- data.table::as.data.table(data.table::copy(cfg$data_bu))
  dt_sim <- data.table::as.data.table(data.table::copy(cfg$data_sim))
  
  required_bu  <- c("section", "description_en", cfg$col, cfg$income_col)
  required_sim <- c("section", "description_en", cfg$col, cfg$income_col)
  
  if (!all(required_bu %in% names(dt_bu))) {
    stop(paste0("Module ", cfg$module, ": missing columns in BU dataset. Found: ",
                paste(names(dt_bu), collapse = ", ")))
  }
  if (!all(required_sim %in% names(dt_sim))) {
    stop(paste0("Module ", cfg$module, ": missing columns in SIM dataset. Found: ",
                paste(names(dt_sim), collapse = ", ")))
  }
  
  dt_bu[,  income_base := rowSums(.SD, na.rm = TRUE), .SDcols = cfg$income_col]
  dt_sim[, income_base := rowSums(.SD, na.rm = TRUE), .SDcols = cfg$income_col]
  
  agg_bu <- dt_bu[
    ,
    .(
      pit_bu    = sum(get(cfg$col), na.rm = TRUE),
      income_bu = sum(income_base,  na.rm = TRUE)
    ),
    by = .(section, description_en)
  ]
  
  agg_sim <- dt_sim[
    ,
    .(
      pit_sim    = sum(get(cfg$col), na.rm = TRUE),
      income_sim = sum(income_base,  na.rm = TRUE)
    ),
    by = .(section, description_en)
  ]
  
  dt_agg <- merge(agg_bu, agg_sim, by = c("section", "description_en"), all = TRUE)
  
  dt_agg <- dt_agg[
    !(pit_bu == 0 & income_bu == 0 & pit_sim == 0 & income_sim == 0)
  ]
  
  dt_agg[
    ,
    `:=`(
      `ETR Current law (%)` = data.table::fifelse(
        income_bu  > 0, (pit_bu  / income_bu)  * 100, NA_real_
      ),
      `ETR Simulation (%)` = data.table::fifelse(
        income_sim > 0, (pit_sim / income_sim) * 100, NA_real_
      )
    )
  ]
  
  dt_agg[
    ,
    `:=`(
      `ETR change (pp)` = `ETR Simulation (%)` - `ETR Current law (%)`,
      Module            = cfg$module
    )
  ]
  
  dt_agg[, c("pit_bu", "income_bu", "pit_sim", "income_sim") := NULL]
  
  data.table::setcolorder(
    dt_agg,
    c(
      "Module", "section", "description_en",
      "ETR Current law (%)", "ETR Simulation (%)",
      "ETR change (pp)"
    )
  )
  
  return(dt_agg[])
}


# 3. Apply to all modules and combine ----

pit_etr_by_section <- data.table::rbindlist(
  lapply(pit_module_config_etr, aggregate_pit_etr),
  use.names = TRUE,
  fill      = FALSE
)


# 4. Order ----

data.table::setorder(pit_etr_by_section, Module, section, description_en)


# 5. Print ----

print(pit_etr_by_section)


#==============================================================================
# V. AGGREGATION BY INCOME DECILE / PERCENTILE
#==============================================================================

# IMPORTANT:
# Groups are defined using CURRENT-LAW income.
# Therefore taxpayers remain in the same income group when comparing
# Current Law vs Simulation.
#
# Decile:
#   D1 = lowest 10%
#   ...
#   D10 = highest 10%
#
# Percentile:
#   P1 = lowest 1%
#   ...
#   P100 = highest 1%
#==============================================================================


#==============================================================================
# METHODOLOGY NOTE - DATA CLEANING AND DEDUPLICATION
#==============================================================================
#
# The Kosovo PIT system collects tax through five separate modules:
#
#   PD  - Personal Declaration (annual reconciliation form)
#   WM  - Wage withholding (employer monthly statement)
#   WR  - Withholding tax (interest, rents, lottery, non-resident)
#   QS  - Small business flat-rate regime
#   IS  - Standard commercial / services regime
#
# A single taxpayer can appear in more than one module simultaneously.
# Investigation confirmed the following overlaps:
#
#   PD taxpayers also in QS :     3  (0.0% of QS)  - negligible
#   PD taxpayers also in WR : 4,873  (11.3% of WR) - significant
#   PD taxpayers also in IS : 1,043  (3.0% of IS)  - meaningful
#   Total PD with any overlap: 5,480  (48.8% of all PD taxpayers)
#
# Without correction, stacking all modules and summing per taxpayer
# produces double-counted income and PIT figures, distorting the
# income ranking and ETR profile used for distributional analysis.
#
# CORRECTIONS APPLIED:
#
# CORRECTION 1 - PD income column:
#   OLD: taxable_inc_before_tax_calc
#   NEW: total_inc_rank_calc
#        Total gross income with negative business income floored at zero.
#
# CORRECTION 2 - PD PIT column:
#   OLD: tax_on_tax_inc_bracket_calc  (gross bracket tax = 25 LCU Mil)
#   NEW: amount_due_calc              (net amount due = 8 LCU Mil)
#        = bracket_tax [30]
#        - withholding credits [36]   (WM + WR already captured)
#        - advance payments [40]      (IS quarterly already captured)
#
# CORRECTION 3 - Deduplication across modules:
#   PD is the MASTER record. QS/IS/WR rows dropped for PD taxpayers.
#   WM always kept - separate fiscal flow.
#
# CORRECTION 4 - WR income column:
#   income_distribution_calc used instead of totalMonthlyPayments_calc.
#   Excludes all D11 bank interest flows (9 banks = 73,533 LCU Mil).
#   Income base: grossRentPaid + nonResidentPayments + nonBizPayments.
#
# CORRECTION 5 - WR PIT column for distributional ETR:
#   pit_distribution_calc used instead of totalWithholdingTax_calc.
#   Excludes standardWithholdingTax_calc (D12 bank interest tax).
#   Matched exactly to income_distribution_calc to prevent ETR > 9%.
#   PIT base: D14 (rent 9%) + D17 (non-resident 5%) + D20 (non-biz 3%/1%).
#
# CORRECTION 6 - WR zero-income exclusion:
#   WR taxpayers with income_bu == 0 excluded from distributional dataset.
#   These filers have no measurable personal income on the WR form.
#   Including them placed them artificially in D1 with real withholding
#   tax, inflating D1 ETR.
#==============================================================================


#------------------------------------------------------------------------------
# 1. Recover selected scenario name and build PD master ID set
#------------------------------------------------------------------------------

selected_scenario_pd <- pd_pit_results$selected_scenario_pd

pd_ids_distribution <- unique(
  as.character(
    pd_pit_results$PIT_BU_list_pd_selected[[selected_scenario_pd]]$id_n
  )
)

cat(
  sprintf(
    "\nDistribution - PD master records: %s taxpayers\n",
    format(length(pd_ids_distribution), big.mark = ",")
  )
)


#------------------------------------------------------------------------------
# 2. Convert each module to taxpayer level
#------------------------------------------------------------------------------

prepare_distribution_module <- function(cfg) {
  
  dt_bu <- data.table::as.data.table(data.table::copy(cfg$data_bu))
  dt_sim <- data.table::as.data.table(data.table::copy(cfg$data_sim))
  
  required_bu  <- c(cfg$id_col, cfg$tax_col, cfg$income_col)
  required_sim <- c(cfg$id_col, cfg$tax_col, cfg$income_col)
  
  if (!all(required_bu %in% names(dt_bu))) {
    stop(paste0("Module ", cfg$module, ": missing columns in BU distribution data: ",
                paste(setdiff(required_bu, names(dt_bu)), collapse = ", ")))
  }
  
  if (!all(required_sim %in% names(dt_sim))) {
    stop(paste0("Module ", cfg$module, ": missing columns in SIM distribution data: ",
                paste(setdiff(required_sim, names(dt_sim)), collapse = ", ")))
  }
  
  dt_bu[,  income_base := rowSums(.SD, na.rm = TRUE), .SDcols = cfg$income_col]
  dt_sim[, income_base := rowSums(.SD, na.rm = TRUE), .SDcols = cfg$income_col]
  
  bu_taxpayer <- dt_bu[
    ,
    .(
      income_bu = sum(income_base,      na.rm = TRUE),
      pit_bu    = sum(get(cfg$tax_col), na.rm = TRUE)
    ),
    by = .(id_n = as.character(get(cfg$id_col)))
  ]
  
  sim_taxpayer <- dt_sim[
    ,
    .(
      income_sim = sum(income_base,      na.rm = TRUE),
      pit_sim    = sum(get(cfg$tax_col), na.rm = TRUE)
    ),
    by = .(id_n = as.character(get(cfg$id_col)))
  ]
  
  out <- merge(bu_taxpayer, sim_taxpayer, by = "id_n", all = TRUE)
  
  out[
    ,
    `:=`(
      income_bu  = data.table::fcoalesce(income_bu,  0),
      income_sim = data.table::fcoalesce(income_sim, 0),
      pit_bu     = data.table::fcoalesce(pit_bu,     0),
      pit_sim    = data.table::fcoalesce(pit_sim,    0),
      Module     = cfg$module
    )
  ]
  
  return(out[])
}


#------------------------------------------------------------------------------
# 3. Module configuration
#
# CORRECTIONS APPLIED:
#
#   PD tax_col    : amount_due_calc          (CORRECTION 2)
#   PD income_col : total_inc_rank_calc      (CORRECTION 1)
#
#   WR tax_col    : pit_distribution_calc    (CORRECTION 5)
#     = D14 + D17 + D20 only.
#     Excludes standardWithholdingTax_calc (D12 bank interest tax).
#     Matched to income_distribution_calc to keep ETR within [0%, 9%].
#
#   WR income_col : income_distribution_calc (CORRECTION 4)
#     = grossRentPaid + nonResidentGrossPayment +
#       nonResidentServicePayment + nonBusinessIndividualPayments
#     Excludes all D11 bank interest flows.
#------------------------------------------------------------------------------

pit_module_config_distribution <- list(
  
  list(
    module     = "PD",
    data_bu    = pd_pit_results$PIT_BU_list_pd_selected[[selected_scenario_pd]],
    data_sim   = pd_pit_results$PIT_SIM_list_pd_selected[[selected_scenario_pd]],
    id_col     = "id_n",
    tax_col    = "amount_due_calc",           # CORRECTION 2
    income_col = "total_inc_rank_calc"        # CORRECTION 1
  ),
  
  list(
    module     = "WM",
    data_bu    = wm_pit_results$PIT_BU_list_wm_selected[[selected_scenario_pd]],
    data_sim   = wm_pit_results$PIT_SIM_list_wm_selected[[selected_scenario_pd]],
    id_col     = "id_n",
    tax_col    = "pitax_w_calc",
    income_col = "inc_bef_tax_withheld_calc"
  ),
  
  list(
    module     = "WR",
    data_bu    = wr_pit_results$PIT_BU_list_wr_selected[[selected_scenario_pd]],
    data_sim   = wr_pit_results$PIT_SIM_list_wr_selected[[selected_scenario_pd]],
    id_col     = "id_n",
    tax_col    = "pit_distribution_calc",     # CORRECTION 5
    income_col = "income_distribution_calc"   # CORRECTION 4
  ),
  
  list(
    module     = "QS",
    data_bu    = qs_pit_results$PIT_BU_list_qs_selected[[selected_scenario_pd]],
    data_sim   = qs_pit_results$PIT_SIM_list_qs_selected[[selected_scenario_pd]],
    id_col     = "id_n",
    tax_col    = "total_pit_qs_calc",
    income_col = c(
      "grossIncomeCommercial",
      "grossIncomeServices",
      "gross_rent_income"
    )
  ),
  
  list(
    module     = "IS",
    data_bu    = is_pit_results$PIT_BU_list_is_selected[[selected_scenario_pd]],
    data_sim   = is_pit_results$PIT_SIM_list_is_selected[[selected_scenario_pd]],
    id_col     = "id_n",
    tax_col    = "pit_commercial_services_calc",
    income_col = c(
      "grossIncomeCommercial",
      "grossIncomeServices"
    )
  )
)


#------------------------------------------------------------------------------
# 4. Convert each module to taxpayer level
#------------------------------------------------------------------------------

pit_distribution_by_module <- data.table::rbindlist(
  lapply(pit_module_config_distribution, prepare_distribution_module),
  use.names = TRUE,
  fill      = FALSE
)


#------------------------------------------------------------------------------
# 5. Deduplicate - PD is master record; remove PD taxpayers from QS/IS/WR
#
# CORRECTION 3: PD is master. QS/IS/WR rows dropped for PD taxpayers.
# WM always kept - covers wage withholding, separate fiscal flow.
#------------------------------------------------------------------------------

pit_distribution_by_module_dedup <- data.table::rbindlist(
  list(
    pit_distribution_by_module[Module == "PD"],
    pit_distribution_by_module[Module == "WM"],
    pit_distribution_by_module[Module == "QS" & !id_n %in% pd_ids_distribution],
    pit_distribution_by_module[Module == "IS" & !id_n %in% pd_ids_distribution],
    pit_distribution_by_module[Module == "WR" & !id_n %in% pd_ids_distribution]
  ),
  use.names = TRUE,
  fill      = FALSE
)


#------------------------------------------------------------------------------
# 5b. Remove WR taxpayers with zero distributional income
#
# CORRECTION 6: WR filers with income_distribution_calc = 0 have no
# measurable personal income on the WR form. Including them places them
# artificially in D1 with real withholding tax, inflating D1 ETR.
# Their tax is correctly captured in aggregate fiscal tables.
# They are excluded ONLY from the distributional income ranking.
#------------------------------------------------------------------------------

wr_zero_income_removed <- nrow(
  pit_distribution_by_module_dedup[
    Module == "WR" & income_bu == 0 & income_sim == 0
  ]
)

pit_distribution_by_module_dedup <- pit_distribution_by_module_dedup[
  !(Module == "WR" & income_bu == 0 & income_sim == 0)
]

cat(
  sprintf(
    "WR zero-income exclusion: %s taxpayers removed from distributional ranking\n\n",
    format(wr_zero_income_removed, big.mark = ",")
  )
)


# Deduplication diagnostics
cat(
  sprintf(
    "Distribution deduplication:\n  Before : %s rows\n  After  : %s rows\n  Removed: %s rows (PD taxpayers dropped from QS/IS/WR)\n\n",
    format(nrow(pit_distribution_by_module),       big.mark = ","),
    format(nrow(pit_distribution_by_module_dedup), big.mark = ","),
    format(
      nrow(pit_distribution_by_module) -
        nrow(pit_distribution_by_module_dedup),
      big.mark = ","
    )
  )
)


#------------------------------------------------------------------------------
# 6. Aggregate across modules to ONE observation per taxpayer
#------------------------------------------------------------------------------

pit_distribution_taxpayer <- pit_distribution_by_module_dedup[
  ,
  .(
    income_bu  = sum(income_bu,  na.rm = TRUE),
    income_sim = sum(income_sim, na.rm = TRUE),
    pit_bu     = sum(pit_bu,     na.rm = TRUE),
    pit_sim    = sum(pit_sim,    na.rm = TRUE)
  ),
  by = .(id_n)
]

pit_distribution_taxpayer <- pit_distribution_taxpayer[
  !is.na(id_n) & trimws(id_n) != ""
]

cat(
  sprintf(
    "Distribution master dataset: %s unique taxpayers\n\n",
    format(nrow(pit_distribution_taxpayer), big.mark = ",")
  )
)


#------------------------------------------------------------------------------
# 7. Rank taxpayers according to CURRENT-LAW income
#------------------------------------------------------------------------------

data.table::setorder(pit_distribution_taxpayer, income_bu, id_n)

pit_distribution_taxpayer[, IncomeRank := seq_len(.N)]

pit_distribution_taxpayer[
  ,
  Percentile := pmin(100L, ceiling(100 * IncomeRank / .N))
]

pit_distribution_taxpayer[
  ,
  Decile := pmin(10L, ceiling(10 * IncomeRank / .N))
]

pit_distribution_taxpayer[
  ,
  `:=`(
    PercentileGroup = sprintf("P%03d", Percentile),
    DecileGroup     = sprintf("D%02d", Decile)
  )
]

pit_distribution_taxpayer[, fiscal_impact := pit_sim - pit_bu]


#==============================================================================
# 8. DECILE AGGREGATION
#==============================================================================

pit_by_decile <- pit_distribution_taxpayer[
  ,
  .(
    Taxpayers                              = .N,
    `Gross Income - Current Law (LCU Mil)` = sum(income_bu,      na.rm = TRUE) / 1e6,
    `Gross Income - Simulation (LCU Mil)`  = sum(income_sim,     na.rm = TRUE) / 1e6,
    `PIT - Current Law (LCU Mil)`          = sum(pit_bu,         na.rm = TRUE) / 1e6,
    `PIT - Simulation (LCU Mil)`           = sum(pit_sim,        na.rm = TRUE) / 1e6,
    `PIT Fiscal Impact (LCU Mil)`          = sum(fiscal_impact,  na.rm = TRUE) / 1e6
  ),
  by = .(Decile, DecileGroup)
]

pit_by_decile[
  ,
  `:=`(
    `ETR - Current Law (%)` = data.table::fifelse(
      `Gross Income - Current Law (LCU Mil)` > 0,
      100 * `PIT - Current Law (LCU Mil)` / `Gross Income - Current Law (LCU Mil)`,
      NA_real_
    ),
    `ETR - Simulation (%)` = data.table::fifelse(
      `Gross Income - Simulation (LCU Mil)` > 0,
      100 * `PIT - Simulation (LCU Mil)` / `Gross Income - Simulation (LCU Mil)`,
      NA_real_
    )
  )
]

pit_by_decile[, `ETR Change (pp)` := `ETR - Simulation (%)` - `ETR - Current Law (%)`]

money_columns_distribution <- c(
  "Gross Income - Current Law (LCU Mil)",
  "Gross Income - Simulation (LCU Mil)",
  "PIT - Current Law (LCU Mil)",
  "PIT - Simulation (LCU Mil)",
  "PIT Fiscal Impact (LCU Mil)"
)

etr_columns_distribution <- c(
  "ETR - Current Law (%)",
  "ETR - Simulation (%)",
  "ETR Change (pp)"
)

pit_by_decile[
  , (money_columns_distribution) := lapply(.SD, round, digits = 2),
  .SDcols = money_columns_distribution
]

pit_by_decile[
  , (etr_columns_distribution) := lapply(.SD, round, digits = 2),
  .SDcols = etr_columns_distribution
]

data.table::setcolorder(
  pit_by_decile,
  c(
    "Decile", "DecileGroup", "Taxpayers",
    "Gross Income - Current Law (LCU Mil)",
    "Gross Income - Simulation (LCU Mil)",
    "PIT - Current Law (LCU Mil)",
    "PIT - Simulation (LCU Mil)",
    "PIT Fiscal Impact (LCU Mil)",
    "ETR - Current Law (%)",
    "ETR - Simulation (%)",
    "ETR Change (pp)"
  )
)

data.table::setorder(pit_by_decile, Decile)


#==============================================================================
# 9. PERCENTILE AGGREGATION
#==============================================================================

pit_by_percentile <- pit_distribution_taxpayer[
  ,
  .(
    Taxpayers                              = .N,
    `Gross Income - Current Law (LCU Mil)` = sum(income_bu,     na.rm = TRUE) / 1e6,
    `Gross Income - Simulation (LCU Mil)`  = sum(income_sim,    na.rm = TRUE) / 1e6,
    `PIT - Current Law (LCU Mil)`          = sum(pit_bu,        na.rm = TRUE) / 1e6,
    `PIT - Simulation (LCU Mil)`           = sum(pit_sim,       na.rm = TRUE) / 1e6,
    `PIT Fiscal Impact (LCU Mil)`          = sum(fiscal_impact, na.rm = TRUE) / 1e6
  ),
  by = .(Percentile, PercentileGroup, Decile)
]

pit_by_percentile[
  ,
  `:=`(
    `ETR - Current Law (%)` = data.table::fifelse(
      `Gross Income - Current Law (LCU Mil)` > 0,
      100 * `PIT - Current Law (LCU Mil)` / `Gross Income - Current Law (LCU Mil)`,
      NA_real_
    ),
    `ETR - Simulation (%)` = data.table::fifelse(
      `Gross Income - Simulation (LCU Mil)` > 0,
      100 * `PIT - Simulation (LCU Mil)` / `Gross Income - Simulation (LCU Mil)`,
      NA_real_
    )
  )
]

pit_by_percentile[, `ETR Change (pp)` := `ETR - Simulation (%)` - `ETR - Current Law (%)`]

pit_by_percentile[
  , (money_columns_distribution) := lapply(.SD, round, digits = 2),
  .SDcols = money_columns_distribution
]

pit_by_percentile[
  , (etr_columns_distribution) := lapply(.SD, round, digits = 2),
  .SDcols = etr_columns_distribution
]

data.table::setcolorder(
  pit_by_percentile,
  c(
    "Percentile", "PercentileGroup", "Decile", "Taxpayers",
    "Gross Income - Current Law (LCU Mil)",
    "Gross Income - Simulation (LCU Mil)",
    "PIT - Current Law (LCU Mil)",
    "PIT - Simulation (LCU Mil)",
    "PIT Fiscal Impact (LCU Mil)",
    "ETR - Current Law (%)",
    "ETR - Simulation (%)",
    "ETR Change (pp)"
  )
)

data.table::setorder(pit_by_percentile, Percentile)


#==============================================================================
# 10. DIAGNOSTICS
#==============================================================================

cat("\n=== PIT Distribution by Gross Income Decile ===\n")
print(pit_by_decile)

cat("\n=== PIT Distribution by Gross Income Percentile ===\n")
print(pit_by_percentile)

cat(
  "\nNumber of unique taxpayers used for distribution:",
  nrow(pit_distribution_taxpayer), "\n"
)

cat("\n=== Module membership distribution ===\n")
print(
  pit_distribution_by_module_dedup[
    , .(taxpayers = data.table::uniqueN(id_n)), by = Module
  ][order(-taxpayers)]
)

cat("\n=== PIT contribution by module (LCU Mil) ===\n")
print(
  pit_distribution_by_module_dedup[
    ,
    .(
      taxpayers  = data.table::uniqueN(id_n),
      income_bu  = round(sum(income_bu,  na.rm = TRUE) / 1e6, 2),
      pit_bu     = round(sum(pit_bu,     na.rm = TRUE) / 1e6, 2)
    ),
    by = Module
  ][order(-pit_bu)]
)


# Step11 Cleanup ----

rm(dict_tp, unmatched, pit_by_taxpayer_type_en)
gc(verbose = FALSE)
