###############################################################################
# PIT TAX EXPENDITURE SIMULATION - PD MODULE
###############################################################################


# Step 1 CREATE ZEROED PARAMETER TABLES ----

# Combined - both deductions zeroed
pit_simulation_parameters_te <- data.table::copy(
  pit_simulation_parameters_raw
)

pit_simulation_parameters_te[
  Parameters == "rate_ded_rent",
  Value := 0
]

pit_simulation_parameters_te[
  Parameters == "rate_ded_charitable",
  Value := 0
]

# Rent deduction only
pit_sim_params_te_rent <- data.table::copy(
  pit_simulation_parameters_raw
)

pit_sim_params_te_rent[
  Parameters == "rate_ded_rent",
  Value := 0
]

# Charitable deduction only
pit_sim_params_te_charity <- data.table::copy(
  pit_simulation_parameters_raw
)

pit_sim_params_te_charity[
  Parameters == "rate_ded_charitable",
  Value := 0
]

cat("=== Combined TE parameter check ===\n")
print(
  pit_simulation_parameters_te[
    Parameters %in% c("rate_ded_rent", "rate_ded_charitable"),
    .(Parameters, Year, Value)
  ]
)

cat("\n=== Rent TE parameter check ===\n")
print(
  pit_sim_params_te_rent[
    Parameters == "rate_ded_rent",
    .(Parameters, Year, Value)
  ]
)

cat("\n=== Charitable TE parameter check ===\n")
print(
  pit_sim_params_te_charity[
    Parameters == "rate_ded_charitable",
    .(Parameters, Year, Value)
  ]
)


# Step 2 RECOVER SCENARIO METADATA FROM pd_pit_results ----

scenario_years_pit_te <- pd_pit_results$scenario_years_pd
scenarios_pit_te      <- pd_pit_results$scenarios_pd
forecast_horizon_te   <- pd_pit_results$forecast_horizon_pd
scenario_lookup_te    <- pd_pit_results$scenario_lookup_pd

start_index_te <- match(
  as.integer(SimulationYear),
  scenario_years_pit_te
)

if (is.na(start_index_te)) {
  stop(sprintf(
    "SimulationYear %s not found in TE scenario years.",
    SimulationYear
  ))
}


# Step 3 REDEFINE FUNCTIONS ----

get_growth_factor_row <- function(
    scenario_name,
    growth_dt = pit_growth_factors_pd,
    variables = vars_to_grow
) {
  
  growth_row <- growth_dt[scenarios == scenario_name]
  
  if (nrow(growth_row) == 0L) {
    stop(sprintf(
      "Scenario '%s' was not found in pit_growth_factors_pd.",
      scenario_name
    ))
  }
  
  if (nrow(growth_row) > 1L) {
    stop(sprintf(
      "Scenario '%s' appears more than once in pit_growth_factors_pd.",
      scenario_name
    ))
  }
  
  growth_output <- setNames(rep(1, length(variables)), variables)
  
  for (variable_name in variables) {
    
    if (!variable_name %in% names(growth_row)) {
      growth_output[variable_name] <- 1
      next
    }
    
    factor_value <- growth_row[[variable_name]][1L]
    
    if (length(factor_value) == 0L || is.na(factor_value)) {
      growth_output[variable_name] <- 1
    } else if (!is.numeric(factor_value) || !is.finite(factor_value)) {
      stop(sprintf(
        "Invalid growth factor for variable '%s' in scenario '%s'.",
        variable_name, scenario_name
      ))
    } else {
      growth_output[variable_name] <- factor_value
    }
  }
  
  return(growth_output)
}


tax_calc_fun <- function(dt_scn, params_dt, year) {
  
  rate1               <- get_param_fun(params_dt, "rate1",               year)
  rate2               <- get_param_fun(params_dt, "rate2",               year)
  rate3               <- get_param_fun(params_dt, "rate3",               year)
  rate4               <- get_param_fun(params_dt, "rate4",               year)
  tbrk1               <- get_param_fun(params_dt, "tbrk1",               year)
  tbrk2               <- get_param_fun(params_dt, "tbrk2",               year)
  tbrk3               <- get_param_fun(params_dt, "tbrk3",               year)
  rate_ded_rent       <- get_param_fun(params_dt, "rate_ded_rent",       year)
  rate_ded_charitable <- get_param_fun(params_dt, "rate_ded_charitable", year)
  
  if (tbrk1 < 0 || tbrk2 < tbrk1 || tbrk3 < tbrk2) {
    stop(sprintf(
      "Tax brackets are not ordered correctly for year %s.", year
    ))
  }
  
  dt_scn[
    ,
    total_inc_calc := rowSums(.SD, na.rm = TRUE),
    .SDcols = income_vars
  ]
  
  #----------------------------------------------------------------------------
  # rent_deduction_calc - FIXED v2
  #
  # Uses base R if/else because rate_ded_rent is a scalar.
  # fifelse() requires equal-length arguments and fails when the scalar
  # condition (length 1) is combined with a vector result (length n).
  #
  # When rate_ded_rent = 0: entire rent deduction zeroed (scalar 0
  # recycled by data.table across all rows).
  # When rate_ded_rent > 0: original pmax() formula applied.
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    rent_deduction_calc := if (rate_ded_rent == 0) {
      0
    } else {
      pmax(
        fcoalesce(ded_rents_expen_rents10pct, 0),
        rate_ded_rent * fcoalesce(gross_rents, 0)
      )
    }
  ]
  
  dt_scn[
    ,
    total_ded_calc :=
      rent_deduction_calc             +
      fcoalesce(ded_pen_cont,      0) +
      fcoalesce(ded_exp_int_prop,  0) +
      fcoalesce(other_allowed_ded, 0)
  ]
  
  dt_scn[
    ,
    taxable_amount_calc := total_inc_calc - total_ded_calc
  ]
  
  #----------------------------------------------------------------------------
  # charity_contribution_calc - FIXED v2
  #
  # Same scalar/vector issue. Uses base R if/else.
  # When rate_ded_charitable = 0: deduction zeroed.
  # When rate_ded_charitable > 0: original pmin() formula applied.
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    charity_contribution_calc := if (rate_ded_charitable == 0) {
      0
    } else {
      pmin(
        fcoalesce(dis_charity_contribution, 0),
        pmax(taxable_amount_calc * rate_ded_charitable, 0)
      )
    }
  ]
  
  dt_scn[
    ,
    tot_additional_ded_calc :=
      charity_contribution_calc +
      fcoalesce(loss_carried_for, 0)
  ]
  
  dt_scn[
    ,
    taxable_inc_before_tax_calc := pmax(
      taxable_amount_calc - tot_additional_ded_calc,
      0
    )
  ]
  
  dt_scn[
    ,
    tax_on_tax_inc_bracket_calc := {
      tax_base <- fcoalesce(taxable_inc_before_tax_calc, 0)
      rate1 * pmin(tax_base, tbrk1) +
        rate2 * pmin(tbrk2 - tbrk1, pmax(0, tax_base - tbrk1)) +
        rate3 * pmin(tbrk3 - tbrk2, pmax(0, tax_base - tbrk2)) +
        rate4 * pmax(0, tax_base - tbrk3)
    }
  ]
  
  invisible(dt_scn)
}


# Step 4 BUSINESS-AS-USUAL PATH ----

start.time.te <- proc.time()

PIT_BU_list_pd_te <- list()
dt_scn_BU_te      <- data.table::copy(pit_raw_pd)

for (i in seq_along(scenarios_pit_te)) {
  
  scenario_name_te <- scenarios_pit_te[i]
  scenario_year_te <- scenario_years_pit_te[i]
  
  growth_values_te <- get_growth_factor_row(scenario_name_te)
  
  apply_growth_factors(
    dt            = dt_scn_BU_te,
    growth_values = growth_values_te
  )
  
  tax_calc_fun(
    dt_scn    = dt_scn_BU_te,
    params_dt = pit_simulation_parameters_raw,
    year      = scenario_year_te
  )
  
  dt_scn_BU_te[
    ,
    `:=`(
      scenario             = scenario_name_te,
      simulation_data_year = scenario_year_te
    )
  ]
  
  PIT_BU_list_pd_te[[scenario_name_te]] <- data.table::copy(dt_scn_BU_te)
}

rm(dt_scn_BU_te)


# Step 5 HELPER - RUN ONE SIM PATH ----

run_sim_path_te <- function(params_sim) {
  
  PIT_SIM <- list()
  
  if (start_index_te > 1L) {
    for (i in seq_len(start_index_te - 1L)) {
      scenario_name_te <- scenarios_pit_te[i]
      PIT_SIM[[scenario_name_te]] <- data.table::copy(
        PIT_BU_list_pd_te[[scenario_name_te]]
      )
    }
  }
  
  if (start_index_te == 1L) {
    dt_scn_SIM <- data.table::copy(pit_raw_pd)
  } else {
    previous_scenario_te <- scenarios_pit_te[start_index_te - 1L]
    dt_scn_SIM <- data.table::copy(
      PIT_BU_list_pd_te[[previous_scenario_te]]
    )
  }
  
  for (i in seq.int(from = start_index_te, to = length(scenarios_pit_te))) {
    
    scenario_name_te <- scenarios_pit_te[i]
    scenario_year_te <- scenario_years_pit_te[i]
    
    growth_values_te <- get_growth_factor_row(scenario_name_te)
    
    apply_growth_factors(
      dt            = dt_scn_SIM,
      growth_values = growth_values_te
    )
    
    tax_calc_fun(
      dt_scn    = dt_scn_SIM,
      params_dt = params_sim,
      year      = scenario_year_te
    )
    
    dt_scn_SIM[
      ,
      `:=`(
        scenario             = scenario_name_te,
        simulation_data_year = scenario_year_te
      )
    ]
    
    PIT_SIM[[scenario_name_te]] <- data.table::copy(dt_scn_SIM)
  }
  
  rm(dt_scn_SIM)
  return(PIT_SIM)
}


# Step 6 RUN ALL THREE SIM PATHS ----

cat("Running combined TE simulation (rent + charitable)...\n")
PIT_SIM_list_pd_te <- run_sim_path_te(
  params_sim = pit_simulation_parameters_te
)

cat("Running TE simulation - rent deduction only...\n")
PIT_SIM_list_pd_te_rent <- run_sim_path_te(
  params_sim = pit_sim_params_te_rent
)

cat("Running TE simulation - charitable deduction only...\n")
PIT_SIM_list_pd_te_charity <- run_sim_path_te(
  params_sim = pit_sim_params_te_charity
)

message("All TE simulation paths completed.\n")


# Step 7 AGGREGATE BU AND COMBINED SIM ----

summary_BU_te  <- summarize_PIT_fun_dt(PIT_list = PIT_BU_list_pd_te,  suffix = "_bu")
summary_SIM_te <- summarize_PIT_fun_dt(PIT_list = PIT_SIM_list_pd_te, suffix = "_sim")

merged_PIT_BU_SIM_pd_te <- merge(
  summary_BU_te, summary_SIM_te,
  by  = "scenarios",
  all = TRUE
)

merged_PIT_BU_SIM_pd_te <- merge(
  scenario_lookup_te,
  merged_PIT_BU_SIM_pd_te,
  by    = "scenarios",
  all.x = TRUE,
  sort  = FALSE
)

data.table::setorder(merged_PIT_BU_SIM_pd_te, year)

data.table::setcolorder(
  merged_PIT_BU_SIM_pd_te,
  c(
    "year", "scenarios",
    setdiff(names(merged_PIT_BU_SIM_pd_te), c("year", "scenarios"))
  )
)


# Step 8 CONVERT TO LCU MILLIONS ----

aggregate_value_columns_te <- setdiff(
  names(merged_PIT_BU_SIM_pd_te),
  c("year", "scenarios")
)

numeric_aggregate_columns_te <- aggregate_value_columns_te[
  vapply(
    merged_PIT_BU_SIM_pd_te[, ..aggregate_value_columns_te],
    is.numeric,
    logical(1)
  )
]

merged_PIT_BU_SIM_pd_te[
  ,
  (numeric_aggregate_columns_te) := lapply(.SD, function(v) v / 1e6),
  .SDcols = numeric_aggregate_columns_te
]


# Step 9 FISCAL IMPACT SUMMARY ----

pit_summary_pd_te <- merged_PIT_BU_SIM_pd_te[
  ,
  .(
    year,
    `Current law (LCU Mil)`     = round(tax_on_tax_inc_bracket_calc_bu,  3),
    `Benchmark (LCU Mil)`       = round(tax_on_tax_inc_bracket_calc_sim, 3),
    `Tax Expenditure (LCU Mil)` = round(
      tax_on_tax_inc_bracket_calc_sim - tax_on_tax_inc_bracket_calc_bu,
      3
    )
  )
]


# Step 10 NACE SECTION BREAKDOWN ----

extract_te_nace_fun <- function(dt, scenario) {
  dt[, .(tax_on_tax_inc_bracket_calc, section, scenario = scenario)]
}

extracted_bu_te  <- mapply(
  extract_te_nace_fun,
  PIT_BU_list_pd_te, scenarios_pit_te,
  SIMPLIFY = FALSE
)
combined_bu_te  <- data.table::rbindlist(extracted_bu_te)
result_bu_te    <- combined_bu_te[
  ,
  .(total_pit_bu = sum(tax_on_tax_inc_bracket_calc, na.rm = TRUE)),
  by = .(scenario, section)
]

extracted_sim_te <- mapply(
  extract_te_nace_fun,
  PIT_SIM_list_pd_te, scenarios_pit_te,
  SIMPLIFY = FALSE
)
combined_sim_te  <- data.table::rbindlist(extracted_sim_te)
result_sim_te    <- combined_sim_te[
  ,
  .(total_pit_sim = sum(tax_on_tax_inc_bracket_calc, na.rm = TRUE)),
  by = .(scenario, section)
]

result_bu_te[,  year := forecast_horizon_te[match(scenario, scenarios_pit_te)]]
result_sim_te[, year := forecast_horizon_te[match(scenario, scenarios_pit_te)]]

nace_pit_summary_te <- merge(
  result_bu_te, result_sim_te,
  by  = c("scenario", "year", "section"),
  all = TRUE
)

nace_pit_summary_te[, tax_expenditure := (total_pit_sim - total_pit_bu) / 1e6]
nace_pit_summary_te <- nace_pit_summary_te[
  !is.na(section),
  .(year, section, tax_expenditure)
]

rm(
  extracted_bu_te,  extracted_sim_te,
  combined_bu_te,   combined_sim_te,
  result_bu_te,     result_sim_te
)


# Step 11 TAXPAYER TYPE BREAKDOWN ----

extract_te_taxpayer_fun <- function(dt, scenario) {
  dt[, .(tax_on_tax_inc_bracket_calc, TaxpayerType, scenario = scenario)]
}

extracted_bu_tp_te  <- mapply(
  extract_te_taxpayer_fun,
  PIT_BU_list_pd_te, scenarios_pit_te,
  SIMPLIFY = FALSE
)
combined_bu_tp_te  <- data.table::rbindlist(extracted_bu_tp_te)
result_bu_tp_te    <- combined_bu_tp_te[
  ,
  .(total_pit_bu = sum(tax_on_tax_inc_bracket_calc, na.rm = TRUE)),
  by = .(scenario, TaxpayerType)
]

extracted_sim_tp_te <- mapply(
  extract_te_taxpayer_fun,
  PIT_SIM_list_pd_te, scenarios_pit_te,
  SIMPLIFY = FALSE
)
combined_sim_tp_te  <- data.table::rbindlist(extracted_sim_tp_te)
result_sim_tp_te    <- combined_sim_tp_te[
  ,
  .(total_pit_sim = sum(tax_on_tax_inc_bracket_calc, na.rm = TRUE)),
  by = .(scenario, TaxpayerType)
]

result_bu_tp_te[,  year := forecast_horizon_te[match(scenario, scenarios_pit_te)]]
result_sim_tp_te[, year := forecast_horizon_te[match(scenario, scenarios_pit_te)]]

taxpayer_pit_summary_te <- merge(
  result_bu_tp_te, result_sim_tp_te,
  by  = c("scenario", "year", "TaxpayerType"),
  all = TRUE
)

taxpayer_pit_summary_te[, tax_expenditure := (total_pit_sim - total_pit_bu) / 1e6]
taxpayer_pit_summary_te <- taxpayer_pit_summary_te[
  !is.na(TaxpayerType),
  .(year, TaxpayerType, tax_expenditure)
]

rm(
  extracted_bu_tp_te,  extracted_sim_tp_te,
  combined_bu_tp_te,   combined_sim_tp_te,
  result_bu_tp_te,     result_sim_tp_te
)


# Step 12 SPLIT TE BY INCENTIVE TYPE ----

agg_annual_total_te <- function(lst) {
  data.table::rbindlist(
    lapply(lst, function(dt) {
      dt[, .(
        total_pit = sum(tax_on_tax_inc_bracket_calc, na.rm = TRUE),
        year      = first(simulation_data_year)
      )]
    })
  )
}

bu_annual_te          <- agg_annual_total_te(PIT_BU_list_pd_te)
sim_rent_annual_te    <- agg_annual_total_te(PIT_SIM_list_pd_te_rent)
sim_charity_annual_te <- agg_annual_total_te(PIT_SIM_list_pd_te_charity)

te_rent_split <- merge(
  bu_annual_te, sim_rent_annual_te,
  by       = "year",
  suffixes = c("_bu", "_sim")
)
te_rent_split[
  ,
  `:=`(
    te_lcu    = total_pit_sim - total_pit_bu,
    incentive = "Rent Deduction"
  )
]

te_charity_split <- merge(
  bu_annual_te, sim_charity_annual_te,
  by       = "year",
  suffixes = c("_bu", "_sim")
)
te_charity_split[
  ,
  `:=`(
    te_lcu    = total_pit_sim - total_pit_bu,
    incentive = "Charitable Deduction"
  )
]

te_split_combined <- data.table::rbindlist(
  list(
    te_rent_split[,    .(year, te_lcu, incentive)],
    te_charity_split[, .(year, te_lcu, incentive)]
  )
)

rm(
  bu_annual_te, sim_rent_annual_te, sim_charity_annual_te,
  te_rent_split, te_charity_split,
  PIT_SIM_list_pd_te_rent,
  PIT_SIM_list_pd_te_charity
)


# Step 13 RUNTIME ----

end.time.te           <- proc.time()
runtime_minutes_pd_te <- as.numeric(end.time.te[3L] - start.time.te[3L]) / 60


# Step 14 STORE ALL TE RESULTS ----

pd_pit_results_te <- list(
  pit_simulation_parameters_te = pit_simulation_parameters_te,
  PIT_BU_list_pd_te            = PIT_BU_list_pd_te,
  PIT_SIM_list_pd_te           = PIT_SIM_list_pd_te,
  summary_BU_pd_te             = summary_BU_te,
  summary_SIM_pd_te            = summary_SIM_te,
  merged_PIT_BU_SIM_pd_te      = merged_PIT_BU_SIM_pd_te,
  pit_summary_pd_te            = pit_summary_pd_te,
  nace_pit_summary_te          = nace_pit_summary_te,
  taxpayer_pit_summary_te      = taxpayer_pit_summary_te,
  te_split_combined            = te_split_combined,
  runtime_minutes_pd_te        = runtime_minutes_pd_te
)


# Step 15 CLEAN UP ----

rm(
  pit_simulation_parameters_te,
  pit_sim_params_te_rent,
  pit_sim_params_te_charity,
  PIT_BU_list_pd_te,
  PIT_SIM_list_pd_te,
  summary_BU_te,
  summary_SIM_te,
  merged_PIT_BU_SIM_pd_te,
  pit_summary_pd_te,
  nace_pit_summary_te,
  taxpayer_pit_summary_te,
  te_split_combined,
  scenario_years_pit_te,
  scenarios_pit_te,
  forecast_horizon_te,
  scenario_lookup_te,
  start_index_te,
  aggregate_value_columns_te,
  numeric_aggregate_columns_te,
  runtime_minutes_pd_te,
  start.time.te,
  end.time.te
)

gc(verbose = FALSE)


# Step 16 PRINT RESULTS ----

cat("=== PD Tax Expenditure Summary ===\n")
print(
  pd_pit_results_te$pit_summary_pd_te[
    ,
    .(
      year,
      `Current law (LCU Mil)`     = formatC(`Current law (LCU Mil)`,     digits = 3, format = "f"),
      `Benchmark (LCU Mil)`       = formatC(`Benchmark (LCU Mil)`,       digits = 3, format = "f"),
      `Tax Expenditure (LCU Mil)` = formatC(`Tax Expenditure (LCU Mil)`, digits = 3, format = "f")
    )
  ]
)

cat("\n=== Split TE by Incentive Type (LCU) ===\n")
print(pd_pit_results_te$te_split_combined)

cat("\n=== Split TE by Incentive Type (LCU Mil) ===\n")
print(
  pd_pit_results_te$te_split_combined[
    ,
    .(
      year,
      te_lcu_mil = round(te_lcu / 1e6, 4),
      incentive
    )
  ]
)

cat("\n=== PD Tax Expenditure by NACE Section ===\n")
print(pd_pit_results_te$nace_pit_summary_te)

cat("\n=== PD Tax Expenditure by TaxpayerType ===\n")
print(pd_pit_results_te$taxpayer_pit_summary_te)

cat(
  "\nPD TE module runtime in minutes:",
  round(pd_pit_results_te$runtime_minutes_pd_te, 2),
  "\n\n"
)


###

# Step1 Translate TaxpayerType in TE summary ----

dict_tp <- unique(
  data.table::as.data.table(dictionary_taxpayers_type),
  by = "TaxpayerType"
)

te_tp <- data.table::copy(
  data.table::as.data.table(pd_pit_results_te$taxpayer_pit_summary_te)
)

te_tp <- merge(
  te_tp,
  dict_tp,
  by    = "TaxpayerType",
  all.x = TRUE,
  sort  = FALSE
)

# Step2 Check unmatched values ----
# Values that are already English (on a rerun) are not reported.

unmatched_te <- unique(
  te_tp[is.na(TaxpayerType_en) &
          !TaxpayerType %in% dict_tp$TaxpayerType_en, TaxpayerType]
)

if (length(unmatched_te) > 0L) {
  warning("TE TaxpayerType values without English translation: ",
          paste(unmatched_te, collapse = ", "))
}

# Step3 Keep only English name (fallback to current value if no match) ----

te_tp[, TaxpayerType := data.table::fcoalesce(TaxpayerType_en, TaxpayerType)]
te_tp[, TaxpayerType_en := NULL]

# Step4 Aggregate duplicates created by translation ----

te_tp <- te_tp[
  ,
  .(tax_expenditure = sum(tax_expenditure, na.rm = TRUE)),
  by = .(year, TaxpayerType)
]

data.table::setorder(te_tp, year, TaxpayerType)

# Step5 Write back under the original name ----

pd_pit_results_te$taxpayer_pit_summary_te <- te_tp

# Step6 Cleanup ----

rm(dict_tp, te_tp, unmatched_te)
gc(verbose = FALSE)


