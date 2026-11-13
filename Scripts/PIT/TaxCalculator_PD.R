###############################################################################
# PIT MICROSIMULATION - PD MODULE
#
# FEATURES
#   - Dynamic years and scenarios from pit_growth_factors_pd
#   - Year-specific raw and updated policy parameters
#   - Cumulative annual growth factors
#   - No weights
#   - No gross_wage_w
#   - No tax_witheld
#   - No employee_cont
#   - No employer_cont
#   - Calculated totals are rebuilt after applying growth
#   - Withholding credits and advance payments fully computed [UPDATED]
#   - PD amount due used as fiscal output to avoid all double counting
#
# DOUBLE COUNTING CORRECTIONS APPLIED:
###############################################################################


# Step1 Packages and input data ----

library(data.table)

setDTthreads(threads = 8)

pit_raw_pd <- as.data.table(copy(pit_raw_pd))

pit_growth_factors_pd <- as.data.table(
  copy(pit_growth_factors_pd)
)

pit_simulation_parameters_raw <- as.data.table(
  copy(pit_simulation_parameters_raw)
)

start.time <- proc.time()


# Step2 Forecast years and scenarios ----

required_growth_columns <- c("Year", "scenarios")

missing_growth_columns <- setdiff(
  required_growth_columns,
  names(pit_growth_factors_pd)
)

if (length(missing_growth_columns) > 0L) {
  stop(
    "pit_growth_factors_pd is missing columns: ",
    paste(missing_growth_columns, collapse = ", ")
  )
}

setorder(pit_growth_factors_pd, Year)

if (anyNA(pit_growth_factors_pd$Year)) {
  stop("pit_growth_factors_pd contains missing Year values.")
}

if (anyNA(pit_growth_factors_pd$scenarios)) {
  stop("pit_growth_factors_pd contains missing scenario labels.")
}

if (anyDuplicated(pit_growth_factors_pd$Year)) {
  stop("Each year must appear exactly once in pit_growth_factors_pd.")
}

if (anyDuplicated(pit_growth_factors_pd$scenarios)) {
  stop("Each scenario must appear exactly once in pit_growth_factors_pd.")
}

scenario_years_pit <- pit_growth_factors_pd$Year
scenarios_pit      <- pit_growth_factors_pd$scenarios
forecast_horizon   <- scenario_years_pit
base_year          <- min(scenario_years_pit)
end_year           <- max(scenario_years_pit)
simulation_year    <- SimulationYear

start_index <- match(simulation_year, scenario_years_pit)

if (is.na(start_index)) {
  stop(
    sprintf(
      paste0(
        "SimulationYear %s is not included in the forecast horizon. ",
        "Available years are: %s."
      ),
      simulation_year,
      paste(scenario_years_pit, collapse = ", ")
    )
  )
}

scenario_lookup <- data.table(
  scenarios = scenarios_pit,
  year      = scenario_years_pit
)


# Step3 Validate baseline microdata ----

if (!"Year" %in% names(pit_raw_pd)) {
  stop("pit_raw_pd does not contain a Year column.")
}

raw_data_years <- unique(pit_raw_pd[!is.na(Year), Year])

if (length(raw_data_years) == 0L) {
  stop("pit_raw_pd contains no valid Year values.")
}

if (length(raw_data_years) > 1L) {
  stop("pit_raw_pd contains multiple years: ",
       paste(raw_data_years, collapse = ", "),
       ". Only one baseline year is allowed.")
}

if (raw_data_years[1L] != base_year) {
  message("PD data year (", raw_data_years[1L], ") differs from growth factor ",
          "start year (", base_year, "). ",
          "Overwriting PD Year with ", base_year, ".")
  pit_raw_pd[, Year := base_year]
}



# Step4 Parameter retrieval function ----

get_param_fun <- function(params_dt, param_name, year) {
  
  parameter_value <- params_dt[
    Parameters == param_name & Year == year,
    Value
  ]
  
  if (length(parameter_value) == 0L) {
    stop(sprintf("Parameter '%s' for year %s was not found.", param_name, year))
  }
  
  if (length(parameter_value) > 1L) {
    stop(sprintf("Parameter '%s' for year %s appears more than once.", param_name, year))
  }
  
  parameter_value <- parameter_value[1L]
  
  if (is.na(parameter_value) || !is.numeric(parameter_value) || !is.finite(parameter_value)) {
    stop(sprintf("Parameter '%s' for year %s has an invalid value.", param_name, year))
  }
  
  return(parameter_value)
}


# Step5 Validate parameter tables ----

required_parameters <- c(
  "rate1",
  "rate2",
  "rate3",
  "rate4",
  "tbrk1",
  "tbrk2",
  "tbrk3",
  "rate_ded_rent",
  "rate_ded_charitable"
)


validate_parameter_table <- function(
    params_dt,
    table_name,
    required_years,
    required_parameters
) {
  
  required_columns <- c("Parameters", "Year", "Value")
  
  missing_columns <- setdiff(required_columns, names(params_dt))
  
  if (length(missing_columns) > 0L) {
    stop(table_name, " is missing columns: ", paste(missing_columns, collapse = ", "))
  }
  
  required_grid <- CJ(
    Parameters = required_parameters,
    Year       = required_years,
    unique     = TRUE
  )
  
  available_grid <- unique(
    params_dt[
      Parameters %in% required_parameters & Year %in% required_years,
      .(Parameters, Year)
    ]
  )
  
  missing_parameter_years <- required_grid[!available_grid, on = .(Parameters, Year)]
  
  if (nrow(missing_parameter_years) > 0L) {
    missing_text <- paste(
      paste(missing_parameter_years$Parameters, missing_parameter_years$Year, sep = " / "),
      collapse = "\n"
    )
    stop(table_name, " is missing parameter-year combinations:\n", missing_text)
  }
  
  duplicate_parameter_years <- params_dt[
    Parameters %in% required_parameters & Year %in% required_years,
    .N,
    by = .(Parameters, Year)
  ][N > 1L]
  
  if (nrow(duplicate_parameter_years) > 0L) {
    duplicate_text <- paste(
      paste(duplicate_parameter_years$Parameters, duplicate_parameter_years$Year, sep = " / "),
      collapse = "\n"
    )
    stop(table_name, " contains duplicate parameter-year combinations:\n", duplicate_text)
  }
  
  invalid_parameter_values <- params_dt[
    Parameters %in% required_parameters &
      Year %in% required_years &
      (is.na(Value) | !is.finite(Value))
  ]
  
  if (nrow(invalid_parameter_values) > 0L) {
    stop(table_name, " contains missing or invalid parameter values.")
  }
  
  invisible(TRUE)
}


validate_parameter_table(
  params_dt           = pit_simulation_parameters_raw,
  table_name          = "pit_simulation_parameters_raw",
  required_years      = scenario_years_pit,
  required_parameters = required_parameters
)

updated_parameter_years <- scenario_years_pit[
  seq.int(from = start_index, to = length(scenario_years_pit))
]

validate_parameter_table(
  params_dt           = pit_simulation_parameters_updated,
  table_name          = "pit_simulation_parameters_updated",
  required_years      = updated_parameter_years,
  required_parameters = required_parameters
)


# Step6 Variables to grow ----

income_vars <- c(
  "gross_wage",
  "net_income_business",
  "net_income_partnership",
  "gross_rents",
  "gross_i_interest_pen_pay",
  "gross_i_interest",
  "gross_i_inta_prop",
  "capital_gain",
  "foreign_s_inc",
  "gross_i_lottery",
  "other_inc_gifts"
)

deduction_vars <- c(
  "ded_rents_expen_rents10pct",
  "ded_pen_cont",
  "ded_exp_int_prop",
  "other_allowed_ded",
  "loss_carried_for",
  "dis_charity_contribution"
)

withholding_credit_vars <- c(
  "tax_withheld_wage_employer",
  "tax_withheld_wage_interest",
  "tax_withheld_rents",
  "tax_witheld_lottery_gains",
  "foreign_tax_credit"
)

advance_payment_vars <- c(
  "q_advance_business_inc",
  "q_payment_ren_inc_int_prop"
)

vars_to_grow <- c(
  income_vars,
  deduction_vars,
  withholding_credit_vars,
  advance_payment_vars
)

missing_raw_variables <- setdiff(vars_to_grow, names(pit_raw_pd))

if (length(missing_raw_variables) > 0L) {
  stop(
    "The following growth variables are missing from pit_raw_pd: ",
    paste(missing_raw_variables, collapse = ", ")
  )
}

missing_growth_factor_columns <- setdiff(vars_to_grow, names(pit_growth_factors_pd))

if (length(missing_growth_factor_columns) > 0L) {
  warning(
    paste0(
      "The following variables have no growth-factor columns. ",
      "A growth factor of 1 will be used: ",
      paste(missing_growth_factor_columns, collapse = ", ")
    )
  )
}


# Step7 Growth-factor retrieval function ----

get_growth_factor_row <- function(
    scenario_name,
    growth_dt = pit_growth_factors_pd,
    variables = vars_to_grow
) {
  
  growth_row <- growth_dt[scenarios == scenario_name]
  
  if (nrow(growth_row) == 0L) {
    stop(sprintf("Scenario '%s' was not found in pit_growth_factors_pd.", scenario_name))
  }
  
  if (nrow(growth_row) > 1L) {
    stop(sprintf("Scenario '%s' appears more than once in pit_growth_factors_pd.", scenario_name))
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
      stop(
        sprintf(
          "Invalid growth factor for variable '%s' in scenario '%s'.",
          variable_name,
          scenario_name
        )
      )
    } else {
      growth_output[variable_name] <- factor_value
    }
  }
  
  return(growth_output)
}


# Step8 Apply growth factors ----

apply_growth_factors <- function(dt, growth_values) {
  
  variables_present <- intersect(names(growth_values), names(dt))
  
  if (length(variables_present) == 0L) {
    warning("No growth variables were found in the microdata.")
    return(invisible(dt))
  }
  
  numeric_variables <- variables_present[
    vapply(dt[, ..variables_present], is.numeric, logical(1))
  ]
  
  nonnumeric_variables <- setdiff(variables_present, numeric_variables)
  
  if (length(nonnumeric_variables) > 0L) {
    warning(
      "The following nonnumeric variables were not grown: ",
      paste(nonnumeric_variables, collapse = ", ")
    )
  }
  
  for (variable_name in numeric_variables) {
    dt[, (variable_name) := get(variable_name) * growth_values[[variable_name]]]
  }
  
  invisible(dt)
}


# Step9 Tax calculation function ----
#
# STRUCTURE OF PD FORM (Kosovo):
#
#   [8]-[18]  Gross income by source
#   [19]      Total income
#   [20]-[24] Deductions (rent, pension, intangible property, other)
#   [25]      Taxable amount = [19] - [24]
#   [26]-[28] Additional deductions (losses carried forward, charitable)
#   [29]      Taxable income before tax = [25] - [28]  (floor 0)
#   [30]      Bracket tax on [29]
#
#   --- WITHHOLDING CREDITS (Layer 1 deduction) ---
#   [31]      Wage withholding    - already in WM module
#   [32]      Interest withholding - already in WR module
#   [33]      Rent withholding     - already in WR module
#   [34]      Lottery withholding  - already in WR module
#   [35]      Foreign tax credit   - not in any other module
#   [36]      Total credits = sum([31] to [35])
#   [37]      Net tax payable = [30] - [36]  (floor 0)
#
#   --- ADVANCE PAYMENTS (Layer 2 deduction) ---
#   [38]      Quarterly advances on business income - already in IS module
#   [39]      Quarterly advances on rent / intangible property
#   [40]      Total advance payments = [38] + [39]
#
#   [41/42]   Amount due = [37] - [40]  (floor 0)
#             PRIMARY FISCAL OUTPUT - the only PD payment not
#             captured in any other module.

tax_calc_fun <- function(dt_scn, params_dt, year) {
  
  #----------------------------------------------------------------------------
  # 9.1 Year-specific parameters
  #----------------------------------------------------------------------------
  
  rate1 <- get_param_fun(params_dt, "rate1", year)
  rate2 <- get_param_fun(params_dt, "rate2", year)
  rate3 <- get_param_fun(params_dt, "rate3", year)
  rate4 <- get_param_fun(params_dt, "rate4", year)
  
  tbrk1 <- get_param_fun(params_dt, "tbrk1", year)
  tbrk2 <- get_param_fun(params_dt, "tbrk2", year)
  tbrk3 <- get_param_fun(params_dt, "tbrk3", year)
  
  rate_ded_rent       <- get_param_fun(params_dt, "rate_ded_rent",       year)
  rate_ded_charitable <- get_param_fun(params_dt, "rate_ded_charitable", year)
  
  if (tbrk1 < 0 || tbrk2 < tbrk1 || tbrk3 < tbrk2) {
    stop(sprintf("Tax brackets are not ordered correctly for year %s.", year))
  }
  
  
  #----------------------------------------------------------------------------
  # 9.2 Total income [19]
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    total_inc_calc := rowSums(.SD, na.rm = TRUE),
    .SDcols = income_vars
  ]
  
  
  #----------------------------------------------------------------------------
  # 9.3 Loss-limited income base for distributional ranking
  #
  # net_income_business can be negative (trading losses).
  # For income ranking purposes losses are floored at zero so they do
  # not drag a taxpayer's total income below zero and distort their rank.
  # This mirrors the total_inc_adjusted column already in pit_raw_pd.
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    total_inc_rank_calc :=
      total_inc_calc -
      fcoalesce(net_income_business, 0) +
      pmax(fcoalesce(net_income_business, 0), 0)
  ]
  
  
  #----------------------------------------------------------------------------
  # 9.4 Rental deduction [20]
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    rent_deduction_calc := pmax(
      fcoalesce(ded_rents_expen_rents10pct, 0),
      rate_ded_rent * fcoalesce(gross_rents, 0)
    )
  ]
  
  
  #----------------------------------------------------------------------------
  # 9.5 Total deductions [24]
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    total_ded_calc :=
      rent_deduction_calc +
      fcoalesce(ded_pen_cont,      0) +
      fcoalesce(ded_exp_int_prop,  0) +
      fcoalesce(other_allowed_ded, 0)
  ]
  
  
  #----------------------------------------------------------------------------
  # 9.6 Taxable amount [25]
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    taxable_amount_calc := total_inc_calc - total_ded_calc
  ]
  
  
  #----------------------------------------------------------------------------
  # 9.7 Allowable charitable contribution [27]
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    charity_contribution_calc := pmin(
      fcoalesce(dis_charity_contribution, 0),
      pmax(taxable_amount_calc * rate_ded_charitable, 0)
    )
  ]
  
  
  #----------------------------------------------------------------------------
  # 9.8 Total additional deductions [28]
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    tot_additional_ded_calc :=
      charity_contribution_calc +
      fcoalesce(loss_carried_for, 0)
  ]
  
  
  #----------------------------------------------------------------------------
  # 9.9 Taxable income before tax [29]
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    taxable_inc_before_tax_calc := pmax(
      taxable_amount_calc - tot_additional_ded_calc,
      0
    )
  ]
  
  
  #----------------------------------------------------------------------------
  # 9.10 Bracket tax [30]
  #
  # GROSS PIT liability before any credits or advance payments.
  # Retained for:
  #   - Baseline validation
  #   - TE script (Calc-TaxExpenditures_v1.R uses this column)
  # Do NOT use as the PD fiscal output in the aggregate.
  #----------------------------------------------------------------------------
  
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
  
  
  #----------------------------------------------------------------------------
  # 9.11 Total withholding credits [36]   --- LAYER 1 DEDUCTION ---
  #
  # Tax already paid at source and reported back on PD:
  #   tax_withheld_wage_employer : captured in WM module
  #   tax_withheld_wage_interest : captured in WR module
  #   tax_withheld_rents         : captured in WR module
  #   tax_witheld_lottery_gains  : captured in WR module
  #   foreign_tax_credit         : not in any other module
  #
  # Subtracting these avoids double counting WM and WR in the aggregate.
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    total_witheld_credit_calc :=
      fcoalesce(tax_withheld_wage_employer, 0) +
      fcoalesce(tax_withheld_wage_interest, 0) +
      fcoalesce(tax_withheld_rents,         0) +
      fcoalesce(tax_witheld_lottery_gains,  0) +
      fcoalesce(foreign_tax_credit,         0)
  ]
  
  
  #----------------------------------------------------------------------------
  # 9.12 Net tax payable after withholding credits [37]
  #
  # Intermediate result. Floor at zero: credits cannot exceed bracket tax.
  # Not used as fiscal output because advance payments still to be removed.
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    total_tax_payable_calc := pmax(
      tax_on_tax_inc_bracket_calc - total_witheld_credit_calc,
      0
    )
  ]
  
  
  #----------------------------------------------------------------------------
  # 9.13 Total advance payments [40]   --- LAYER 2 DEDUCTION ---
  #
  # Quarterly estimated tax payments made during the year:
  #   q_advance_business_inc     : captured in IS module (taxPaymentTotal)
  #   q_payment_ren_inc_int_prop : rent / intangible property advances
  #
  # Subtracting these avoids double counting IS quarterly payments
  # in the aggregate.
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    total_advance_payment_calc :=
      fcoalesce(q_advance_business_inc,     0) +
      fcoalesce(q_payment_ren_inc_int_prop, 0)
  ]
  
  
  #----------------------------------------------------------------------------
  # 9.14 Amount due [41/42]   --- PRIMARY FISCAL OUTPUT ---
  #
  # The residual tax still owed at filing time after both layers of
  # deduction. This is the ONLY PD payment not captured elsewhere.
  #
  # Formula:
  #   amount_due_calc = bracket_tax [30]
  #                   - withholding_credits [36]   (layer 1)
  #                   - advance_payments [40]       (layer 2)
  #
  # Floor at zero applied at record level (not aggregate) so that
  # overpayments by individual taxpayers do not offset other taxpayers.
  # This reproduces the reported amount_due exactly (validated).
  #----------------------------------------------------------------------------
  
  dt_scn[
    ,
    amount_due_calc := pmax(
      total_tax_payable_calc - total_advance_payment_calc,
      0
    )
  ]
  
  
  invisible(dt_scn)
}


# Step10 Business-as-usual path ----

PIT_BU_list_pd <- list()

dt_scn_BU <- copy(pit_raw_pd)

for (i in seq_along(scenarios_pit)) {
  
  scenario_name <- scenarios_pit[i]
  scenario_year <- scenario_years_pit[i]
  
  growth_values <- get_growth_factor_row(scenario_name = scenario_name)
  
  apply_growth_factors(dt = dt_scn_BU, growth_values = growth_values)
  
  tax_calc_fun(
    dt_scn    = dt_scn_BU,
    params_dt = pit_simulation_parameters_raw,
    year      = scenario_year
  )
  
  dt_scn_BU[
    ,
    `:=`(
      scenario             = scenario_name,
      simulation_data_year = scenario_year
    )
  ]
  
  PIT_BU_list_pd[[scenario_name]] <- copy(dt_scn_BU)
}


# Step11 Simulation path ----

PIT_SIM_list_pd <- list()

if (start_index > 1L) {
  for (i in seq_len(start_index - 1L)) {
    scenario_name <- scenarios_pit[i]
    PIT_SIM_list_pd[[scenario_name]] <- copy(PIT_BU_list_pd[[scenario_name]])
  }
}

if (start_index == 1L) {
  dt_scn_SIM <- copy(pit_raw_pd)
} else {
  previous_scenario <- scenarios_pit[start_index - 1L]
  dt_scn_SIM        <- copy(PIT_BU_list_pd[[previous_scenario]])
}

for (i in seq.int(from = start_index, to = length(scenarios_pit))) {
  
  scenario_name <- scenarios_pit[i]
  scenario_year <- scenario_years_pit[i]
  
  growth_values <- get_growth_factor_row(scenario_name = scenario_name)
  
  apply_growth_factors(dt = dt_scn_SIM, growth_values = growth_values)
  
  tax_calc_fun(
    dt_scn    = dt_scn_SIM,
    params_dt = pit_simulation_parameters_updated,
    year      = scenario_year
  )
  
  dt_scn_SIM[
    ,
    `:=`(
      scenario             = scenario_name,
      simulation_data_year = scenario_year
    )
  ]
  
  PIT_SIM_list_pd[[scenario_name]] <- copy(dt_scn_SIM)
}

message(
  paste0("PIT simulation completed for ", length(scenarios_pit), " scenarios.")
)

rm(dt_scn_BU, dt_scn_SIM)


# Step12 Aggregation function ----

summarize_PIT_fun_dt <- function(PIT_list, suffix) {
  
  if (length(PIT_list) == 0L) {
    stop("The supplied PIT scenario list is empty.")
  }
  
  summary_list <- lapply(
    names(PIT_list),
    function(scenario_name) {
      
      scenario_dt <- PIT_list[[scenario_name]]
      
      calculated_columns <- names(scenario_dt)[
        endsWith(names(scenario_dt), "_calc")
      ]
      
      if (length(calculated_columns) == 0L) {
        stop(sprintf("No _calc columns were found for scenario '%s'.", scenario_name))
      }
      
      numeric_calculated_columns <- calculated_columns[
        vapply(scenario_dt[, ..calculated_columns], is.numeric, logical(1))
      ]
      
      if (length(numeric_calculated_columns) == 0L) {
        stop(sprintf("No numeric _calc columns were found for scenario '%s'.", scenario_name))
      }
      
      scenario_summary <- scenario_dt[
        ,
        lapply(.SD, sum, na.rm = TRUE),
        .SDcols = numeric_calculated_columns
      ]
      
      scenario_summary[, scenarios := scenario_name]
      
      setcolorder(
        scenario_summary,
        c("scenarios", setdiff(names(scenario_summary), "scenarios"))
      )
      
      return(scenario_summary)
    }
  )
  
  result_dt <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)
  
  result_value_columns <- setdiff(names(result_dt), "scenarios")
  
  setnames(
    result_dt,
    result_value_columns,
    paste0(result_value_columns, suffix)
  )
  
  return(result_dt)
}


# Step13 Aggregate BU and simulation results ----

summary_BU  <- summarize_PIT_fun_dt(PIT_list = PIT_BU_list_pd,  suffix = "_bu")
summary_SIM <- summarize_PIT_fun_dt(PIT_list = PIT_SIM_list_pd, suffix = "_sim")

merged_PIT_BU_SIM_pd <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)

merged_PIT_BU_SIM_pd <- merge(
  scenario_lookup,
  merged_PIT_BU_SIM_pd,
  by    = "scenarios",
  all.x = TRUE,
  sort  = FALSE
)

setorder(merged_PIT_BU_SIM_pd, year)

setcolorder(
  merged_PIT_BU_SIM_pd,
  c("year", "scenarios", setdiff(names(merged_PIT_BU_SIM_pd), c("year", "scenarios")))
)


# Step14 Convert aggregates to LCU millions ----

aggregate_value_columns <- setdiff(names(merged_PIT_BU_SIM_pd), c("year", "scenarios"))

numeric_aggregate_columns <- aggregate_value_columns[
  vapply(merged_PIT_BU_SIM_pd[, ..aggregate_value_columns], is.numeric, logical(1))
]

merged_PIT_BU_SIM_pd[
  ,
  (numeric_aggregate_columns) := lapply(.SD, function(v) v / 1e6),
  .SDcols = numeric_aggregate_columns
]


# Step15 Main fiscal-impact summary ----
#
# PRIMARY OUTPUT: amount_due_calc [41/42]
#   Final amount owed at filing after both deduction layers.
#   This is the correct PD fiscal contribution to the aggregate because:
#     - Withholding credits [36] are already in WM and WR modules
#     - Advance payments [40] are already in the IS module
#   Using amount_due_calc ensures no double counting across all modules.
#
# RETAINED FOR REFERENCE:
#   tax_on_tax_inc_bracket_calc [30] - gross bracket tax
#     Used by TE script. Do not remove.
#   total_tax_payable_calc [37]      - net of withholding credits only
#     Intermediate step. Retained for validation.

required_summary_columns <- c(
  "amount_due_calc_bu",
  "amount_due_calc_sim",
  "total_tax_payable_calc_bu",
  "total_tax_payable_calc_sim",
  "tax_on_tax_inc_bracket_calc_bu",
  "tax_on_tax_inc_bracket_calc_sim"
)

missing_summary_columns <- setdiff(required_summary_columns, names(merged_PIT_BU_SIM_pd))

if (length(missing_summary_columns) > 0L) {
  stop(
    "The following summary columns are missing: ",
    paste(missing_summary_columns, collapse = ", ")
  )
}

pit_summary_pd <- merged_PIT_BU_SIM_pd[
  ,
  .(
    year,
    
    # PRIMARY: amount due [41/42] - no double counting
    `Current law (LCU Mil)`   = round(amount_due_calc_bu,  1),
    `Simulation (LCU Mil)`    = round(amount_due_calc_sim, 1),
    `Fiscal impact (LCU Mil)` = round(
      amount_due_calc_sim - amount_due_calc_bu,
      1
    ),
    
    # REFERENCE: net tax payable [37] - after withholding credits only
    `Net tax payable BU (LCU Mil)`  = round(total_tax_payable_calc_bu,  1),
    `Net tax payable SIM (LCU Mil)` = round(total_tax_payable_calc_sim, 1),
    
    # REFERENCE: gross bracket tax [30] - for validation and TE script
    `Gross bracket tax BU (LCU Mil)`  = round(tax_on_tax_inc_bracket_calc_bu,  1),
    `Gross bracket tax SIM (LCU Mil)` = round(tax_on_tax_inc_bracket_calc_sim, 1)
  )
]


# Step16 Baseline validation ----

baseline_scenario <- scenarios_pit[1L]
baseline_dt       <- PIT_BU_list_pd[[baseline_scenario]]

baseline_validation <- baseline_dt[
  ,
  .(
    reported_total_income =
      sum(total_inc,  na.rm = TRUE) / 1e6,
    calculated_total_income =
      sum(total_inc_calc, na.rm = TRUE) / 1e6,
    
    reported_total_deductions =
      sum(total_ded,      na.rm = TRUE) / 1e6,
    calculated_total_deductions =
      sum(total_ded_calc, na.rm = TRUE) / 1e6,
    
    reported_taxable_amount =
      sum(taxable_amount,      na.rm = TRUE) / 1e6,
    calculated_taxable_amount =
      sum(taxable_amount_calc, na.rm = TRUE) / 1e6,
    
    reported_taxable_income_before_tax =
      sum(taxable_inc_before_tax,      na.rm = TRUE) / 1e6,
    calculated_taxable_income_before_tax =
      sum(taxable_inc_before_tax_calc, na.rm = TRUE) / 1e6,
    
    reported_bracket_tax =
      sum(tax_on_tax_inc_bracket, na.rm = TRUE) / 1e6,
    calculated_bracket_tax =
      sum(tax_on_tax_inc_bracket_calc, na.rm = TRUE) / 1e6,
    
    reported_total_witheld_credit =
      sum(total_witheld_credit,      na.rm = TRUE) / 1e6,
    calculated_total_witheld_credit =
      sum(total_witheld_credit_calc, na.rm = TRUE) / 1e6,
    
    # NOTE: reported_total_tax_payable in raw data = amount_due (box 42)
    # not box 37. This is a naming issue in the source data.
    reported_total_tax_payable =
      sum(total_tax_payable,      na.rm = TRUE) / 1e6,
    calculated_total_tax_payable =
      sum(total_tax_payable_calc, na.rm = TRUE) / 1e6,
    
    reported_total_advance_payment =
      sum(total_advance_payment,      na.rm = TRUE) / 1e6,
    calculated_total_advance_payment =
      sum(total_advance_payment_calc, na.rm = TRUE) / 1e6,
    
    # amount_due is the correctly named reported column (box 42)
    reported_amount_due =
      sum(amount_due,      na.rm = TRUE) / 1e6,
    calculated_amount_due =
      sum(amount_due_calc, na.rm = TRUE) / 1e6
  )
]

baseline_validation[
  ,
  `:=`(
    income_difference =
      calculated_total_income - reported_total_income,
    deduction_difference =
      calculated_total_deductions - reported_total_deductions,
    taxable_amount_difference =
      calculated_taxable_amount - reported_taxable_amount,
    taxable_income_difference =
      calculated_taxable_income_before_tax - reported_taxable_income_before_tax,
    bracket_tax_difference =
      calculated_bracket_tax - reported_bracket_tax,
    witheld_credit_difference =
      calculated_total_witheld_credit - reported_total_witheld_credit,
    tax_payable_difference =
      calculated_total_tax_payable - reported_total_tax_payable,
    advance_payment_difference =
      calculated_total_advance_payment - reported_total_advance_payment,
    amount_due_difference =
      calculated_amount_due - reported_amount_due
  )
]


# Step17 Growth-factor check ----

growth_factor_check <- rbindlist(
  lapply(
    scenarios_pit,
    function(scenario_name) {
      scenario_growth_values <- get_growth_factor_row(scenario_name = scenario_name)
      scenario_year <- scenario_lookup[scenarios == scenario_name, year]
      data.table(
        scenarios                  = scenario_name,
        year                       = scenario_year,
        minimum_growth_factor      = min(scenario_growth_values, na.rm = TRUE),
        maximum_growth_factor      = max(scenario_growth_values, na.rm = TRUE),
        number_of_growth_variables = length(scenario_growth_values)
      )
    }
  )
)


# Step18 Calculate runtime ----

end.time <- proc.time()

runtime_minutes_pd <- as.numeric(end.time[3L] - start.time[3L]) / 60


# Step19 Identify the selected simulation scenario ----

selected_scenario_pd <- scenario_lookup[
  year == as.integer(SimulationYear),
  scenarios
]

if (length(selected_scenario_pd) != 1L) {
  stop(
    paste0(
      "Exactly one PD scenario must correspond to SimulationYear ",
      SimulationYear,
      ". Matches found: ",
      length(selected_scenario_pd)
    )
  )
}


# Step20 Create the main PD results list ----

pd_pit_results <- list(
  
  scenario_years_pd    = scenario_years_pit,
  scenarios_pd         = scenarios_pit,
  forecast_horizon_pd  = forecast_horizon,
  base_year_pd         = base_year,
  end_year_pd          = end_year,
  simulation_year_pd   = as.integer(SimulationYear),
  selected_scenario_pd = selected_scenario_pd,
  scenario_lookup_pd   = scenario_lookup,
  
  PIT_BU_list_pd_selected  = PIT_BU_list_pd[selected_scenario_pd],
  PIT_SIM_list_pd_selected = PIT_SIM_list_pd[selected_scenario_pd],
  
  summary_BU_pd        = summary_BU,
  summary_SIM_pd       = summary_SIM,
  merged_PIT_BU_SIM_pd = merged_PIT_BU_SIM_pd,
  pit_summary_pd       = pit_summary_pd,
  
  baseline_validation_pd = baseline_validation,
  growth_factor_check_pd = growth_factor_check,
  runtime_minutes_pd     = runtime_minutes_pd
)


# Step21 Remove standalone objects ----

rm(
  simulation_year,
  selected_scenario_pd,
  scenario_lookup,
  PIT_BU_list_pd,
  PIT_SIM_list_pd,
  summary_BU,
  summary_SIM,
  merged_PIT_BU_SIM_pd,
  pit_summary_pd,
  baseline_validation,
  growth_factor_check,
  runtime_minutes_pd,
  start.time,
  end.time
)

gc(verbose = FALSE)


# Step22 Print results ----

print(pd_pit_results$scenario_lookup_pd)
print(pd_pit_results$growth_factor_check_pd)
print(pd_pit_results$pit_summary_pd)
print(pd_pit_results$baseline_validation_pd)
print(names(pd_pit_results$PIT_BU_list_pd_selected))
print(names(pd_pit_results$PIT_SIM_list_pd_selected))

cat(
  "\nPD module runtime in minutes:",
  pd_pit_results$runtime_minutes_pd,
  "\n\n"
)

cat("==========================================================\n")
cat("  PD FISCAL OUTPUT SUMMARY\n")
cat("==========================================================\n")
cat("  Gross bracket tax [30]       : retained for TE script\n")
cat("  Less withholding credits [36]: WM + WR already captured\n")
cat("  Less advance payments [40]   : IS quarterly already captured\n")
cat("  = Amount due [42]            : PRIMARY FISCAL OUTPUT\n")
cat("==========================================================\n")
