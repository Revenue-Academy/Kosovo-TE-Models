###############################################################################
# PIT WITHHOLDING-TAX MODULE

###############################################################################


#==============================================================================
# 0. CHECK SHARED FUNCTIONS
#==============================================================================

required_shared_functions_wr <- c(
  "get_param_fun",
  "validate_parameter_table",
  "get_growth_factor_row",
  "apply_growth_factors",
  "summarize_PIT_fun_dt"
)

available_shared_functions_wr <- vapply(
  required_shared_functions_wr,
  exists, logical(1), mode = "function", inherits = TRUE
)

missing_shared_functions_wr <- required_shared_functions_wr[!available_shared_functions_wr]

if (length(missing_shared_functions_wr) > 0L) {
  stop("Run the main PIT script first. Missing shared functions: ",
       paste(missing_shared_functions_wr, collapse = ", "))
}

if (!"package:data.table" %in% search()) {
  stop("The data.table package is not attached. Run the previous PIT script first.")
}


#==============================================================================
# INTEREST THRESHOLD FOR DISTRIBUTIONAL ANALYSIS

#==============================================================================

WR_INTEREST_THRESHOLD <- 1e6   # 1 Million LCU


#==============================================================================
# 1. WITHHOLDING-TAX MODULE
#==============================================================================

run_withholding_tax_module <- function(
    raw_data,
    growth_data,
    parameters_raw,
    parameters_updated,
    simulation_year,
    interest_threshold = WR_INTEREST_THRESHOLD
) {
  
  module_start_time_wr <- proc.time()
  
  #----------------------------------------------------------------------------
  # 1.1 Local copies
  #----------------------------------------------------------------------------
  raw_data           <- as.data.table(copy(raw_data))
  growth_data        <- as.data.table(copy(growth_data))
  parameters_raw     <- as.data.table(copy(parameters_raw))
  parameters_updated <- as.data.table(copy(parameters_updated))
  
  #----------------------------------------------------------------------------
  # 1.2 Legal person type codes
  #----------------------------------------------------------------------------
  legal_person_types_wr <- c(
    "SH.P.K.",
    "Shoq\u00ebri akcionare",
    "OJQ",
    "Kooperativa Bujq."
  )
  
  #----------------------------------------------------------------------------
  # 1.3 Validate growth-factor table
  #----------------------------------------------------------------------------
  required_growth_columns_wr <- c(
    "Year", "scenarios",
    "grossInterestPaid", "propertyRightsPaid", "grossLotteryWinnings",
    "grossRentPaid", "nonResidentGrossPayment", "nonResidentServicePayment",
    "nonBusinessIndividualPayments"
  )
  
  missing_growth_columns_wr <- setdiff(required_growth_columns_wr, names(growth_data))
  if (length(missing_growth_columns_wr) > 0L) {
    stop("pit_growth_factors_wr is missing columns: ",
         paste(missing_growth_columns_wr, collapse = ", "))
  }
  
  setorder(growth_data, Year)
  if (anyNA(growth_data$Year))              stop("pit_growth_factors_wr contains missing Year values.")
  if (anyNA(growth_data$scenarios))         stop("pit_growth_factors_wr contains missing scenario labels.")
  if (anyDuplicated(growth_data$Year))      stop("Each year must appear exactly once in pit_growth_factors_wr.")
  if (anyDuplicated(growth_data$scenarios)) stop("Each scenario must appear exactly once in pit_growth_factors_wr.")
  
  #----------------------------------------------------------------------------
  # 1.4 Dynamic years and scenarios
  #----------------------------------------------------------------------------
  scenario_years_wr   <- growth_data$Year
  scenarios_wr        <- growth_data$scenarios
  forecast_horizon_wr <- scenario_years_wr
  base_year_wr        <- min(scenario_years_wr)
  end_year_wr         <- max(scenario_years_wr)
  
  start_index_wr <- match(simulation_year, scenario_years_wr)
  if (is.na(start_index_wr)) {
    stop(sprintf("SimulationYear %s not in WR forecast horizon. Available: %s.",
                 simulation_year, paste(scenario_years_wr, collapse = ", ")))
  }
  
  scenario_lookup_wr <- data.table(scenarios = scenarios_wr, year = scenario_years_wr)
  
  #----------------------------------------------------------------------------
  # 1.5 Validate raw WR data
  #----------------------------------------------------------------------------
  required_raw_columns_wr <- c(
    "id_n", "Year", "TaxpayerType",
    "grossInterestPaid", "propertyRightsPaid", "grossLotteryWinnings",
    "totalMonthlyPayments", "standardWithholdingTax",
    "grossRentPaid", "rentWithholdingTax",
    "nonResidentGrossPayment", "nonResidentServicePayment", "nonResidentWithholdingTax",
    "totalTaxesWithheld",
    "nonBusinessIndividualPayments", "nonBusinessIndividualTax",
    "totalWithholdingTax"
  )
  
  missing_raw_columns_wr <- setdiff(required_raw_columns_wr, names(raw_data))
  if (length(missing_raw_columns_wr) > 0L) {
    stop("pit_raw_wr is missing columns: ", paste(missing_raw_columns_wr, collapse = ", "))
  }
  
  raw_years_wr <- unique(raw_data[!is.na(Year), Year])
  if (length(raw_years_wr) != 1L) {
    stop("pit_raw_wr must contain exactly one baseline year. Found: ",
         paste(raw_years_wr, collapse = ", "))
  }
  if (raw_years_wr[1L] != base_year_wr) {
    stop(sprintf("Baseline year in pit_raw_wr is %s but first growth-factor year is %s.",
                 raw_years_wr[1L], base_year_wr))
  }
  
  #----------------------------------------------------------------------------
  # 1.6 Variables receiving growth factors
  #----------------------------------------------------------------------------
  vars_to_grow_wr <- c(
    "grossInterestPaid", "propertyRightsPaid", "grossLotteryWinnings",
    "grossRentPaid", "nonResidentGrossPayment", "nonResidentServicePayment",
    "nonBusinessIndividualPayments"
  )
  
  missing_raw_growth_vars_wr <- setdiff(vars_to_grow_wr, names(raw_data))
  if (length(missing_raw_growth_vars_wr) > 0L) {
    stop("Missing WR growth variables in pit_raw_wr: ",
         paste(missing_raw_growth_vars_wr, collapse = ", "))
  }
  
  missing_growth_factor_vars_wr <- setdiff(vars_to_grow_wr, names(growth_data))
  if (length(missing_growth_factor_vars_wr) > 0L) {
    warning("No growth-factor column for: ",
            paste(missing_growth_factor_vars_wr, collapse = ", "),
            ". Factor of 1 applied.")
  }
  
  #----------------------------------------------------------------------------
  # 1.7 Validate policy parameters
  #----------------------------------------------------------------------------
  required_parameters_wr <- c(
    "wr_rate_expect_rent", "wr_rate_rent_res", "wr_rate_rent_non_res",
    "wr_rate_ind_physical_person", "wr_rate_ind_legal_person"
  )
  
  validate_parameter_table(
    params_dt           = parameters_raw,
    table_name          = "pit_simulation_parameters_raw",
    required_years      = scenario_years_wr,
    required_parameters = required_parameters_wr
  )
  
  updated_parameter_years_wr <- scenario_years_wr[
    seq.int(from = start_index_wr, to = length(scenario_years_wr))
  ]
  
  validate_parameter_table(
    params_dt           = parameters_updated,
    table_name          = "pit_simulation_parameters_updated",
    required_years      = updated_parameter_years_wr,
    required_parameters = required_parameters_wr
  )
  
  #----------------------------------------------------------------------------
  # 1.8 Withholding-tax calculation function
  #----------------------------------------------------------------------------
  
  withholding_tax_calc_fun <- function(dt_scn, params_dt, year,
                                       legal_types, interest_threshold) {
    
    wr_rate_expect_rent         <- get_param_fun(params_dt, "wr_rate_expect_rent",         year)
    wr_rate_rent_res            <- get_param_fun(params_dt, "wr_rate_rent_res",            year)
    wr_rate_rent_non_res        <- get_param_fun(params_dt, "wr_rate_rent_non_res",        year)
    wr_rate_ind_physical_person <- get_param_fun(params_dt, "wr_rate_ind_physical_person", year)
    wr_rate_ind_legal_person    <- get_param_fun(params_dt, "wr_rate_ind_legal_person",    year)
    
    withholding_rates <- c(wr_rate_expect_rent, wr_rate_rent_res, wr_rate_rent_non_res,
                           wr_rate_ind_physical_person, wr_rate_ind_legal_person)
    if (any(withholding_rates < 0 | withholding_rates > 1 | !is.finite(withholding_rates))) {
      stop(sprintf("Invalid withholding-tax rates for year %s.", year))
    }
    
    #==========================================================================
    # A. INTEREST, OWNERSHIP RIGHTS, LOTTERY (D8-D12) - FISCAL COLUMNS
    #==========================================================================
    dt_scn[, totalMonthlyPayments_calc :=
             fifelse(!is.na(grossInterestPaid)    & grossInterestPaid    > 0, grossInterestPaid,    0) +
             fifelse(!is.na(propertyRightsPaid)   & propertyRightsPaid   > 0, propertyRightsPaid,   0) +
             fifelse(!is.na(grossLotteryWinnings) & grossLotteryWinnings > 0, grossLotteryWinnings, 0)]
    
    dt_scn[, standardWithholdingTax_calc :=
             fifelse(
               totalMonthlyPayments_calc > 0 &
                 !is.na(standardWithholdingTax) & standardWithholdingTax > 0,
               totalMonthlyPayments_calc * wr_rate_expect_rent,
               0
             )]
    
    #==========================================================================
    # B. RENT AND NON-RESIDENT (D13-D18) - FISCAL COLUMNS
    #==========================================================================
    dt_scn[, rentWithholdingTax_res_calc :=
             fifelse(
               !is.na(grossRentPaid) & grossRentPaid > 0 &
                 !is.na(rentWithholdingTax) & rentWithholdingTax > 0,
               grossRentPaid * wr_rate_rent_res,
               0
             )]
    
    dt_scn[, nonResidentBase_calc :=
             fifelse(!is.na(nonResidentGrossPayment)   & nonResidentGrossPayment   > 0, nonResidentGrossPayment,   0) +
             fifelse(!is.na(nonResidentServicePayment) & nonResidentServicePayment > 0, nonResidentServicePayment, 0)]
    
    dt_scn[, rentWithholdingTax_non_res_calc :=
             fifelse(
               nonResidentBase_calc > 0 &
                 !is.na(nonResidentWithholdingTax) & nonResidentWithholdingTax > 0,
               nonResidentBase_calc * wr_rate_rent_non_res,
               0
             )]
    
    dt_scn[, totalTaxesWithheld_calc :=
             standardWithholdingTax_calc + rentWithholdingTax_res_calc + rentWithholdingTax_non_res_calc]
    
    #==========================================================================
    # C. NON-BUSINESS INDIVIDUAL PAYMENTS (D19-D20) - FISCAL COLUMNS
    #==========================================================================
    dt_scn[, nonBusinessIndividualTax_calc :=
             fifelse(
               !is.na(nonBusinessIndividualPayments) & nonBusinessIndividualPayments > 0 &
                 !is.na(nonBusinessIndividualTax)      & nonBusinessIndividualTax      > 0,
               fifelse(
                 TaxpayerType %in% legal_types,
                 nonBusinessIndividualPayments * wr_rate_ind_legal_person,
                 nonBusinessIndividualPayments * wr_rate_ind_physical_person
               ),
               0
             )]
    
    #==========================================================================
    # D. TOTAL WITHHOLDING TAX (D21) - FISCAL REVENUE COLUMN
    #==========================================================================
    dt_scn[, totalWithholdingTax_calc :=
             totalTaxesWithheld_calc + nonBusinessIndividualTax_calc]
    
    #==========================================================================
    # E. DISTRIBUTIONAL COLUMNS

    #==========================================================================
    
    dt_scn[, is_high_interest_filer := !is.na(grossInterestPaid) &
             grossInterestPaid > interest_threshold]
    
    dt_scn[, income_distribution_calc :=
             fifelse(is_high_interest_filer, 0, fcoalesce(totalMonthlyPayments_calc, 0)) +
             fcoalesce(grossRentPaid,                 0) +
             fcoalesce(nonResidentGrossPayment,       0) +
             fcoalesce(nonResidentServicePayment,     0) +
             fcoalesce(nonBusinessIndividualPayments, 0)]
    
    dt_scn[, pit_distribution_calc :=
             fifelse(is_high_interest_filer, 0, standardWithholdingTax_calc) +
             rentWithholdingTax_res_calc     +
             rentWithholdingTax_non_res_calc +
             nonBusinessIndividualTax_calc]
    
    dt_scn[, is_high_interest_filer := NULL]
    
    invisible(dt_scn)
  }
  
  
  #============================================================================
  # 2. BUSINESS-AS-USUAL PATH
  #============================================================================
  PIT_BU_list_wr <- list()
  dt_scn_BU_wr   <- copy(raw_data)
  
  for (i in seq_along(scenarios_wr)) {
    scenario_name <- scenarios_wr[i]
    scenario_year <- scenario_years_wr[i]
    
    growth_values <- get_growth_factor_row(scenario_name, growth_data, vars_to_grow_wr)
    apply_growth_factors(dt = dt_scn_BU_wr, growth_values = growth_values)
    withholding_tax_calc_fun(dt_scn_BU_wr, parameters_raw, scenario_year,
                             legal_person_types_wr, interest_threshold)
    dt_scn_BU_wr[, `:=`(scenario = scenario_name, simulation_data_year = scenario_year)]
    PIT_BU_list_wr[[scenario_name]] <- copy(dt_scn_BU_wr)
  }
  
  
  #============================================================================
  # 3. SIMULATION PATH
  #============================================================================
  PIT_SIM_list_wr <- list()
  
  if (start_index_wr > 1L) {
    for (i in seq_len(start_index_wr - 1L)) {
      scenario_name <- scenarios_wr[i]
      PIT_SIM_list_wr[[scenario_name]] <- copy(PIT_BU_list_wr[[scenario_name]])
    }
  }
  
  if (start_index_wr == 1L) {
    dt_scn_SIM_wr <- copy(raw_data)
  } else {
    dt_scn_SIM_wr <- copy(PIT_BU_list_wr[[scenarios_wr[start_index_wr - 1L]]])
  }
  
  for (i in seq.int(from = start_index_wr, to = length(scenarios_wr))) {
    scenario_name <- scenarios_wr[i]
    scenario_year <- scenario_years_wr[i]
    
    growth_values <- get_growth_factor_row(scenario_name, growth_data, vars_to_grow_wr)
    apply_growth_factors(dt = dt_scn_SIM_wr, growth_values = growth_values)
    withholding_tax_calc_fun(dt_scn_SIM_wr, parameters_updated, scenario_year,
                             legal_person_types_wr, interest_threshold)
    dt_scn_SIM_wr[, `:=`(scenario = scenario_name, simulation_data_year = scenario_year)]
    PIT_SIM_list_wr[[scenario_name]] <- copy(dt_scn_SIM_wr)
  }
  
  
  #============================================================================
  # 4. AGGREGATE BU AND SIMULATION RESULTS
  #============================================================================
  summary_BU_wr  <- summarize_PIT_fun_dt(PIT_list = PIT_BU_list_wr,  suffix = "_bu")
  summary_SIM_wr <- summarize_PIT_fun_dt(PIT_list = PIT_SIM_list_wr, suffix = "_sim")
  
  merged_PIT_BU_SIM_wr <- merge(summary_BU_wr, summary_SIM_wr, by = "scenarios", all = TRUE)
  merged_PIT_BU_SIM_wr <- merge(scenario_lookup_wr, merged_PIT_BU_SIM_wr,
                                by = "scenarios", all.x = TRUE, sort = FALSE)
  setorder(merged_PIT_BU_SIM_wr, year)
  setcolorder(merged_PIT_BU_SIM_wr,
              c("year", "scenarios", setdiff(names(merged_PIT_BU_SIM_wr), c("year", "scenarios"))))
  
  
  #============================================================================
  # 5. CONVERT AGGREGATES TO LCU MILLIONS
  #============================================================================
  aggregate_columns_wr <- setdiff(names(merged_PIT_BU_SIM_wr), c("year", "scenarios"))
  numeric_aggregate_columns_wr <- aggregate_columns_wr[
    vapply(merged_PIT_BU_SIM_wr[, ..aggregate_columns_wr], is.numeric, logical(1))
  ]
  merged_PIT_BU_SIM_wr[
    , (numeric_aggregate_columns_wr) := lapply(.SD, function(x) x / 1e6),
    .SDcols = numeric_aggregate_columns_wr
  ]
  
  
  #============================================================================
  # 6. MAIN FISCAL-IMPACT SUMMARY
  #============================================================================
  pit_summary_wr <- merged_PIT_BU_SIM_wr[, .(
    year,
    `Current law withholding tax (LCU Mil)`   = round(totalWithholdingTax_calc_bu,  1),
    `Simulation withholding tax (LCU Mil)`    = round(totalWithholdingTax_calc_sim, 1),
    `Fiscal impact withholding tax (LCU Mil)` = round(totalWithholdingTax_calc_sim - totalWithholdingTax_calc_bu, 1)
  )]
  
  
  #============================================================================
  # 7. COMPONENT SUMMARY
  #============================================================================
  wr_component_summary <- merged_PIT_BU_SIM_wr[, .(
    year,
    `Interest/rights/lottery - current law` = round(standardWithholdingTax_calc_bu,  1),
    `Interest/rights/lottery - simulation`  = round(standardWithholdingTax_calc_sim, 1),
    `Interest/rights/lottery - impact`      = round(standardWithholdingTax_calc_sim - standardWithholdingTax_calc_bu, 1),
    `Resident rent - current law`           = round(rentWithholdingTax_res_calc_bu,  1),
    `Resident rent - simulation`            = round(rentWithholdingTax_res_calc_sim, 1),
    `Resident rent - impact`                = round(rentWithholdingTax_res_calc_sim - rentWithholdingTax_res_calc_bu, 1),
    `Non-resident payments - current law`   = round(rentWithholdingTax_non_res_calc_bu,  1),
    `Non-resident payments - simulation`    = round(rentWithholdingTax_non_res_calc_sim, 1),
    `Non-resident payments - impact`        = round(rentWithholdingTax_non_res_calc_sim - rentWithholdingTax_non_res_calc_bu, 1),
    `Total taxes withheld - current law`    = round(totalTaxesWithheld_calc_bu,  1),
    `Total taxes withheld - simulation`     = round(totalTaxesWithheld_calc_sim, 1),
    `Total taxes withheld - impact`         = round(totalTaxesWithheld_calc_sim - totalTaxesWithheld_calc_bu, 1),
    `Non-business individuals - current law`= round(nonBusinessIndividualTax_calc_bu,  1),
    `Non-business individuals - simulation` = round(nonBusinessIndividualTax_calc_sim, 1),
    `Non-business individuals - impact`     = round(nonBusinessIndividualTax_calc_sim - nonBusinessIndividualTax_calc_bu, 1),
    `Total withholding tax - current law`   = round(totalWithholdingTax_calc_bu,  1),
    `Total withholding tax - simulation`    = round(totalWithholdingTax_calc_sim, 1),
    `Total withholding tax - fiscal impact` = round(totalWithholdingTax_calc_sim - totalWithholdingTax_calc_bu, 1)
  )]
  
  
  #============================================================================
  # 8. BASELINE VALIDATION
  #============================================================================
  baseline_dt_wr <- PIT_BU_list_wr[[scenarios_wr[1L]]]
  
  baseline_validation_wr <- baseline_dt_wr[, .(
    reported_D11         = sum(totalMonthlyPayments,        na.rm = TRUE) / 1e6,
    calc_D11             = sum(totalMonthlyPayments_calc,   na.rm = TRUE) / 1e6,
    reported_D12         = sum(standardWithholdingTax,      na.rm = TRUE) / 1e6,
    calc_D12             = sum(standardWithholdingTax_calc, na.rm = TRUE) / 1e6,
    reported_D13         = sum(grossRentPaid,               na.rm = TRUE) / 1e6,
    reported_D14         = sum(rentWithholdingTax,          na.rm = TRUE) / 1e6,
    calc_D14             = sum(rentWithholdingTax_res_calc, na.rm = TRUE) / 1e6,
    reported_D15D16_base = (sum(nonResidentGrossPayment,    na.rm = TRUE) +
                              sum(nonResidentServicePayment, na.rm = TRUE)) / 1e6,
    calc_D15D16_base     = sum(nonResidentBase_calc,              na.rm = TRUE) / 1e6,
    reported_D17         = sum(nonResidentWithholdingTax,         na.rm = TRUE) / 1e6,
    calc_D17             = sum(rentWithholdingTax_non_res_calc,   na.rm = TRUE) / 1e6,
    reported_D18         = sum(totalTaxesWithheld,                na.rm = TRUE) / 1e6,
    calc_D18             = sum(totalTaxesWithheld_calc,           na.rm = TRUE) / 1e6,
    reported_D19         = sum(nonBusinessIndividualPayments,     na.rm = TRUE) / 1e6,
    reported_D20         = sum(nonBusinessIndividualTax,          na.rm = TRUE) / 1e6,
    calc_D20             = sum(nonBusinessIndividualTax_calc,     na.rm = TRUE) / 1e6,
    reported_D21         = sum(totalWithholdingTax,               na.rm = TRUE) / 1e6,
    calc_D21             = sum(totalWithholdingTax_calc,          na.rm = TRUE) / 1e6,
    high_interest_filers_excluded      = sum(!is.na(grossInterestPaid) & grossInterestPaid > interest_threshold),
    high_interest_excluded_MIL         = sum(fifelse(!is.na(grossInterestPaid) & grossInterestPaid > interest_threshold,
                                                     grossInterestPaid, 0), na.rm = TRUE) / 1e6,
    individual_interest_included_MIL   = sum(fifelse(!is.na(grossInterestPaid) & grossInterestPaid <= interest_threshold,
                                                     grossInterestPaid, 0), na.rm = TRUE) / 1e6,
    income_distribution_total_MIL      = sum(income_distribution_calc, na.rm = TRUE) / 1e6,
    pit_distribution_total_MIL         = sum(pit_distribution_calc,    na.rm = TRUE) / 1e6,
    distribution_ETR_pct               = round(
      sum(pit_distribution_calc, na.rm = TRUE) /
        pmax(sum(income_distribution_calc, na.rm = TRUE), 1) * 100, 2)
  )]
  
  baseline_validation_wr[, `:=`(
    diff_D11    = calc_D11    - reported_D11,
    diff_D12    = calc_D12    - reported_D12,
    diff_D14    = calc_D14    - reported_D14,
    diff_D15D16 = calc_D15D16_base - reported_D15D16_base,
    diff_D17    = calc_D17    - reported_D17,
    diff_D18    = calc_D18    - reported_D18,
    diff_D20    = calc_D20    - reported_D20,
    diff_D21    = calc_D21    - reported_D21
  )]
  
  
  #============================================================================
  # 9. GROWTH-FACTOR CHECK
  #============================================================================
  growth_factor_check_wr <- rbindlist(lapply(scenarios_wr, function(sn) {
    gv <- get_growth_factor_row(sn, growth_data, vars_to_grow_wr)
    yr <- scenario_lookup_wr[scenarios == sn, year]
    data.table(scenarios = sn, year = yr,
               minimum_growth_factor = min(gv, na.rm = TRUE),
               maximum_growth_factor = max(gv, na.rm = TRUE),
               number_of_growth_variables = length(gv))
  }))
  
  
  #============================================================================
  # 10. RUNTIME
  #============================================================================
  module_end_time_wr <- proc.time()
  runtime_minutes_wr <- (module_end_time_wr[3L] - module_start_time_wr[3L]) / 60
  
  
  #============================================================================
  # 11. RETURN RESULTS
  #============================================================================
  return(list(
    scenario_years_wr        = scenario_years_wr,
    scenarios_wr             = scenarios_wr,
    forecast_horizon_wr      = forecast_horizon_wr,
    base_year_wr             = base_year_wr,
    end_year_wr              = end_year_wr,
    scenario_lookup_wr       = scenario_lookup_wr,
    PIT_BU_list_wr           = PIT_BU_list_wr,
    PIT_SIM_list_wr          = PIT_SIM_list_wr,
    summary_BU_wr            = summary_BU_wr,
    summary_SIM_wr           = summary_SIM_wr,
    merged_PIT_BU_SIM_wr     = merged_PIT_BU_SIM_wr,
    pit_summary_wr           = pit_summary_wr,
    wr_component_summary     = wr_component_summary,
    baseline_validation_wr   = baseline_validation_wr,
    growth_factor_check_wr   = growth_factor_check_wr,
    interest_threshold_wr    = interest_threshold,
    runtime_minutes_wr       = runtime_minutes_wr
  ))
}


#==============================================================================
# 2. RUN WITHHOLDING-TAX MODULE
#==============================================================================

wr_pit_results <- run_withholding_tax_module(
  raw_data           = pit_raw_wr,
  growth_data        = pit_growth_factors_wr,
  parameters_raw     = pit_simulation_parameters_raw,
  parameters_updated = pit_simulation_parameters_updated,
  simulation_year    = SimulationYear,
  interest_threshold = WR_INTEREST_THRESHOLD
)


#==============================================================================
# 3. KEEP ONLY THE SELECTED SIMULATION YEAR
#==============================================================================

selected_scenario_wr <- wr_pit_results$scenario_lookup_wr[
  year == as.integer(SimulationYear), scenarios
]

if (length(selected_scenario_wr) != 1L) {
  stop("Exactly one WR scenario must correspond to SimulationYear ", SimulationYear,
       ". Matches found: ", length(selected_scenario_wr))
}

wr_pit_results$selected_scenario_wr     <- selected_scenario_wr
wr_pit_results$PIT_BU_list_wr_selected  <- wr_pit_results$PIT_BU_list_wr[selected_scenario_wr]
wr_pit_results$PIT_SIM_list_wr_selected <- wr_pit_results$PIT_SIM_list_wr[selected_scenario_wr]


#==============================================================================
# 4. REMOVE COMPLETE t0-t5 LISTS
#==============================================================================

wr_pit_results$PIT_BU_list_wr  <- NULL
wr_pit_results$PIT_SIM_list_wr <- NULL

rm(selected_scenario_wr)
gc(verbose = FALSE)

message("Selected WR scenario retained: ", wr_pit_results$selected_scenario_wr,
        " / ", SimulationYear, ". Complete WR BU and SIM lists removed.")


#==============================================================================
# 5. VERIFY SELECTED WR LISTS
#==============================================================================

print(names(wr_pit_results$PIT_BU_list_wr_selected))
print(names(wr_pit_results$PIT_SIM_list_wr_selected))
print("PIT_BU_list_wr"  %in% names(wr_pit_results))
print("PIT_SIM_list_wr" %in% names(wr_pit_results))


#==============================================================================
# 6. PRINT RESULTS
#==============================================================================

cat("\n--- Scenario Lookup ---\n");     print(wr_pit_results$scenario_lookup_wr)
cat("\n--- Growth Factor Check ---\n"); print(wr_pit_results$growth_factor_check_wr)
cat("\n--- Merged BU/SIM ---\n");       print(wr_pit_results$merged_PIT_BU_SIM_wr)
cat("\n--- Fiscal Summary ---\n");      print(wr_pit_results$pit_summary_wr)
cat("\n--- Component Summary ---\n");   print(wr_pit_results$wr_component_summary)
cat("\n--- Baseline Validation ---\n"); print(wr_pit_results$baseline_validation_wr)

cat("\n--- Distributional Column Validation ---\n")
cat(sprintf("  Interest threshold (LCU)                   : %s\n",
            format(WR_INTEREST_THRESHOLD, big.mark = ",")))
cat(sprintf("  High-interest filers excluded from distribution: %s\n",
            wr_pit_results$baseline_validation_wr$high_interest_filers_excluded))
cat(sprintf("  Interest excluded from distribution (LCU Mil)  : %.3f\n",
            wr_pit_results$baseline_validation_wr$high_interest_excluded_MIL))
cat(sprintf("  Individual interest kept in distribution (LCU Mil): %.3f\n",
            wr_pit_results$baseline_validation_wr$individual_interest_included_MIL))
cat(sprintf("  income_distribution total (LCU Mil)        : %.3f\n",
            wr_pit_results$baseline_validation_wr$income_distribution_total_MIL))
cat(sprintf("  pit_distribution total (LCU Mil)           : %.3f\n",
            wr_pit_results$baseline_validation_wr$pit_distribution_total_MIL))
cat(sprintf("  Aggregate distributional ETR               : %.2f%%\n",
            wr_pit_results$baseline_validation_wr$distribution_ETR_pct))
cat("\nWR module runtime (minutes):", wr_pit_results$runtime_minutes_wr, "\n\n")
