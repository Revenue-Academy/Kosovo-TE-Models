
###############################################################################
# WM MODULE
###############################################################################


#==============================================================================
# Step1 CHECK INPUT ----
#==============================================================================

if (!exists("pit_raw_wm")) {
  stop(
    paste0(
      "pit_raw_wm not found in the environment. ",
      "Load the raw WM data before running this script."
    )
  )
}

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("The data.table package is required.")
}

if (!"package:data.table" %in% search()) {
  stop(
    paste0(
      "The data.table package is not attached. ",
      "Run the main PIT script first."
    )
  )
}

required_wm_cols <- c(
  "id_n",
  "gross_wage_w",
  "employee_cont",
  "net_income"
)

missing_wm_cols <- setdiff(required_wm_cols, names(pit_raw_wm))

if (length(missing_wm_cols) > 0L) {
  stop(
    paste0(
      "pit_raw_wm is missing required columns: ",
      paste(missing_wm_cols, collapse = ", ")
    )
  )
}


#==============================================================================
# Step2 CORRECTION PARAMETERS ----
#==============================================================================

# Statutory employee SSC rate used to back-calculate implied gross wage
ssc_rate_correction <- 0.05   # 5%

# Minimum gross wage (LCU) to trigger the SSC cross-check
# Records below this threshold are not reviewed - errors at low wages
# have negligible distributional impact
wage_threshold <- 50000

# Maximum allowed relative discrepancy between recorded and SSC-implied
# gross wage before the correction is applied
tolerance <- 0.10             # 10%

cat(
  sprintf(
    paste0(
      "\n=== WM Data Preparation: SSC Gross Wage Correction ===\n",
      "Parameters:\n",
      "  ssc_rate_correction : %.2f%%\n",
      "  wage_threshold      : %s LCU\n",
      "  tolerance           : %.0f%%\n\n"
    ),
    ssc_rate_correction * 100,
    format(wage_threshold, big.mark = ","),
    tolerance * 100
  )
)


#==============================================================================
# Step3 BUILD CORRECTED DATASET ----
#
# Work on a full copy - no TaxpayerType exclusions.
# All 29 taxpayer types are included in the correction check.
#==============================================================================

pit_raw_wm_corrected <- data.table::copy(
  data.table::as.data.table(pit_raw_wm)
)


#==============================================================================
# Step4 COMPUTE SSC-IMPLIED GROSS WAGE AND DISCREPANCY ----
#==============================================================================

pit_raw_wm_corrected[
  ,
  ssc_implied_gross := data.table::fcoalesce(employee_cont, 0) /
    ssc_rate_correction
]

pit_raw_wm_corrected[
  ,
  discrepancy_ratio := data.table::fifelse(
    gross_wage_w > 0,
    abs(gross_wage_w - ssc_implied_gross) / gross_wage_w,
    NA_real_
  )
]

pit_raw_wm_corrected[
  ,
  correction_flag := as.integer(
    gross_wage_w                        > wage_threshold &
      data.table::fcoalesce(employee_cont, 0) > 0              &
      discrepancy_ratio                   > tolerance
  )
]


#==============================================================================
# Step5 CORRECTION PREVIEW ----
#==============================================================================

correction_summary_pre <- pit_raw_wm_corrected[
  correction_flag == 1L,
  .(
    records_to_correct      = .N,
    recorded_gross_wage_MIL = sum(gross_wage_w,      na.rm = TRUE) / 1e6,
    implied_gross_wage_MIL  = sum(ssc_implied_gross, na.rm = TRUE) / 1e6,
    overstatement_MIL       = sum(
      gross_wage_w - ssc_implied_gross,
      na.rm = TRUE
    ) / 1e6,
    reported_pit_MIL        = sum(
      data.table::fcoalesce(tax_witheld, 0),
      na.rm = TRUE
    ) / 1e6,
    employee_cont_MIL       = sum(
      data.table::fcoalesce(employee_cont, 0),
      na.rm = TRUE
    ) / 1e6
  )
]

cat("--- SSC Gross Wage Correction Preview (Overall) ---\n")
print(correction_summary_pre)

correction_by_type_pre <- pit_raw_wm_corrected[
  correction_flag == 1L,
  .(
    records                 = .N,
    recorded_gross_wage_MIL = sum(gross_wage_w,      na.rm = TRUE) / 1e6,
    implied_gross_wage_MIL  = sum(ssc_implied_gross, na.rm = TRUE) / 1e6,
    overstatement_MIL       = sum(
      gross_wage_w - ssc_implied_gross,
      na.rm = TRUE
    ) / 1e6
  ),
  by = TaxpayerType
][order(-overstatement_MIL)]

cat("\n--- SSC Gross Wage Correction Preview (by TaxpayerType) ---\n")
print(correction_by_type_pre)


#==============================================================================
# Step6 PRESERVE ORIGINALS FOR AUDIT TRAIL ----
#==============================================================================

pit_raw_wm_corrected[
  correction_flag == 1L,
  `:=`(
    gross_wage_w_original = gross_wage_w,
    net_income_original   = net_income
  )
]


#==============================================================================
# Step7 APPLY CORRECTION ----
#
# gross_wage_w  <- ssc_implied_gross
# net_income    <- implied_gross - employee_cont - tax_witheld
#
# net_income is recalculated to maintain internal consistency:
#   net = gross - employee_ssc - pit_withheld
#==============================================================================

pit_raw_wm_corrected[
  correction_flag == 1L,
  `:=`(
    gross_wage_w = ssc_implied_gross,
    net_income   = ssc_implied_gross -
      data.table::fcoalesce(employee_cont, 0) -
      data.table::fcoalesce(tax_witheld,   0)
  )
]

cat(
  sprintf(
    paste0(
      "\n--- SSC Gross Wage Correction Applied ---\n",
      "  Records corrected        : %s\n",
      "  Gross wage reduced (MIL) : %.3f\n\n"
    ),
    format(
      pit_raw_wm_corrected[correction_flag == 1L, .N],
      big.mark = ","
    ),
    round(
      pit_raw_wm_corrected[
        correction_flag == 1L,
        sum(gross_wage_w_original - gross_wage_w, na.rm = TRUE) / 1e6
      ],
      3
    )
  )
)


#==============================================================================
# Step8 REMOVE HELPER COLUMNS ----
#
# ssc_implied_gross and discrepancy_ratio are internal to this script.
# correction_flag is retained so the WM calculator can report how many
# records were corrected in its audit output.
#==============================================================================

pit_raw_wm_corrected[
  ,
  `:=`(
    ssc_implied_gross = NULL,
    discrepancy_ratio = NULL
  )
]


#==============================================================================
# Step9 VERIFY OUTPUT ----
#==============================================================================

cat("--- pit_raw_wm_corrected: summary ---\n")
cat(
  sprintf(
    "  Total records    : %s\n",
    format(nrow(pit_raw_wm_corrected), big.mark = ",")
  )
)
cat(
  sprintf(
    "  Records corrected: %s (%.2f%% of total)\n",
    format(
      pit_raw_wm_corrected[correction_flag == 1L, .N],
      big.mark = ","
    ),
    100 * pit_raw_wm_corrected[correction_flag == 1L, .N] /
      nrow(pit_raw_wm_corrected)
  )
)
cat(
  sprintf(
    "  Gross wage total (MIL) - raw      : %.2f\n",
    sum(pit_raw_wm$gross_wage_w, na.rm = TRUE) / 1e6
  )
)
cat(
  sprintf(
    "  Gross wage total (MIL) - corrected: %.2f\n\n",
    sum(pit_raw_wm_corrected$gross_wage_w, na.rm = TRUE) / 1e6
  )
)

cat(
  "pit_raw_wm_corrected is ready. Run TaxCalculator_v1_WM_v1.R next.\n\n"
)



###############################################################################
# WAGE PIT MODULE - FINAL VERSION
#
# RUN THIS SCRIPT AFTER:
#   (1) The main PD PIT script  (shared functions must exist)
#   (2) Prepare_WM_Data.R       (pit_raw_wm_corrected must exist)
#
# Reuses these functions from the first script:
#   get_param_fun()
#   validate_parameter_table()
#   get_growth_factor_row()
#   apply_growth_factors()
#   summarize_PIT_fun_dt()
#
# No weights are used.
#
# KEY METHODOLOGICAL DECISIONS:
#
#   (1) NO TaxpayerType exclusions - all 29 taxpayer types are included.
#
#   (2) SSC GROSS WAGE CORRECTION (applied in Prepare_WM_Data.R BEFORE
#       this script runs):
#       For records where gross_wage_w > 50,000 AND employee_cont > 0
#       AND |recorded - SSC-implied| / recorded > 10%, the gross wage
#       is replaced with the SSC-implied gross wage (employee_cont / 0.05).
#       Records with employee_cont == 0 are NOT corrected - these are
#       genuinely exempt employees (diplomats, international staff, etc.).
#       Original values are preserved in gross_wage_w_original and
#       net_income_original for audit purposes.
#       The corrected dataset is pit_raw_wm_corrected, produced by
#       Prepare_WM_Data.R.
#
#   (3) SSC POLICY CALCULATION - CONDITIONED ON REPORTED CONTRIBUTION:
#       employee_cont_policy_calc = 0  when reported employee_cont == 0
#       employee_cont_policy_calc = gross_wage_w * ssc_rate  otherwise
#       This prevents the model from deducting a fictional SSC contribution
#       for exempt employees, which would artificially reduce their taxable
#       base and distort calculated PIT.
#
#   (4) PIT ELIGIBILITY TOGGLE:
#       toggle_exp_wages == 0  ->  calculate PIT for all records
#       toggle_exp_wages == 1  ->  calculate PIT only where tax_witheld > 0
###############################################################################


#==============================================================================
# 0. CHECK SHARED FUNCTIONS AND INPUT DATA
#==============================================================================

required_shared_functions <- c(
  "get_param_fun",
  "validate_parameter_table",
  "get_growth_factor_row",
  "apply_growth_factors",
  "summarize_PIT_fun_dt"
)

available_shared_functions <- vapply(
  required_shared_functions,
  exists,
  logical(1),
  mode     = "function",
  inherits = TRUE
)

missing_shared_functions <- required_shared_functions[
  !available_shared_functions
]

if (length(missing_shared_functions) > 0L) {
  stop(
    paste0(
      "Run the main PD PIT script first. ",
      "The following shared functions are missing: ",
      paste(missing_shared_functions, collapse = ", ")
    )
  )
}

# pit_raw_wm_corrected must have been produced by Prepare_WM_Data.R
if (!exists("pit_raw_wm_corrected")) {
  stop(
    paste0(
      "pit_raw_wm_corrected not found. ",
      "Run Prepare_WM_Data.R before this script."
    )
  )
}

# Verify the correction audit objects are available
if (!exists("correction_summary_pre")) {
  warning(
    paste0(
      "correction_summary_pre not found. ",
      "Prepare_WM_Data.R may not have run fully. ",
      "Correction audit will be unavailable in wm_pit_results."
    )
  )
}

if (!exists("correction_by_type_pre")) {
  warning(
    paste0(
      "correction_by_type_pre not found. ",
      "Prepare_WM_Data.R may not have run fully. ",
      "Correction audit by TaxpayerType will be unavailable."
    )
  )
}


#==============================================================================
# 1. WAGE MODULE FUNCTION
#==============================================================================

run_wage_pit_module <- function(
    raw_data,
    growth_data,
    parameters_raw,
    parameters_updated,
    simulation_year
) {
  
  module_start_time <- proc.time()
  
  
  #----------------------------------------------------------------------------
  # 1.1 Prepare local copies
  #----------------------------------------------------------------------------
  
  raw_data    <- as.data.table(copy(raw_data))
  growth_data <- as.data.table(copy(growth_data))
  
  
  #----------------------------------------------------------------------------
  # 1.2 Validate growth-factor table
  #----------------------------------------------------------------------------
  
  required_growth_columns_wm <- c(
    "Year",
    "scenarios",
    "gross_wage_w",
    "employee_cont",
    "employer_cont",
    "tax_witheld"
  )
  
  missing_growth_columns_wm <- setdiff(
    required_growth_columns_wm,
    names(growth_data)
  )
  
  if (length(missing_growth_columns_wm) > 0L) {
    stop(
      "pit_growth_factors_wm is missing columns: ",
      paste(missing_growth_columns_wm, collapse = ", ")
    )
  }
  
  setorder(growth_data, Year)
  
  if (anyNA(growth_data$Year)) {
    stop("pit_growth_factors_wm contains missing Year values.")
  }
  
  if (anyNA(growth_data$scenarios)) {
    stop("pit_growth_factors_wm contains missing scenario labels.")
  }
  
  if (anyDuplicated(growth_data$Year)) {
    stop("Each year must appear exactly once in pit_growth_factors_wm.")
  }
  
  if (anyDuplicated(growth_data$scenarios)) {
    stop("Each scenario must appear exactly once in pit_growth_factors_wm.")
  }
  
  
  #----------------------------------------------------------------------------
  # 1.3 Dynamic years and scenarios
  #----------------------------------------------------------------------------
  
  scenario_years_wm   <- growth_data$Year
  scenarios_wm        <- growth_data$scenarios
  forecast_horizon_wm <- scenario_years_wm
  base_year_wm        <- min(scenario_years_wm)
  end_year_wm         <- max(scenario_years_wm)
  
  start_index_wm <- match(simulation_year, scenario_years_wm)
  
  if (is.na(start_index_wm)) {
    stop(
      sprintf(
        paste0(
          "SimulationYear %s is not in the wage forecast horizon. ",
          "Available years: %s."
        ),
        simulation_year,
        paste(scenario_years_wm, collapse = ", ")
      )
    )
  }
  
  scenario_lookup_wm <- data.table(
    scenarios = scenarios_wm,
    year      = scenario_years_wm
  )
  
  
  #----------------------------------------------------------------------------
  # 1.4 Validate raw wage data
  #----------------------------------------------------------------------------
  
  required_raw_columns_wm <- c(
    "id_n",
    "Year",
    "gross_wage_w",
    "employee_cont",
    "employer_cont",
    "tax_witheld"
  )
  
  missing_raw_columns_wm <- setdiff(
    required_raw_columns_wm,
    names(raw_data)
  )
  
  if (length(missing_raw_columns_wm) > 0L) {
    stop(
      "pit_raw_wm_corrected is missing columns: ",
      paste(missing_raw_columns_wm, collapse = ", ")
    )
  }
  
  raw_years_wm <- unique(raw_data[!is.na(Year), Year])
  
  if (length(raw_years_wm) != 1L) {
    stop(
      paste0(
        "pit_raw_wm_corrected must contain exactly one baseline year. Years found: ",
        paste(raw_years_wm, collapse = ", ")
      )
    )
  }
  
  if (raw_years_wm[1L] != base_year_wm) {
    stop(
      sprintf(
        paste0(
          "The baseline year in pit_raw_wm_corrected is %s, ",
          "but the first wage growth-factor year is %s."
        ),
        raw_years_wm[1L],
        base_year_wm
      )
    )
  }
  
  
  #----------------------------------------------------------------------------
  # 1.5 Variables receiving growth factors
  #----------------------------------------------------------------------------
  
  vars_to_grow_wm <- c(
    "gross_wage_w",
    "employee_cont",
    "employer_cont",
    "tax_witheld"
  )
  
  missing_raw_growth_vars_wm <- setdiff(vars_to_grow_wm, names(raw_data))
  
  if (length(missing_raw_growth_vars_wm) > 0L) {
    stop(
      "Missing wage growth variables in pit_raw_wm_corrected: ",
      paste(missing_raw_growth_vars_wm, collapse = ", ")
    )
  }
  
  # net_income intentionally excluded - not part of the tax formula.
  
  
  #----------------------------------------------------------------------------
  # 1.6 Validate year-specific parameters
  #----------------------------------------------------------------------------
  
  required_parameters_wm <- c(
    "rate1",
    "rate2",
    "rate3",
    "rate4",
    "tbrk1",
    "tbrk2",
    "tbrk3",
    "toggle_exp_wages",
    "ssc_rate"
  )
  
  validate_parameter_table(
    params_dt           = parameters_raw,
    table_name          = "pit_simulation_parameters_raw",
    required_years      = scenario_years_wm,
    required_parameters = required_parameters_wm
  )
  
  updated_parameter_years_wm <- scenario_years_wm[
    seq.int(from = start_index_wm, to = length(scenario_years_wm))
  ]
  
  validate_parameter_table(
    params_dt           = parameters_updated,
    table_name          = "pit_simulation_parameters_updated",
    required_years      = updated_parameter_years_wm,
    required_parameters = required_parameters_wm
  )
  
  
  #----------------------------------------------------------------------------
  # 1.7 Wage-tax calculation (local to run_wage_pit_module)
  #----------------------------------------------------------------------------
  
  wage_tax_calc_fun <- function(dt_scn, params_dt, year) {
    
    rate1 <- get_param_fun(params_dt, "rate1", year)
    rate2 <- get_param_fun(params_dt, "rate2", year)
    rate3 <- get_param_fun(params_dt, "rate3", year)
    rate4 <- get_param_fun(params_dt, "rate4", year)
    
    tbrk1 <- get_param_fun(params_dt, "tbrk1", year)
    tbrk2 <- get_param_fun(params_dt, "tbrk2", year)
    tbrk3 <- get_param_fun(params_dt, "tbrk3", year)
    
    toggle_exp_wages <- get_param_fun(params_dt, "toggle_exp_wages", year)
    ssc_rate         <- get_param_fun(params_dt, "ssc_rate",         year)
    
    # Validate brackets
    if (tbrk1 < 0 || tbrk2 < tbrk1 || tbrk3 < tbrk2) {
      stop(
        sprintf(
          "Tax brackets are not ordered correctly for year %s.",
          year
        )
      )
    }
    
    # Validate SSC rate
    if (ssc_rate < 0 || ssc_rate > 1) {
      stop(
        sprintf(
          "ssc_rate must be between 0 and 1 for year %s.",
          year
        )
      )
    }
    
    #--------------------------------------------------------------------------
    # Employee SSC policy calculation
    #
    # If reported employee_cont == 0, the record is treated as exempt.
    # No SSC deduction is applied. This prevents a fictional deduction from
    # reducing the taxable base for diplomats, international staff, and other
    # exempt categories who legitimately have zero pension contributions.
    #
    # If reported employee_cont > 0, the statutory rate is applied to
    # gross_wage_w to produce the policy-consistent SSC amount.
    #--------------------------------------------------------------------------
    
    dt_scn[
      ,
      employee_cont_policy_calc := fifelse(
        fcoalesce(employee_cont, 0) == 0,
        0,
        fcoalesce(gross_wage_w, 0) * ssc_rate
      )
    ]
    
    #--------------------------------------------------------------------------
    # Taxable wage income
    # gross_wage_w minus policy SSC deduction
    #--------------------------------------------------------------------------
    
    dt_scn[
      ,
      inc_bef_tax_withheld_calc := pmax(
        fcoalesce(gross_wage_w, 0) - employee_cont_policy_calc,
        0
      )
    ]
    
    #--------------------------------------------------------------------------
    # Progressive wage PIT
    #
    # Four-band formula. For Kosovo 2025:
    #   rate1 = 0,    tbrk1 = 3,000
    #   rate2 = 0.08, tbrk2 = 5,400
    #   rate3 = 0.10, tbrk3 = 5,400  (zero-width band - no effect)
    #   rate4 = 0.10
    #
    # Applied when:
    #   toggle_exp_wages == 0  ->  all records
    #   toggle_exp_wages == 1  ->  only records with tax_witheld > 0
    #--------------------------------------------------------------------------
    
    dt_scn[
      ,
      pitax_w_calc := {
        
        tax_base <- fcoalesce(inc_bef_tax_withheld_calc, 0)
        
        calculated_tax <-
          rate1 * pmin(tax_base, tbrk1) +
          rate2 * pmin(tbrk2 - tbrk1, pmax(0, tax_base - tbrk1)) +
          rate3 * pmin(tbrk3 - tbrk2, pmax(0, tax_base - tbrk2)) +
          rate4 * pmax(0, tax_base - tbrk3)
        
        fifelse(
          toggle_exp_wages == 0 | fcoalesce(tax_witheld, 0) > 0,
          pmax(calculated_tax, 0),
          0
        )
      }
    ]
    
    #--------------------------------------------------------------------------
    # Validation differences
    #--------------------------------------------------------------------------
    
    dt_scn[
      ,
      employee_cont_difference_calc :=
        employee_cont_policy_calc - fcoalesce(employee_cont, 0)
    ]
    
    dt_scn[
      ,
      wage_tax_difference_calc :=
        pitax_w_calc - fcoalesce(tax_witheld, 0)
    ]
    
    invisible(dt_scn)
  }
  
  
  #============================================================================
  # 2. BUSINESS-AS-USUAL PATH
  #============================================================================
  
  PIT_BU_list_wm <- list()
  dt_scn_BU_wm   <- copy(raw_data)
  
  for (i in seq_along(scenarios_wm)) {
    
    scenario_name <- scenarios_wm[i]
    scenario_year <- scenario_years_wm[i]
    
    growth_values <- get_growth_factor_row(
      scenario_name = scenario_name,
      growth_dt     = growth_data,
      variables     = vars_to_grow_wm
    )
    
    apply_growth_factors(dt = dt_scn_BU_wm, growth_values = growth_values)
    
    wage_tax_calc_fun(
      dt_scn    = dt_scn_BU_wm,
      params_dt = parameters_raw,
      year      = scenario_year
    )
    
    dt_scn_BU_wm[
      ,
      `:=`(
        scenario             = scenario_name,
        simulation_data_year = scenario_year
      )
    ]
    
    PIT_BU_list_wm[[scenario_name]] <- copy(dt_scn_BU_wm)
  }
  
  
  #============================================================================
  # 3. SIMULATION PATH
  #============================================================================
  
  PIT_SIM_list_wm <- list()
  
  # Years before SimulationYear are identical to BU
  if (start_index_wm > 1L) {
    for (i in seq_len(start_index_wm - 1L)) {
      scenario_name <- scenarios_wm[i]
      PIT_SIM_list_wm[[scenario_name]] <- copy(
        PIT_BU_list_wm[[scenario_name]]
      )
    }
  }
  
  # Starting data for the simulation path
  if (start_index_wm == 1L) {
    dt_scn_SIM_wm <- copy(raw_data)
  } else {
    previous_scenario <- scenarios_wm[start_index_wm - 1L]
    dt_scn_SIM_wm    <- copy(PIT_BU_list_wm[[previous_scenario]])
  }
  
  # Updated policy from SimulationYear onward
  for (i in seq.int(from = start_index_wm, to = length(scenarios_wm))) {
    
    scenario_name <- scenarios_wm[i]
    scenario_year <- scenario_years_wm[i]
    
    growth_values <- get_growth_factor_row(
      scenario_name = scenario_name,
      growth_dt     = growth_data,
      variables     = vars_to_grow_wm
    )
    
    apply_growth_factors(dt = dt_scn_SIM_wm, growth_values = growth_values)
    
    wage_tax_calc_fun(
      dt_scn    = dt_scn_SIM_wm,
      params_dt = parameters_updated,
      year      = scenario_year
    )
    
    dt_scn_SIM_wm[
      ,
      `:=`(
        scenario             = scenario_name,
        simulation_data_year = scenario_year
      )
    ]
    
    PIT_SIM_list_wm[[scenario_name]] <- copy(dt_scn_SIM_wm)
  }
  
  
  #============================================================================
  # 4. AGGREGATION
  #============================================================================
  
  summary_BU_wm  <- summarize_PIT_fun_dt(
    PIT_list = PIT_BU_list_wm,
    suffix   = "_bu"
  )
  
  summary_SIM_wm <- summarize_PIT_fun_dt(
    PIT_list = PIT_SIM_list_wm,
    suffix   = "_sim"
  )
  
  merged_PIT_BU_SIM_wm <- merge(
    summary_BU_wm,
    summary_SIM_wm,
    by  = "scenarios",
    all = TRUE
  )
  
  merged_PIT_BU_SIM_wm <- merge(
    scenario_lookup_wm,
    merged_PIT_BU_SIM_wm,
    by    = "scenarios",
    all.x = TRUE,
    sort  = FALSE
  )
  
  setorder(merged_PIT_BU_SIM_wm, year)
  
  setcolorder(
    merged_PIT_BU_SIM_wm,
    c(
      "year",
      "scenarios",
      setdiff(
        names(merged_PIT_BU_SIM_wm),
        c("year", "scenarios")
      )
    )
  )
  
  # Convert to LCU millions
  aggregate_columns_wm <- setdiff(
    names(merged_PIT_BU_SIM_wm),
    c("year", "scenarios")
  )
  
  numeric_aggregate_columns_wm <- aggregate_columns_wm[
    vapply(
      merged_PIT_BU_SIM_wm[, ..aggregate_columns_wm],
      is.numeric,
      logical(1)
    )
  ]
  
  merged_PIT_BU_SIM_wm[
    ,
    (numeric_aggregate_columns_wm) :=
      lapply(.SD, function(v) v / 1e6),
    .SDcols = numeric_aggregate_columns_wm
  ]
  
  
  #============================================================================
  # 5. WAGE PIT SUMMARY
  #============================================================================
  
  pit_summary_wm <- merged_PIT_BU_SIM_wm[
    ,
    .(
      year,
      `Current law wage PIT (LCU Mil)`   = round(pitax_w_calc_bu,  1),
      `Simulation wage PIT (LCU Mil)`    = round(pitax_w_calc_sim, 1),
      `Fiscal impact wage PIT (LCU Mil)` = round(
        pitax_w_calc_sim - pitax_w_calc_bu,
        1
      )
    )
  ]
  
  
  #============================================================================
  # 6. EMPLOYEE SSC SUMMARY
  #============================================================================
  
  ssc_summary_wm <- merged_PIT_BU_SIM_wm[
    ,
    .(
      year,
      `Current law employee SSC (LCU Mil)`   = round(
        employee_cont_policy_calc_bu,
        1
      ),
      `Simulation employee SSC (LCU Mil)`    = round(
        employee_cont_policy_calc_sim,
        1
      ),
      `Fiscal impact employee SSC (LCU Mil)` = round(
        employee_cont_policy_calc_sim - employee_cont_policy_calc_bu,
        1
      )
    )
  ]
  
  
  #============================================================================
  # 7. BASELINE VALIDATION
  #============================================================================
  
  baseline_scenario_wm <- scenarios_wm[1L]
  baseline_dt_wm       <- PIT_BU_list_wm[[baseline_scenario_wm]]
  
  # Aggregate validation
  baseline_validation_wm <- baseline_dt_wm[
    ,
    .(
      reported_gross_wage_MIL               = sum(gross_wage_w,              na.rm = TRUE) / 1e6,
      reported_employee_contribution_MIL    = sum(fcoalesce(employee_cont, 0), na.rm = TRUE) / 1e6,
      calculated_employee_contribution_MIL  = sum(employee_cont_policy_calc, na.rm = TRUE) / 1e6,
      ssc_difference_MIL                    = (
        sum(employee_cont_policy_calc,     na.rm = TRUE) -
          sum(fcoalesce(employee_cont, 0),   na.rm = TRUE)
      ) / 1e6,
      reported_employer_contribution_MIL    = sum(fcoalesce(employer_cont, 0), na.rm = TRUE) / 1e6,
      reported_tax_withheld_MIL             = sum(fcoalesce(tax_witheld, 0),  na.rm = TRUE) / 1e6,
      calculated_wage_tax_MIL               = sum(pitax_w_calc,               na.rm = TRUE) / 1e6,
      wage_tax_difference_MIL               = (
        sum(pitax_w_calc,                  na.rm = TRUE) -
          sum(fcoalesce(tax_witheld, 0),     na.rm = TRUE)
      ) / 1e6
    )
  ]
  
  # Band-level breakdown
  baseline_validation_bands_wm <- baseline_dt_wm[
    pitax_w_calc > 0 | fcoalesce(tax_witheld, 0) > 0,
    .(
      records                        = .N,
      gross_wage_MIL                 = sum(gross_wage_w,                na.rm = TRUE) / 1e6,
      reported_ssc_MIL               = sum(fcoalesce(employee_cont, 0), na.rm = TRUE) / 1e6,
      calculated_ssc_MIL             = sum(employee_cont_policy_calc,   na.rm = TRUE) / 1e6,
      reported_pit_MIL               = sum(fcoalesce(tax_witheld, 0),   na.rm = TRUE) / 1e6,
      calculated_pit_MIL             = sum(pitax_w_calc,                na.rm = TRUE) / 1e6,
      discrepancy_MIL                = sum(wage_tax_difference_calc,    na.rm = TRUE) / 1e6,
      ETR_reported                   = sum(fcoalesce(tax_witheld, 0),   na.rm = TRUE) /
        pmax(sum(gross_wage_w,           na.rm = TRUE), 1),
      ETR_calculated                 = sum(pitax_w_calc,                na.rm = TRUE) /
        pmax(
          sum(inc_bef_tax_withheld_calc, na.rm = TRUE),
          1
        )
    ),
    by = .(
      band = fcase(
        inc_bef_tax_withheld_calc == 0,    "0_zero taxable base",
        inc_bef_tax_withheld_calc <= 3000, "1_zero rate (<=3,000)",
        inc_bef_tax_withheld_calc <= 5400, "2_eight pct (3,000-5,400)",
        default                           = "3_ten pct (>5,400)"
      )
    )
  ][order(band)]
  
  # Compliance gap by TaxpayerType - 10% band only
  compliance_gap_wm <- baseline_dt_wm[
    inc_bef_tax_withheld_calc > 5400,
    .(
      records                = .N,
      gross_wage_MIL         = sum(gross_wage_w,                na.rm = TRUE) / 1e6,
      reported_ssc_MIL       = sum(fcoalesce(employee_cont, 0), na.rm = TRUE) / 1e6,
      calculated_ssc_MIL     = sum(employee_cont_policy_calc,   na.rm = TRUE) / 1e6,
      ssc_difference_MIL     = (
        sum(employee_cont_policy_calc,   na.rm = TRUE) -
          sum(fcoalesce(employee_cont, 0), na.rm = TRUE)
      ) / 1e6,
      ssc_compliance_ratio   = sum(fcoalesce(employee_cont, 0), na.rm = TRUE) /
        pmax(
          sum(employee_cont_policy_calc, na.rm = TRUE),
          1
        ),
      reported_pit_MIL       = sum(fcoalesce(tax_witheld, 0),   na.rm = TRUE) / 1e6,
      calculated_pit_MIL     = sum(pitax_w_calc,                na.rm = TRUE) / 1e6,
      discrepancy_MIL        = sum(wage_tax_difference_calc,    na.rm = TRUE) / 1e6,
      pit_compliance_ratio   = sum(fcoalesce(tax_witheld, 0),   na.rm = TRUE) /
        pmax(sum(pitax_w_calc,           na.rm = TRUE), 1),
      ETR_reported           = sum(fcoalesce(tax_witheld, 0),   na.rm = TRUE) /
        pmax(sum(gross_wage_w,           na.rm = TRUE), 1),
      ETR_calculated         = sum(pitax_w_calc,                na.rm = TRUE) /
        pmax(
          sum(inc_bef_tax_withheld_calc, na.rm = TRUE),
          1
        ),
      ETR_difference         = (
        sum(pitax_w_calc,                na.rm = TRUE) /
          pmax(
            sum(inc_bef_tax_withheld_calc, na.rm = TRUE),
            1
          )
      ) - (
        sum(fcoalesce(tax_witheld, 0),   na.rm = TRUE) /
          pmax(sum(gross_wage_w,           na.rm = TRUE), 1)
      )
    ),
    by = TaxpayerType
  ][order(-discrepancy_MIL)]
  
  
  #============================================================================
  # 8. RUNTIME
  #============================================================================
  
  module_end_time    <- proc.time()
  runtime_minutes_wm <- (module_end_time[3L] - module_start_time[3L]) / 60
  
  
  #============================================================================
  # 9. RETURN ALL RESULTS
  #============================================================================
  
  return(
    list(
      scenario_years_wm            = scenario_years_wm,
      scenarios_wm                 = scenarios_wm,
      forecast_horizon_wm          = forecast_horizon_wm,
      base_year_wm                 = base_year_wm,
      end_year_wm                  = end_year_wm,
      scenario_lookup_wm           = scenario_lookup_wm,
      PIT_BU_list_wm               = PIT_BU_list_wm,
      PIT_SIM_list_wm              = PIT_SIM_list_wm,
      summary_BU_wm                = summary_BU_wm,
      summary_SIM_wm               = summary_SIM_wm,
      merged_PIT_BU_SIM_wm         = merged_PIT_BU_SIM_wm,
      pit_summary_wm               = pit_summary_wm,
      ssc_summary_wm               = ssc_summary_wm,
      baseline_validation_wm       = baseline_validation_wm,
      baseline_validation_bands_wm = baseline_validation_bands_wm,
      compliance_gap_wm            = compliance_gap_wm,
      runtime_minutes_wm           = runtime_minutes_wm
    )
  )
}


#==============================================================================
# 2. RUN THE WAGE MODULE ON CORRECTED FULL DATASET
#==============================================================================

wage_pit_results <- run_wage_pit_module(
  raw_data           = pit_raw_wm_corrected,
  growth_data        = pit_growth_factors_wm,
  parameters_raw     = pit_simulation_parameters_raw,
  parameters_updated = pit_simulation_parameters_updated,
  simulation_year    = SimulationYear
)


#==============================================================================
# 3. SELECT SIMULATION YEAR AND BUILD MAIN RESULTS LIST
#==============================================================================

selected_scenario_wm <- wage_pit_results$scenario_lookup_wm[
  year == as.integer(SimulationYear),
  scenarios
]

if (length(selected_scenario_wm) != 1L) {
  stop(
    paste0(
      "Exactly one WM scenario must correspond to SimulationYear ",
      SimulationYear,
      ". Matches found: ",
      length(selected_scenario_wm)
    )
  )
}

wm_pit_results <- list(
  
  selected_scenario_wm         = selected_scenario_wm,
  scenario_lookup_wm           = wage_pit_results$scenario_lookup_wm,
  
  PIT_BU_list_wm_selected      = wage_pit_results$PIT_BU_list_wm[
    selected_scenario_wm
  ],
  PIT_SIM_list_wm_selected     = wage_pit_results$PIT_SIM_list_wm[
    selected_scenario_wm
  ],
  
  pit_summary_wm               = wage_pit_results$pit_summary_wm,
  ssc_summary_wm               = wage_pit_results$ssc_summary_wm,
  baseline_validation_wm       = wage_pit_results$baseline_validation_wm,
  baseline_validation_bands_wm = wage_pit_results$baseline_validation_bands_wm,
  compliance_gap_wm            = wage_pit_results$compliance_gap_wm,
  runtime_minutes_wm           = wage_pit_results$runtime_minutes_wm,
  
  # Correction audit objects from Prepare_WM_Data.R
  correction_summary_pre       = if (exists("correction_summary_pre"))
    correction_summary_pre else NULL,
  correction_by_type_pre       = if (exists("correction_by_type_pre"))
    correction_by_type_pre else NULL,
  records_corrected            = if ("correction_flag" %in% names(pit_raw_wm_corrected))
    pit_raw_wm_corrected[correction_flag == 1L, .N] else NA_integer_
)

rm(wage_pit_results, selected_scenario_wm)
gc(verbose = FALSE)


#==============================================================================
# 4. PRINT RESULTS
#==============================================================================

cat("\n--- Scenario Lookup ---\n")
print(wm_pit_results$scenario_lookup_wm)

cat("\n--- Selected BU Scenario ---\n")
print(names(wm_pit_results$PIT_BU_list_wm_selected))

cat("\n--- Selected SIM Scenario ---\n")
print(names(wm_pit_results$PIT_SIM_list_wm_selected))

cat("\n--- Wage PIT Summary ---\n")
print(wm_pit_results$pit_summary_wm)

cat("\n--- Employee SSC Summary ---\n")
print(wm_pit_results$ssc_summary_wm)

cat("\n--- Baseline Validation (Aggregate) ---\n")
print(wm_pit_results$baseline_validation_wm)

cat("\n--- Baseline Validation (By Band) ---\n")
print(wm_pit_results$baseline_validation_bands_wm)

cat("\n--- Compliance Gap by TaxpayerType (10% band only) ---\n")
print(wm_pit_results$compliance_gap_wm)

cat("\n--- SSC Correction Audit: Overall ---\n")
print(wm_pit_results$correction_summary_pre)

cat("\n--- SSC Correction Audit: By TaxpayerType ---\n")
print(wm_pit_results$correction_by_type_pre)

cat(
  "\nRecords with gross wage corrected:",
  wm_pit_results$records_corrected,
  "\n"
)

cat(
  "\nWage module runtime in minutes:",
  wm_pit_results$runtime_minutes_wm,
  "\n\n"
)
