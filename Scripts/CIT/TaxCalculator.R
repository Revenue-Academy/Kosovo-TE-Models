# TaxCalculator_v3.R
# Changes vs v2:
#   1. Insurance CIT restricted to name_en == 'Insurance company' only
#   2. ETR computed on calc_grossinc > 0 & calc_cit > 0 firms only
#   3. Decile/centile assigned on positive-income firms only (group 0 for loss firms)
#   All formula logic unchanged from v2 - mapping fix applied in data prep script.


# Helper functions --------------------------------------------------------

get_param_fun <- function(params_dt, param_name, year) {
  params_dt[Parameters == param_name & Year == year, Value]
}

summarize_CIT_fun <- function(CIT_list, suffix) {
  result <- data.frame(
    scenarios = character(),
    stringsAsFactors = FALSE
  )
  for (scenario in names(CIT_list)) {
    df <- CIT_list[[scenario]]
    calc_columns <- names(df)[grepl("^(calc|totax)", names(df))]
    calc_sums <- sapply(df[, ..calc_columns], function(col) sum(col, na.rm = TRUE))
    scenario_result <- data.frame(
      scenarios = scenario,
      as.list(calc_sums),
      stringsAsFactors = FALSE
    )
    result <- rbind(result, scenario_result)
  }
  names(result)[-1] <- paste0(names(result)[-1], suffix)
  return(result)
}

cal_weighted_deciles_fun <- function(x) {
  # FIX 3: assign decile groups only among firms with positive gross income.
  # Loss and zero-income firms assigned group 0.
  # Avoids quantile collapse near zero which produced empty deciles in v2.
  out     <- integer(length(x))
  pos_idx <- which(x > 0)
  if (length(pos_idx) < 2L) {
    out[pos_idx] <- 1L
    return(out)
  }
  breaks   <- unique(quantile(x[pos_idx], probs = seq(0, 1, by = 0.1), na.rm = TRUE))
  n_groups <- length(breaks) - 1L
  if (n_groups < 1L) {
    out[pos_idx] <- 1L
    return(out)
  }
  out[pos_idx] <- as.integer(
    cut(x[pos_idx], breaks = breaks, labels = 1:n_groups, include.lowest = TRUE)
  )
  out
}

cal_weighted_centiles_fun <- function(x) {
  # FIX 3: assign centile groups only among firms with positive gross income.
  # Loss and zero-income firms assigned group 0.
  out     <- integer(length(x))
  pos_idx <- which(x > 0)
  if (length(pos_idx) < 2L) {
    out[pos_idx] <- 1L
    return(out)
  }
  breaks   <- unique(quantile(x[pos_idx], probs = seq(0, 1, by = 0.01), na.rm = TRUE))
  n_groups <- length(breaks) - 1L
  if (n_groups < 1L) {
    out[pos_idx] <- 1L
    return(out)
  }
  out[pos_idx] <- as.integer(
    cut(x[pos_idx], breaks = breaks, labels = 1:n_groups, include.lowest = TRUE)
  )
  out
}


# Setup -------------------------------------------------------------------

growth_factors_cit  <- as.data.table(growth_factors_cit)
setorder(growth_factors_cit, Year)

scenario_years_cit  <- growth_factors_cit$Year
scenarios           <- growth_factors_cit$scenarios
forecast_horizon    <- scenario_years_cit
base_year           <- min(scenario_years_cit)
end_year            <- max(scenario_years_cit)

scenario_lookup_cit <- data.table(
  scenario = scenarios,
  year     = scenario_years_cit
)

SimulationYear <- SimulationYear

start_index <- match(SimulationYear, scenario_years_cit)

if (is.na(start_index)) {
  stop(sprintf(
    "SimulationYear %s is not in scenario_years_cit. Available: %s",
    SimulationYear,
    paste(scenario_years_cit, collapse = ", ")
  ))
}

cit_simulation_parameters_raw     <- as.data.table(cit_simulation_parameters_raw)
cit_simulation_parameters_updated <- as.data.table(cit_simulation_parameters_updated)

if (!"Year" %in% names(cit_simulation_parameters_raw))
  stop("cit_simulation_parameters_raw must have a 'Year' column.")
if (!"Year" %in% names(cit_simulation_parameters_updated))
  stop("cit_simulation_parameters_updated must have a 'Year' column.")


# 1. Tax Calculation Function ---------------------------------------------

start.time <- proc.time()

tax_calc_fun <- function(dt_scn, params_dt, year) {
  
  required_cols <- c(
    "forsourceincsch_a", "recbaddebts_sch_b", "capgain_sch_c", "div_sch_d",
    "otherincgain_sch_e", "profitloss_afteradjinc", "disallowedexp_sch_f",
    "repcosts_sch_g", "reservefunds_sch_h", "paymrelper_sch_i", "amort_sch_j",
    "dep_sch_k", "specallownewassets_sch_l", "caploss_sch_c", "otherexp_sch_m",
    "charitycontrib_box28", "grossprem", "forstattaxcredit_sch_o",
    "taxwithheld_sch_p", "sportspodisc_max30pct_box42",
    "culyouthspodisc_max20pct_box42", "name_en"
  )
  
  for (col in required_cols) {
    if (!col %in% names(dt_scn)) {
      if (col == "name_en") dt_scn[, (col) := ""]
      else                  dt_scn[, (col) := 0]
    }
  }
  
  cit_rate                <- get_param_fun(params_dt, "cit_rate",                year)
  cit_rate_ins            <- get_param_fun(params_dt, "cit_rate_ins",            year)
  rate_spl_all            <- get_param_fun(params_dt, "rate_spl_all",            year)
  rate_max_charitable_ded <- get_param_fun(params_dt, "rate_max_charitable_ded", year)
  Loss_CFLimit            <- get_param_fun(params_dt, "Loss_CFLimit",            year)
  rate_ded_culture_youth  <- get_param_fun(params_dt, "rate_ded_culture_youth",  year)
  rate_ded_sponsorship    <- get_param_fun(params_dt, "rate_ded_sponsorship",    year)
  
  if (length(cit_rate) == 0)                stop(sprintf("cit_rate not found for year %s",                year))
  if (length(cit_rate_ins) == 0)            stop(sprintf("cit_rate_ins not found for year %s",            year))
  if (length(rate_spl_all) == 0)            stop(sprintf("rate_spl_all not found for year %s",            year))
  if (length(rate_max_charitable_ded) == 0) stop(sprintf("rate_max_charitable_ded not found for year %s", year))
  if (length(Loss_CFLimit) == 0)            stop(sprintf("Loss_CFLimit not found for year %s",            year))
  if (length(rate_ded_culture_youth) == 0)  stop(sprintf("rate_ded_culture_youth not found for year %s",  year))
  if (length(rate_ded_sponsorship) == 0)    stop(sprintf("rate_ded_sponsorship not found for year %s",    year))
  
  # 1.[16] Total adjustment to income
  dt_scn[, calc_totadjinc_l11_l15 :=
           forsourceincsch_a + recbaddebts_sch_b + capgain_sch_c +
           div_sch_d + otherincgain_sch_e]
  
  # 2.[17] Profit (loss) after adjustment to income
  dt_scn[, calc_profitloss_afteradjinc := profitloss_afteradjinc]
  
  # 3.[27] Total adjustment to expenses
  dt_scn[, calc_totadjexp_l18_l26 :=
           disallowedexp_sch_f + repcosts_sch_g + reservefunds_sch_h +
           paymrelper_sch_i + amort_sch_j + dep_sch_k +
           (specallownewassets_sch_l * rate_spl_all) + caploss_sch_c + otherexp_sch_m]
  
  # 4.[28] Profit (loss) after adjustment to expenses
  dt_scn[, calc_grossinc := calc_profitloss_afteradjinc - calc_totadjexp_l18_l26]
  
  # 5.[29] Charitable contributions - capped at rate_max_charitable_ded of gross income
  dt_scn[, calc_charitycontrib_box28 :=
           pmin(charitycontrib_box28,
                pmax(rate_max_charitable_ded * calc_grossinc, 0))]
  
  dt_scn[, calc_gti := calc_grossinc - calc_charitycontrib_box28]
  
  # 6.[31] Loss carry-forward
  dt_scn[, `:=`(
    newloss1 = fifelse(calc_gti < 0, calc_gti, 0),
    calc_nti = fifelse(calc_gti < 0, 0, calc_gti)
  )]
  
  # 7.[33] Corporate income tax - non-insurance companies
  dt_scn[, calc_cit_non_insurance := pmax(calc_nti * cit_rate, 0)]
  
  # 8.[35] Insurance companies
  # FIX 1: restricted to actual insurance companies only.
  # Non-insurance firms had grossprem > 0 due to mapping error (now fixed
  # in data prep). Belt-and-suspenders check retained here.
  dt_scn[, calc_cit_insurance := pmax(
    fifelse(name_en == "Insurance company", grossprem * cit_rate_ins, 0),
    0
  )]
  
  dt_scn[, calc_cit_large := calc_cit_non_insurance + calc_cit_insurance]
  
  # 9.[39] Total credits
  dt_scn[, calc_totcredits_l37_l38 := forstattaxcredit_sch_o + taxwithheld_sch_p]
  
  # 10.[40] Line [36] less [39]
  dt_scn[, calc_l36_minus_l39 := pmax(0, calc_cit_large - calc_totcredits_l37_l38)]
  
  # [43] Sports sponsorship discount - capped at rate_ded_sponsorship of calc_cit_large
  dt_scn[, calc_sportspodisc_max30pct_box42 :=
           pmin(sportspodisc_max30pct_box42,
                rate_ded_sponsorship * calc_cit_large)]
  
  # [44] Culture and youth sponsorship discount - capped at rate_ded_culture_youth
  dt_scn[, calc_culyouthspodisc_max20pct_box42 :=
           pmin(culyouthspodisc_max20pct_box42,
                rate_ded_culture_youth * calc_cit_large)]
  
  # 11.[45] Total sponsorship discounts - overall cap at rate_ded_sponsorship of calc_cit_large
  dt_scn[, calc_totalspodisc_max30pct_box42 :=
           pmin(
             calc_sportspodisc_max30pct_box42 + calc_culyouthspodisc_max20pct_box42,
             rate_ded_sponsorship * calc_cit_large
           )]
  
  # 12. Final CIT
  dt_scn[, calc_cit := pmax(calc_cit_large - calc_totalspodisc_max30pct_box42, 0)]
}


# 2. Variables to grow ----------------------------------------------------

vars_to_grow <- c(
  "netprofit_fd",
  "forsourceincsch_a",
  "recbaddebts_sch_b",
  "capgain_sch_c",
  "div_sch_d",
  "otherincgain_sch_e",
  "totadjinc_l11_l15",
  "profitloss_afteradjinc",
  "disallowedexp_sch_f",
  "repcosts_sch_g",
  "reservefunds_sch_h",
  "paymrelper_sch_i",
  "amort_sch_j",
  "dep_sch_k",
  "specallownewassets_sch_l",
  "caploss_sch_c",
  "otherexp_sch_m",
  "totadjexp_l18_l26",
  "proflossaftadjexp_l17_l27",
  "charitycontrib_box28",
  "losscarryforward",
  "add_l29_l30",
  "adjprofit_l28_l31",
  "corpinctax_l32",
  "grossprem",
  "corpinctax_insurcomp",
  "corpinctax_total",
  "forstattaxcredit_sch_o",
  "taxwithheld_sch_p",
  "totcredits_l37_l38",
  "l36_minus_l39",
  "installmentspaid_sch_q",
  "taxobl_befdisc_box36",
  "sportspodisc_max30pct_box42",
  "culyouthspodisc_max20pct_box42",
  "totalspodisc_max30pct_box42",
  "taxpaywithform_l36_l39_l41_l45",
  "amountdue_box43",
  "taxpaid"
)

# Step 2.1 Diagnose missing columns in cit_raw ----
vars_missing_in_raw <- setdiff(vars_to_grow, names(cit_raw))
if (length(vars_missing_in_raw) > 0) {
  message("WARNING: The following vars_to_grow columns do NOT exist in cit_raw and will be skipped:")
  message(paste(" -", vars_missing_in_raw, collapse = "\n"))
}
vars_to_grow_existing <- vars_to_grow[vars_to_grow %in% names(cit_raw)]

# Step 2.2 Diagnose missing growth factor columns ----
gf_row_check <- growth_factors_cit[growth_factors_cit$scenarios == scenarios[1], ]

missing_gf <- vapply(vars_to_grow_existing, function(v) {
  gf_col <- sub("_adjusted", "", v)
  !gf_col %in% names(gf_row_check) || length(gf_row_check[[gf_col]]) == 0
}, logical(1))

if (any(missing_gf)) {
  message("WARNING: The following variables have no matching column in growth_factors_cit and will default to 1:")
  message(paste(" -", vars_to_grow_existing[missing_gf], collapse = "\n"))
}

# Step 2.3 Growth factor retrieval with safe fallback ----
get_growth_factor_row <- function(scenario) {
  gf_row <- growth_factors_cit[growth_factors_cit$scenarios == scenario, ]
  out    <- numeric(length(vars_to_grow_existing))
  names(out) <- vars_to_grow_existing
  for (v in vars_to_grow_existing) {
    gf_col <- sub("_adjusted", "", v)
    if (gf_col %in% names(gf_row) && length(gf_row[[gf_col]]) > 0) {
      out[v] <- gf_row[[gf_col]]
    } else {
      out[v] <- 1
    }
  }
  return(out)
}


# 3. Business as usual ----------------------------------------------------

CIT_BU_list <- list()
dt_cit_BU   <- copy(cit_raw)

for (i in seq_along(scenarios)) {
  s             <- scenarios[i]
  scenario_year <- scenario_years_cit[i]
  
  gf_values <- get_growth_factor_row(s)
  for (v in vars_to_grow_existing) {
    dt_cit_BU[, (v) := get(v) * gf_values[v]]
  }
  
  tax_calc_fun(dt_cit_BU, cit_simulation_parameters_raw, scenario_year)
  dt_cit_BU[, `:=`(scenario = s, simulation_data_year = scenario_year)]
  CIT_BU_list[[s]] <- copy(dt_cit_BU)
}


# 4. Simulation -----------------------------------------------------------

CIT_SIM_list <- list()

if (start_index > 1) {
  for (i in seq_len(start_index - 1)) {
    s_early <- scenarios[i]
    CIT_SIM_list[[s_early]] <- copy(CIT_BU_list[[s_early]])
  }
}

if (start_index == 1) {
  dt_cit_SIM <- copy(cit_raw)
} else {
  prev_scenario <- scenarios[start_index - 1]
  dt_cit_SIM   <- copy(CIT_BU_list[[prev_scenario]])
}

for (i in seq(from = start_index, to = length(scenarios))) {
  s             <- scenarios[i]
  scenario_year <- scenario_years_cit[i]
  
  gf_values <- get_growth_factor_row(s)
  for (v in vars_to_grow_existing) {
    dt_cit_SIM[, (v) := get(v) * gf_values[v]]
  }
  
  tax_calc_fun(dt_cit_SIM, cit_simulation_parameters_updated, scenario_year)
  dt_cit_SIM[, `:=`(scenario = s, simulation_data_year = scenario_year)]
  CIT_SIM_list[[s]] <- copy(dt_cit_SIM)
}

message("Block 2 (CIT_SIM_list) done.\n")
message("All done!\n")

rm(dt_cit_BU, dt_cit_SIM)


# 5. Aggregation ----------------------------------------------------------

summary_SIM <- summarize_CIT_fun(CIT_SIM_list, "_sim")
summary_BU  <- summarize_CIT_fun(CIT_BU_list,  "_bu")

merged_CIT_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)

merged_CIT_BU_SIM <- merge(
  scenario_lookup_cit,
  merged_CIT_BU_SIM,
  by.x  = "scenario",
  by.y  = "scenarios",
  all.x = TRUE,
  sort  = FALSE
)

setorder(merged_CIT_BU_SIM, year)
merged_CIT_BU_SIM$year <- as.character(merged_CIT_BU_SIM$year)

numeric_cols <- names(merged_CIT_BU_SIM)[sapply(merged_CIT_BU_SIM, is.numeric)]
merged_CIT_BU_SIM[,
                  (numeric_cols) := lapply(.SD, function(x) x / LCU_unit),
                  .SDcols = numeric_cols
]

cit_summary_df <- merged_CIT_BU_SIM %>%
  pivot_longer(
    cols          = -c(year, scenario),
    names_to      = c("variable", ".value"),
    names_pattern = "(.*)_(bu|sim)"
  ) %>%
  mutate(difference = sim - bu)


# Big corporations summary table ------------------------------------------

cit_summary_df_big <- cit_summary_df %>%
  mutate(across(c(bu, sim, difference), ~ round(., 1))) %>%
  dplyr::filter(variable == "calc_cit") %>%
  dplyr::select(year, bu, sim, difference) %>%
  dplyr::rename(
    "Current law (LCU Mil)"   = "bu",
    "Simulation (LCU Mil)"    = "sim",
    "Fiscal impact (LCU Mil)" = "difference"
  )

cit_summary_df_big$`Current law (LCU Mil)`   <- round(cit_summary_df_big$`Current law (LCU Mil)`   / 1e03, 1)
cit_summary_df_big$`Simulation (LCU Mil)`    <- round(cit_summary_df_big$`Simulation (LCU Mil)`    / 1e03, 1)
cit_summary_df_big$`Fiscal impact (LCU Mil)` <- round(cit_summary_df_big$`Fiscal impact (LCU Mil)` / 1e03, 1)

cit_summary_df_big <- left_join(cit_summary_df_big, MacroFiscalData, by = c("year" = "Year")) %>%
  dplyr::select(year, "Current law (LCU Mil)", "Simulation (LCU Mil)",
                "Fiscal impact (LCU Mil)", GDP) %>%
  dplyr::mutate(
    `Current law (Pct of GDP)`   = round(`Current law (LCU Mil)`   / GDP * 100, 2),
    `Simulation (Pct of GDP)`    = round(`Simulation (LCU Mil)`    / GDP * 100, 2),
    `Fiscal impact (Pct of GDP)` = round(`Fiscal impact (LCU Mil)` / GDP * 100, 2)
  ) %>%
  dplyr::select(-GDP) %>%
  as.data.table()


# 6. Deciles and Percentiles ----------------------------------------------

# 6.1 Business as usual ----
big_corporations_CIT_BU_list <- lapply(CIT_BU_list, as.data.table)

for (name in names(big_corporations_CIT_BU_list)) {
  df <- big_corporations_CIT_BU_list[[name]]
  df$decile_group  <- cal_weighted_deciles_fun(df$calc_grossinc)
  df$centile_group <- cal_weighted_centiles_fun(df$calc_grossinc)
  big_corporations_CIT_BU_list[[name]] <- df
}

# 6.2 Simulation ----
big_corporations_CIT_SIM_list <- lapply(CIT_SIM_list, as.data.table)

for (name in names(big_corporations_CIT_SIM_list)) {
  df <- big_corporations_CIT_SIM_list[[name]]
  df$decile_group  <- cal_weighted_deciles_fun(df$calc_grossinc)
  df$centile_group <- cal_weighted_centiles_fun(df$calc_grossinc)
  big_corporations_CIT_SIM_list[[name]] <- df
}


# 6.3 ETR -----------------------------------------------------------------

extract_filtered_re_df_fun <- function(list_input, forecast_horizon, SimulationYear,
                                       filter_positive = FALSE) {
  if (!SimulationYear %in% forecast_horizon)
    stop("The specified simulation year is not in the forecast horizons.")
  index           <- which(forecast_horizon == SimulationYear)
  df_out          <- list_input[[index]]
  columns_to_keep <- c("id_n", "calc_grossinc", "calc_cit")
  missing_columns <- setdiff(columns_to_keep, colnames(df_out))
  if (length(missing_columns) > 0)
    warning("Missing columns: ", paste(missing_columns, collapse = ", "))
  df_out <- df_out[, ..columns_to_keep, with = FALSE]
  if (filter_positive) {
    condition <- Reduce("&", lapply(columns_to_keep, function(col) {
      if (is.numeric(df_out[[col]])) df_out[[col]] > 0 else rep(TRUE, nrow(df_out))
    }))
    df_out <- df_out[condition]
  }
  return(df_out)
}

# FIX 2: filter to calc_grossinc > 0 & calc_cit > 0 for ETR computation
# Eliminates negative ETR from loss firms included in denominator in v2.

# BU
CIT_BU_simulation_year_df <- extract_filtered_re_df_fun(
  big_corporations_CIT_BU_list, forecast_horizon, SimulationYear
) %>%
  dplyr::filter(calc_grossinc > 0, calc_cit > 0)

etr_bu <- sum(CIT_BU_simulation_year_df$calc_cit) /
  sum(CIT_BU_simulation_year_df$calc_grossinc)

indicator_table_bu <- data.frame(
  Indicator  = "Effective tax rate",
  Name       = "etr_bu",
  Simulation = round(etr_bu, 4)
) %>% data.table()

# SIM
PIT_SIM_simulation_year_df <- extract_filtered_re_df_fun(
  big_corporations_CIT_SIM_list, forecast_horizon, SimulationYear
) %>%
  dplyr::filter(calc_grossinc > 0, calc_cit > 0)

etr_SIM <- sum(PIT_SIM_simulation_year_df$calc_cit) /
  sum(PIT_SIM_simulation_year_df$calc_grossinc)

indicator_table_SIM <- data.frame(
  Indicator  = "Effective tax rate",
  Name       = "etr_SIM",
  Simulation = round(etr_SIM, 4)
) %>% data.table()

# Merge BU and SIM ETR
re_effects_final_big <- merge(
  indicator_table_bu,
  indicator_table_SIM[, c("Indicator", "Simulation")],
  by       = "Indicator",
  suffixes = c("_bu", "_sim")
) %>%
  dplyr::select(-Name) %>%
  dplyr::rename(
    "Business as usual" = "Simulation_bu",
    "Simulation"        = "Simulation_sim"
  ) %>%
  dplyr::mutate(
    Simulation                  = round(Simulation, 4),
    `Percentage Difference (%)` = round(
      (`Simulation` - `Business as usual`) / `Business as usual` * 100, 1)
  )


# 7. Revenues by NACE sections --------------------------------------------

extract_nace_rev_big_coporations_fun <- function(cit_data, scenario) {
  cit_data[, .(calc_cit, section, description, scenario = scenario)]
}

extracted_tables_bu <- mapply(extract_nace_rev_big_coporations_fun,
                              CIT_BU_list, scenarios, SIMPLIFY = FALSE)
combined_data_bu    <- rbindlist(extracted_tables_bu)
result_bu           <- combined_data_bu[,
                                        .(total_calc_cit_bu = sum(calc_cit)), by = .(scenario, description, section)]

extracted_tables_sim <- mapply(extract_nace_rev_big_coporations_fun,
                               CIT_SIM_list, scenarios, SIMPLIFY = FALSE)
combined_data_sim    <- rbindlist(extracted_tables_sim)
result_sim           <- combined_data_sim[,
                                          .(total_calc_cit_sim = sum(calc_cit)), by = .(scenario, section)]

result_bu  <- merge(result_bu,  scenario_lookup_cit, by = "scenario", all.x = TRUE)
result_sim <- merge(result_sim, scenario_lookup_cit, by = "scenario", all.x = TRUE)

cit_nace_summary_big_coporations <- merge(
  result_bu, result_sim, by = c("scenario", "year", "section"), all = TRUE
) %>%
  dplyr::filter(year == SimulationYear)

cit_nace_summary_big_coporations$value <- "Value"

nace_cit_big_corporations_summary_long <- cit_nace_summary_big_coporations %>%
  dplyr::select(-c("scenario", "year", "value")) %>%
  dplyr::rename(
    "baseline"   = "total_calc_cit_bu",
    "simulation" = "total_calc_cit_sim"
  ) %>%
  pivot_longer(
    cols      = c(baseline, simulation),
    names_to  = "category",
    values_to = "value"
  )


# Wrap-up -----------------------------------------------------------------

cat("\n=== RECONCILIATION CHECK (base year:", base_year, ") ===\n")
base_s   <- scenario_lookup_cit[year == base_year, scenario]
dt_check <- CIT_BU_list[[base_s]]
cat("calc_cit_large (BU)         :",
    round(sum(dt_check$calc_cit_large, na.rm = TRUE) / 1e6, 2), "EUR mil\n")
cat("calc_cit (after discounts)  :",
    round(sum(dt_check$calc_cit,       na.rm = TRUE) / 1e6, 2), "EUR mil\n")
cat("Official cash collected     : 219.48 EUR mil\n")
cat("ETR BU                      :", re_effects_final_big$`Business as usual`[1], "\n")
cat("ETR SIM                     :", re_effects_final_big$Simulation[1], "\n")

cat("\n=== DECILE CHECK (base year) ===\n")
df_grp <- big_corporations_CIT_BU_list[[base_s]]
print(table(df_grp$decile_group))

print(merged_CIT_BU_SIM[, .(year, calc_cit_bu, calc_cit_sim)])

end.time  <- proc.time()
save.time <- end.time - start.time
cat("\nNumber of minutes running:", save.time[3] / 60, "\n\n")
