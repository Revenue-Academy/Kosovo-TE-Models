# Calc-TaxExpenditures_v3.R
# Changes vs v2:
#   1. Insurance CIT restricted to name_en == 'Insurance company' only
#   2. Overall 30% sponsorship cap applied on total (per form box 45)
#   All other logic unchanged from v2.

start.time <- proc.time()

# Ensure year-aware get_param_fun is defined ----
get_param_fun <- function(params_dt, param_name, year) {
  params_dt[Parameters == param_name & Year == year, Value]
}

# Safe growth factor retrieval ----
get_gf_safe <- function(scenario) {
  gf_row <- growth_factors_cit[growth_factors_cit$scenarios == scenario, ]
  out    <- numeric(length(vars_to_grow_existing))
  names(out) <- vars_to_grow_existing
  for (v in vars_to_grow_existing) {
    gf_col <- sub("_adjusted", "", v)
    val    <- tryCatch(gf_row[[gf_col]], error = function(e) NULL)
    if (!is.null(val) && length(val) > 0 && !is.na(val[1])) {
      out[v] <- val[1]
    } else {
      out[v] <- 1
    }
  }
  return(out)
}


# Helper: run one TE scenario ---------------------------------------------

run_te_scenario_fun <- function(params_to_zero) {
  
  # Step1 Build TE parameter table ----
  cit_simulation_parameters_te <- data.table::copy(cit_simulation_parameters_raw)
  for (p in params_to_zero) {
    cit_simulation_parameters_te[Parameters == p, Value := 0]
  }
  
  # Step2 Local tax calc function ----
  tax_calc_te_fun <- function(dt_scn, params_dt, year) {
    
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
    
    # 2.[17] Profit after adjustment to income
    dt_scn[, calc_profitloss_afteradjinc := profitloss_afteradjinc]
    
    # 3.[27] Total adjustment to expenses
    dt_scn[, calc_totadjexp_l18_l26 :=
             disallowedexp_sch_f + repcosts_sch_g + reservefunds_sch_h +
             paymrelper_sch_i + amort_sch_j + dep_sch_k +
             (specallownewassets_sch_l * rate_spl_all) + caploss_sch_c + otherexp_sch_m]
    
    # 4.[28] Gross income
    dt_scn[, calc_grossinc := calc_profitloss_afteradjinc - calc_totadjexp_l18_l26]
    
    # 5.[29] Charitable contributions
    dt_scn[, calc_charitycontrib_box28 :=
             pmin(charitycontrib_box28,
                  pmax(rate_max_charitable_ded * calc_grossinc, 0))]
    
    dt_scn[, calc_gti := calc_grossinc - calc_charitycontrib_box28]
    
    # 6.[31] Loss carry-forward
    dt_scn[, `:=`(
      newloss1 = fifelse(calc_gti < 0, calc_gti, 0),
      calc_nti = fifelse(calc_gti < 0, 0, calc_gti)
    )]
    
    # 7.[33] CIT non-insurance
    dt_scn[, calc_cit_non_insurance := pmax(calc_nti * cit_rate, 0)]
    
    # 8.[35] Insurance companies
    # FIX 1: restricted to actual insurance companies only.
    dt_scn[, calc_cit_insurance := pmax(
      fifelse(name_en == "Insurance company", grossprem * cit_rate_ins, 0),
      0
    )]
    
    dt_scn[, calc_cit_large := calc_cit_non_insurance + calc_cit_insurance]
    
    # 9.[39] Total credits
    dt_scn[, calc_totcredits_l37_l38 := forstattaxcredit_sch_o + taxwithheld_sch_p]
    
    # 10.[40] Line [36] less [39]
    dt_scn[, calc_l36_minus_l39 := pmax(0, calc_cit_large - calc_totcredits_l37_l38)]
    
    # [43] Sports sponsorship discount
    dt_scn[, calc_sportspodisc_max30pct_box42 :=
             pmin(sportspodisc_max30pct_box42,
                  rate_ded_sponsorship * calc_cit_large)]
    
    # [44] Culture and youth sponsorship discount
    dt_scn[, calc_culyouthspodisc_max20pct_box42 :=
             pmin(culyouthspodisc_max20pct_box42,
                  rate_ded_culture_youth * calc_cit_large)]
    
    # 11.[45] Total sponsorship discounts - FIX 2: overall 30% cap on total
    dt_scn[, calc_totalspodisc_max30pct_box42 :=
             pmin(
               calc_sportspodisc_max30pct_box42 + calc_culyouthspodisc_max20pct_box42,
               rate_ded_sponsorship * calc_cit_large
             )]
    
    # 12. Final CIT
    dt_scn[, calc_cit := pmax(calc_cit_large - calc_totalspodisc_max30pct_box42, 0)]
  }
  
  # Step3 BU loop ----
  CIT_BU_list_TE <- list()
  dt_cit_BU      <- copy(cit_raw)
  
  for (i in seq_along(scenarios)) {
    s             <- scenarios[i]
    scenario_year <- scenario_years_cit[i]
    
    gf_values <- get_gf_safe(s)
    for (v in vars_to_grow_existing) {
      dt_cit_BU[, (v) := get(v) * gf_values[v]]
    }
    
    tax_calc_te_fun(dt_cit_BU, cit_simulation_parameters_raw, scenario_year)
    dt_cit_BU[, `:=`(scenario = s, simulation_data_year = scenario_year)]
    CIT_BU_list_TE[[s]] <- copy(dt_cit_BU)
  }
  
  # Step4 SIM loop ----
  CIT_SIM_list_TE <- list()
  
  if (start_index > 1) {
    for (i in seq_len(start_index - 1)) {
      CIT_SIM_list_TE[[scenarios[i]]] <- copy(CIT_BU_list_TE[[scenarios[i]]])
    }
  }
  
  if (start_index == 1) {
    dt_cit_SIM <- copy(cit_raw)
  } else {
    dt_cit_SIM <- copy(CIT_BU_list_TE[[scenarios[start_index - 1]]])
  }
  
  for (i in seq(from = start_index, to = length(scenarios))) {
    s             <- scenarios[i]
    scenario_year <- scenario_years_cit[i]
    
    gf_values <- get_gf_safe(s)
    for (v in vars_to_grow_existing) {
      dt_cit_SIM[, (v) := get(v) * gf_values[v]]
    }
    
    tax_calc_te_fun(dt_cit_SIM, cit_simulation_parameters_te, scenario_year)
    dt_cit_SIM[, `:=`(scenario = s, simulation_data_year = scenario_year)]
    CIT_SIM_list_TE[[s]] <- copy(dt_cit_SIM)
  }
  
  rm(dt_cit_BU, dt_cit_SIM)
  
  # Step5 Aggregation ----
  summary_BU  <- summarize_CIT_fun(CIT_BU_list_TE,  "_bu")
  summary_SIM <- summarize_CIT_fun(CIT_SIM_list_TE, "_sim")
  
  merged_TE <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
  
  merged_TE <- merge(
    scenario_lookup_cit,
    merged_TE,
    by.x  = "scenario",
    by.y  = "scenarios",
    all.x = TRUE,
    sort  = FALSE
  )
  setorder(merged_TE, year)
  merged_TE$year <- as.character(merged_TE$year)
  
  numeric_cols <- names(merged_TE)[sapply(merged_TE, is.numeric)]
  merged_TE[, (numeric_cols) := lapply(.SD, function(x) x / LCU_unit), .SDcols = numeric_cols]
  
  # Step6 TE output ----
  cit_df_TE <- merged_TE %>%
    pivot_longer(
      cols          = -c(year, scenario),
      names_to      = c("variable", ".value"),
      names_pattern = "(.*)_(bu|sim)"
    ) %>%
    dplyr::mutate(difference = sim - bu) %>%
    dplyr::filter(variable == "calc_cit") %>%
    dplyr::select(year, bu, sim, difference) %>%
    dplyr::rename(
      "current law"     = "bu",
      "benchmark"       = "sim",
      "tax expenditure" = "difference"
    ) %>%
    #dplyr::mutate(across(where(is.numeric), ~ round(. / 1e03, 1)))
    dplyr::mutate(across(where(is.numeric), ~ round(., 1))) ### this line give in thousands
  
  
  # Step7 Attach GDP ----
  MacroFiscalData_te      <- copy(as.data.table(MacroFiscalData))
  MacroFiscalData_te$Year <- as.character(MacroFiscalData_te$Year)
  
  cit_df_TE <- left_join(cit_df_TE, MacroFiscalData_te, by = c("year" = "Year")) %>%
    dplyr::mutate(
      `current law (Pct of GDP)`     = round(`current law`     / GDP * 100, 2),
      `benchmark (Pct of GDP)`       = round(`benchmark`       / GDP * 100, 2),
      `tax expenditure (Pct of GDP)` = round(`tax expenditure` / GDP * 100, 2)
    ) %>%
    dplyr::select(-GDP) %>%
    as.data.table()
  
  list(
    full            = cit_df_TE,
    te_by_year      = cit_df_TE[, .(year, `tax expenditure`)],
    CIT_BU_list_TE  = CIT_BU_list_TE,
    CIT_SIM_list_TE = CIT_SIM_list_TE
  )
}


# Run individual TE scenarios ---------------------------------------------

cat("Running TE: rate_spl_all\n")
te_rate_spl_all     <- run_te_scenario_fun("rate_spl_all")
te_agg_rate_spl_all <- copy(te_rate_spl_all$te_by_year)
te_agg_rate_spl_all[, type := "Special allowance for new assets"]

cat("Running TE: rate_max_charitable_ded\n")
te_rate_charitable     <- run_te_scenario_fun("rate_max_charitable_ded")
te_agg_rate_charitable <- copy(te_rate_charitable$te_by_year)
te_agg_rate_charitable[, type := "Charitable contribution"]

cat("Running TE: rate_ded_culture_youth\n")
te_rate_culture     <- run_te_scenario_fun("rate_ded_culture_youth")
te_agg_rate_culture <- copy(te_rate_culture$te_by_year)
te_agg_rate_culture[, type := "Discounts for sponsorship in the field of culture and youth"]

cat("Running TE: rate_ded_sponsorship\n")
te_rate_sports     <- run_te_scenario_fun("rate_ded_sponsorship")
te_agg_rate_sports <- copy(te_rate_sports$te_by_year)
te_agg_rate_sports[, type := "Discounts for sponsorship in the field of sports"]


# Run all TE combined -----------------------------------------------------

cat("Running TE: all combined\n")
te_all <- run_te_scenario_fun(c(
  "rate_spl_all",
  "rate_max_charitable_ded",
  "rate_ded_culture_youth",
  "rate_ded_sponsorship"
))

te_summary_df <- te_all$full %>%
  dplyr::select(year, `current law`, benchmark, `tax expenditure`) %>%
  as.data.table()

te_summary_df_type <- rbindlist(list(
  te_agg_rate_spl_all,
  te_agg_rate_charitable,
  te_agg_rate_culture,
  te_agg_rate_sports
))

# write.csv(te_summary_df_type,"te_summary_df_type_cit.csv")

# NACE section breakdown --------------------------------------------------

extract_te_nace_fun <- function(cit_data, scenario) {
  cit_data[, .(calc_cit, section, description, scenario = scenario)]
}

extracted_nace_bu  <- mapply(extract_te_nace_fun, te_all$CIT_BU_list_TE,  scenarios, SIMPLIFY = FALSE)
extracted_nace_sim <- mapply(extract_te_nace_fun, te_all$CIT_SIM_list_TE, scenarios, SIMPLIFY = FALSE)

result_nace_bu  <- rbindlist(extracted_nace_bu)[,
                                                .(total_bu  = sum(calc_cit)), by = .(scenario, section, description)]
result_nace_sim <- rbindlist(extracted_nace_sim)[,
                                                 .(total_sim = sum(calc_cit)), by = .(scenario, section)]

result_nace_bu  <- merge(result_nace_bu,  scenario_lookup_cit, by = "scenario", all.x = TRUE)
result_nace_sim <- merge(result_nace_sim, scenario_lookup_cit, by = "scenario", all.x = TRUE)

nace_cit_summary_te <- merge(
  result_nace_bu, result_nace_sim,
  by = c("scenario", "year", "section"), all = TRUE
) %>%
  dplyr::mutate(tax_expenditures = total_sim - total_bu) %>%
  dplyr::filter(!is.na(section))

nace_cit_summary_tbl <- nace_cit_summary_te %>%
  dplyr::filter(year == SimulationYear) %>%
  dplyr::filter(!is.na(tax_expenditures), tax_expenditures > 0) %>%
  as.data.table()

nace_cit_summary_tbl[, value := "Total Tax Expenditures"]


# Company type breakdown --------------------------------------------------

extract_te_company_type_fun <- function(cit_data, scenario) {
  cit_data[, .(calc_cit, name_en, scenario = scenario)]
}

extracted_ct_bu  <- mapply(extract_te_company_type_fun, te_all$CIT_BU_list_TE,  scenarios, SIMPLIFY = FALSE)
extracted_ct_sim <- mapply(extract_te_company_type_fun, te_all$CIT_SIM_list_TE, scenarios, SIMPLIFY = FALSE)

result_ct_bu  <- rbindlist(extracted_ct_bu)[,
                                            .(total_bu  = sum(calc_cit)), by = .(scenario, name_en)]
result_ct_sim <- rbindlist(extracted_ct_sim)[,
                                             .(total_sim = sum(calc_cit)), by = .(scenario, name_en)]

result_ct_bu  <- merge(result_ct_bu,  scenario_lookup_cit, by = "scenario", all.x = TRUE)
result_ct_sim <- merge(result_ct_sim, scenario_lookup_cit, by = "scenario", all.x = TRUE)

company_type_cit_summary_te <- merge(
  result_ct_bu, result_ct_sim,
  by = c("scenario", "year", "name_en"), all = TRUE
) %>%
  dplyr::mutate(tax_expenditures = total_sim - total_bu) %>%
  dplyr::filter(!is.na(name_en)) %>%
  dplyr::filter(year == SimulationYear, tax_expenditures > 0) %>%
  as.data.table()


# Wrap-up -----------------------------------------------------------------

end.time  <- proc.time()
save.time <- end.time - start.time
cat("\nNumber of minutes running:", save.time[3] / 60, "\n\n")
