# Distribution tables -----------------------------------------------------

# FUNCTIONS ---------------------------------------------------------------

extract_centile_rev_fun <- function(cit_raw, scenario) {
  cit_raw[, .(centile_group, calc_cit, calc_grossinc, scenario = scenario)]
}

extract_dec_rev_fun <- function(cit_raw, scenario) {
  cit_raw[, .(decile_group, calc_cit, calc_grossinc, scenario = scenario)]
}

extract_bins_rev_fun <- function(cit_raw, scenario) {
  cit_raw[, .(id_n, calc_grossinc, calc_cit, scenario = scenario)]
}


# I. Big corporations -----------------------------------------------------

# 1. Centile Groups -------------------------------------------------------

# 1.1 BU ----

extracted_dist_tables_bu <- mapply(extract_centile_rev_fun, big_corporations_CIT_BU_list, scenarios, SIMPLIFY = FALSE)
combined_dt              <- rbindlist(extracted_dist_tables_bu)

combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
combined_dt <- combined_dt %>% dplyr::filter(year == SimulationYear)
combined_dt <- as.data.table(combined_dt)

cit_centile_distribution_bu <- combined_dt[, .(
  sum_calc_citax         = round(sum(calc_cit,      na.rm = TRUE), 1),
  sum_total_gross_income = round(sum(calc_grossinc, na.rm = TRUE), 1)
), by = .(scenario, centile_group)]

cit_centile_distribution_bu[, etr  := sum_calc_citax / sum_total_gross_income]
cit_centile_distribution_bu[, year := forecast_horizon[match(scenario, scenarios)]]
setorder(cit_centile_distribution_bu, centile_group)


# 1.2 SIM ----

extracted_dist_tables_sim <- mapply(extract_centile_rev_fun, big_corporations_CIT_SIM_list, scenarios, SIMPLIFY = FALSE)
combined_dt               <- rbindlist(extracted_dist_tables_sim)

combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
combined_dt <- combined_dt %>% dplyr::filter(year == SimulationYear)
combined_dt <- as.data.table(combined_dt)

cit_centile_distribution_sim <- combined_dt[, .(
  sum_calc_citax         = round(sum(calc_cit,      na.rm = TRUE), 1),
  sum_total_gross_income = round(sum(calc_grossinc, na.rm = TRUE), 1)
), by = .(scenario, centile_group)]

cit_centile_distribution_sim[, etr  := sum_calc_citax / sum_total_gross_income]
cit_centile_distribution_sim[, year := forecast_horizon[match(scenario, scenarios)]]
setorder(cit_centile_distribution_sim, centile_group)


# 1.3 Merge BU and SIM ----

setkey(cit_centile_distribution_bu,  centile_group, scenario, year)
setkey(cit_centile_distribution_sim, centile_group, scenario, year)

cit_centile_distribution_bu_sim <- merge(
  cit_centile_distribution_bu,
  cit_centile_distribution_sim,
  by       = c("centile_group", "scenario", "year"),
  suffixes = c("_bu", "_sim")
)
setorder(cit_centile_distribution_bu_sim, centile_group)

cit_centile_distribution_bu_sim <- cit_centile_distribution_bu_sim %>%
  dplyr::filter(year == SimulationYear)

all_centile_groups <- data.table(centile_group = as.integer(1:100))

cit_centile_distribution_bu_sim <- merge(
  all_centile_groups,
  cit_centile_distribution_bu_sim,
  by    = "centile_group",
  all.x = TRUE
)

cit_centile_distribution_bu_sim[is.na(cit_centile_distribution_bu_sim)] <- 0
cit_centile_distribution_bu_sim[, centile_group := factor(centile_group, levels = as.integer(1:100))]
setorder(cit_centile_distribution_bu_sim, centile_group)


# 2. Decile Groups --------------------------------------------------------

# 2.1 BU ----

extracted_dist_tables_bu <- mapply(extract_dec_rev_fun, big_corporations_CIT_BU_list, scenarios, SIMPLIFY = FALSE)
combined_dt              <- rbindlist(extracted_dist_tables_bu)

combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
combined_dt <- combined_dt %>% dplyr::filter(year == SimulationYear)

cit_decile_distribution_bu <- combined_dt[, .(
  sum_calc_citax         = sum(calc_cit,      na.rm = TRUE),
  mean_calc_citax        = mean(calc_cit,     na.rm = TRUE),
  sum_total_gross_income = sum(calc_grossinc, na.rm = TRUE)
), by = .(scenario, decile_group)]

cit_decile_distribution_bu[, year := forecast_horizon[match(scenario, scenarios)]]
setorder(cit_decile_distribution_bu, decile_group)


# 2.2 SIM ----

extracted_dist_tables_sim <- mapply(extract_dec_rev_fun, big_corporations_CIT_SIM_list, scenarios, SIMPLIFY = FALSE)
combined_dt               <- rbindlist(extracted_dist_tables_sim)

combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
combined_dt <- combined_dt %>% dplyr::filter(year == SimulationYear)

cit_decile_distribution_sim <- combined_dt[, .(
  sum_calc_citax         = sum(calc_cit,      na.rm = TRUE),
  mean_calc_citax        = mean(calc_cit,     na.rm = TRUE),
  sum_total_gross_income = sum(calc_grossinc, na.rm = TRUE)
), by = .(scenario, decile_group)]

cit_decile_distribution_sim[, year := forecast_horizon[match(scenario, scenarios)]]
setorder(cit_decile_distribution_sim, decile_group)


# 2.3 Merge BU and SIM ----

setkey(cit_decile_distribution_bu,  decile_group, scenario, year)
setkey(cit_decile_distribution_sim, decile_group, scenario, year)

cit_decile_distribution_bu_sim_raw <- merge(
  cit_decile_distribution_bu,
  cit_decile_distribution_sim,
  by       = c("decile_group", "scenario", "year"),
  suffixes = c("_bu", "_sim")
)

cit_decile_distribution_bu_sim <- cit_decile_distribution_bu_sim_raw

cit_decile_distribution_bu_sim$year         <- as.character(cit_decile_distribution_bu_sim$year)
cit_decile_distribution_bu_sim$decile_group <- as.character(cit_decile_distribution_bu_sim$decile_group)

cit_decile_distribution_bu_sim <- setnames(
  cit_decile_distribution_bu_sim,
  old = c("decile_group", "sum_calc_citax_bu", "mean_calc_citax_bu", "sum_total_gross_income_bu",
          "sum_calc_citax_sim", "mean_calc_citax_sim", "sum_total_gross_income_sim"),
  new = c("Decile groups",
          "Total CIT liability (business as usual)",
          "Average CIT liability (business as usual)",
          "Total gross income (business as usual)",
          "Total CIT liability (simulation)",
          "Average CIT liability (simulation)",
          "Total gross income (simulation)")
)

cols_mil <- c(
  "Total CIT liability (business as usual)",
  "Total gross income (business as usual)",
  "Total CIT liability (simulation)",
  "Total gross income (simulation)"
)
cit_decile_distribution_bu_sim[, (cols_mil) := lapply(.SD, function(x) round(x / 1e6, 1)), .SDcols = cols_mil]

cols_thou <- c(
  "Average CIT liability (business as usual)",
  "Average CIT liability (simulation)"
)
cit_decile_distribution_bu_sim[, (cols_thou) := lapply(.SD, function(x) round(x / 1e3, 1)), .SDcols = cols_thou]

cit_decile_distribution_bu_sim <- setnames(
  cit_decile_distribution_bu_sim,
  old = c("Decile groups",
          "Total CIT liability (business as usual)",
          "Average CIT liability (business as usual)",
          "Total gross income (business as usual)",
          "Total CIT liability (simulation)",
          "Average CIT liability (simulation)",
          "Total gross income (simulation)"),
  new = c("Decile groups",
          "Total CIT liability (business as usual) in MIL",
          "Average CIT liability (business as usual) in THOUSAND",
          "Total gross income (business as usual) in MIL",
          "Total CIT liability (simulation) in MIL",
          "Average CIT liability (simulation) in THOUSAND",
          "Total gross income (simulation) in MIL")
)

setorder(cit_decile_distribution_bu_sim, year)

cit_decile_distribution_bu_sim <- cit_decile_distribution_bu_sim %>%
  dplyr::filter(year == SimulationYear)

cit_decile_distribution_bu_sim$scenario <- NULL
cit_decile_distribution_bu_sim$year     <- NULL

cit_decile_distribution_bu_sim <- cit_decile_distribution_bu_sim %>%
  dplyr::select(-c("Total gross income (business as usual) in MIL",
                   "Total gross income (simulation) in MIL"))

all_decile_groups <- data.table(`Decile groups` = as.character(1:10))

cit_decile_distribution_bu_sim <- merge(
  all_decile_groups,
  cit_decile_distribution_bu_sim,
  by    = "Decile groups",
  all.x = TRUE
)

cit_decile_distribution_bu_sim[is.na(cit_decile_distribution_bu_sim)] <- 0
cit_decile_distribution_bu_sim[, `Decile groups` := factor(`Decile groups`, levels = as.character(1:10))]
setorder(cit_decile_distribution_bu_sim, `Decile groups`)


# 3. Distribution Table - Income Breaks -----------------------------------

breaks <- c(-Inf, 0, 1e-09, 5000000, 10000000, 15000000, 20000000,
            30000000, 40000000, 50000000, 100000000, 500000000, 1000000000, 9e+99)
labels <- c("<0", "=0", "0-5m", "5-10m", "10-15m", "15-20m", "20-30m",
            "30-40m", "40-50m", "50-100m", "100-500m", "500m-1bn", ">1bn")

# 3.1 BU ----

combined_dt_bins_fun <- rbindlist(lapply(names(big_corporations_CIT_BU_list), function(scenario) {
  data <- big_corporations_CIT_BU_list[[scenario]] %>%
    dplyr::select(id_n, calc_grossinc, calc_cit) %>%
    dplyr::mutate(
      bin_group = cut(calc_grossinc, breaks = breaks, labels = labels, right = FALSE),
      scenario  = scenario
    )
  as.data.table(data)
}))

cit_result_bins    <- combined_dt_bins_fun[, .(sum_calc_citax = sum(calc_cit)), by = .(bin_group, scenario)]
all_scenarios      <- combined_dt_bins_fun[, .(bin_group = "ALL", sum_calc_citax = sum(calc_cit)), by = scenario]
cit_result_bins_bu <- rbind(cit_result_bins, all_scenarios, fill = TRUE)
cit_result_bins_bu[, year := forecast_horizon[match(scenario, scenarios)]]

cit_result_bins_bu_sub <- cit_result_bins_bu %>%
  dplyr::filter(year == SimulationYear) %>%
  dplyr::filter(bin_group != "ALL" & bin_group != "0")


# 3.2 SIM ----

combined_dt_bins_fun <- rbindlist(lapply(names(big_corporations_CIT_SIM_list), function(scenario) {
  data <- big_corporations_CIT_SIM_list[[scenario]] %>%
    dplyr::select(id_n, calc_grossinc, calc_cit) %>%
    dplyr::mutate(
      bin_group = cut(calc_grossinc, breaks = breaks, labels = labels, right = FALSE),
      scenario  = scenario
    )
  as.data.table(data)
}))

cit_result_bins     <- combined_dt_bins_fun[, .(sum_calc_citax = sum(calc_cit)), by = .(bin_group, scenario)]
all_scenarios       <- combined_dt_bins_fun[, .(bin_group = "ALL", sum_calc_citax = sum(calc_cit)), by = scenario]
cit_result_bins_sim <- rbind(cit_result_bins, all_scenarios, fill = TRUE)
cit_result_bins_sim[, year := forecast_horizon[match(scenario, scenarios)]]

cit_result_bins_sim_sub <- cit_result_bins_sim %>%
  dplyr::filter(year == SimulationYear) %>%
  dplyr::filter(bin_group != "ALL" & bin_group != "0") %>%
  dplyr::select(-c(scenario, year))

cit_result_bins_sim_sub[, bin_group := factor(bin_group, levels = c(
  "<0", "=0", "0-5m", "5-10m", "10-15m", "15-20m", "20-30m",
  "30-40m", "40-50m", "50-100m", "100-500m", "500m-1bn", ">1bn"
))]
setorder(cit_result_bins_sim_sub, bin_group)

cit_result_bins_sim_sub$sum_calc_citax <- round(cit_result_bins_sim_sub$sum_calc_citax / 1e06, 1)
