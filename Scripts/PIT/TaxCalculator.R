base_year <- unique(dt$Year)[1]
end_year <- base_year + 4
simulation_year <- SimulationYear  # Year from slider
forecast_horizon <- seq(base_year, end_year)


# Define the scenarios
scenarios <- c("t0", "t1", "t2", "t3", "t4")

# Simulation parameters must be in data.table
pit_simulation_parameters_raw <- pit_simulation_parameters_raw %>% data.table()
pit_simulation_parameters_updated <- pit_simulation_parameters_updated %>% data.table()


# pit_simulation_parameters_raw <-read_excel("PIT_Parameters.xlsx")
# pit_simulation_parameters_updated<-pit_simulation_parameters_raw

# 1. Tax Calculation Function -------------------------------------------------------

start.time <- proc.time()

tax_calc_fun <- function(dt_scn, params_dt) {
  rate1 <- get_param_fun(params_dt, "rate1")
  rate2 <- get_param_fun(params_dt, "rate2")
  rate3 <- get_param_fun(params_dt, "rate3")
  rate4 <- get_param_fun(params_dt, "rate4")
  tbrk1 <- get_param_fun(params_dt, "tbrk1")
  tbrk2 <- get_param_fun(params_dt, "tbrk2")
  tbrk3 <- get_param_fun(params_dt, "tbrk3")
  tbrk4 <- get_param_fun(params_dt, "tbrk4")
  rate_ded_rent <- get_param_fun(params_dt, "rate_ded_rent")
  rate_ded_charitable <- get_param_fun(params_dt, "rate_ded_charitable")
  toggle_exp_wages<- get_param_fun(params_dt, "toggle_exp_wages")
  
  'Income'
  
  # 1. D19 Total Income (add 8 to 18)----------------------------------------------------
  
  # Replace net_income_business with net_income_business_adjusted with 
  
  dt_scn[, calc_total_inc_non_witheld := gross_wage + net_income_business + net_income_partnership +
                                       gross_rents + gross_i_interest_pen_pay + gross_i_interest +
                                       gross_i_inta_prop + capital_gain + foreign_s_inc +
                                       other_inc_gifts]
  
  # 2. D24 Total deductions (add 20 to 23) ------------------------------------------------------------------------
  
  dt_scn[, calc_total_ded := pmax(ded_rents_expen_rents10pct, (rate_ded_rent * gross_rents)) +
           ded_pen_cont + ded_exp_int_prop + other_allowed_ded]
  
  # 3. D25 Taxable amount (19-24) ------------------------------------------
  
  dt_scn[, calc_gti := calc_total_inc_non_witheld - calc_total_ded]
  
  # 4. D27 Deduction for Charitable Contributions (max 5% of taxable amount) not claimed on FS -------------------------------------
  
  #dt_scn[, calc_charity_contribution := min(dis_charity_contribution, max(calc_gti * rate_ded_charitable, 0))]
  dt_scn[, calc_charity_contribution := pmin(dis_charity_contribution, pmax(calc_gti * rate_ded_charitable, 0))]
  
  # 5. D28 Total Additional Deductions (26+27)---------------
  
  dt_scn[, calc_tot_additional_ded := calc_charity_contribution + loss_carried_for]
  
  # 6. D29 Taxable Income before tax [25]-[28] -----------------------------------
  dt_scn[, calc_taxable_inc_before_tax := calc_gti - calc_tot_additional_ded]
  
  # 7. D30 Tax on Taxable Income as per tax brackets -------------------------------
  
  dt_scn[, pitax_n_w := (rate1 * pmin(calc_taxable_inc_before_tax, tbrk1) +
                       rate2 * pmin(tbrk2 - tbrk1, pmax(0, calc_taxable_inc_before_tax - tbrk1)) +
                       rate3 * pmin(tbrk3 - tbrk2, pmax(0, calc_taxable_inc_before_tax - tbrk2)) +
                       rate4 * pmax(0, calc_taxable_inc_before_tax - tbrk3))]
  
  # 8. Wages-Tax Withheld
  
  dt_scn[, calc_inc_bef_tax_withheld := gross_wage_w - employee_cont]
  
  
  dt_scn[, pitax_w := fifelse(
    toggle_exp_wages == 0 | tax_witheld > 0,          
    rate1 * pmin(calc_inc_bef_tax_withheld, tbrk1) +
      rate2 * pmin(tbrk2 - tbrk1,
                   pmax(0, calc_inc_bef_tax_withheld - tbrk1)) +
      rate3 * pmin(tbrk3 - tbrk2,
                   pmax(0, calc_inc_bef_tax_withheld - tbrk2)) +
      rate4 * pmax(0, calc_inc_bef_tax_withheld - tbrk3),
    0
  )]
  
  # dt_scn[, pitax_w := fifelse(
  #   toggle_exp_wages == 1 & tax_witheld > 0,         # <-- extra switch here
  #   rate1 * pmin(calc_inc_bef_tax_withheld, tbrk1) +
  #     rate2 * pmin(tbrk2 - tbrk1, pmax(0, calc_inc_bef_tax_withheld - tbrk1)) +
  #     rate3 * pmin(tbrk3 - tbrk2, pmax(0, calc_inc_bef_tax_withheld - tbrk2)) +
  #     rate4 * pmax(0, calc_inc_bef_tax_withheld - tbrk3),
  #   0
  # )]
  

  
  # dt_scn[, pitax_w := fifelse(
  #   tax_witheld > 0,
  #   rate1 * pmin(calc_inc_bef_tax_withheld, tbrk1) +
  #     rate2 * pmin(tbrk2 - tbrk1, pmax(0, calc_inc_bef_tax_withheld - tbrk1)) +
  #     rate3 * pmin(tbrk3 - tbrk2, pmax(0, calc_inc_bef_tax_withheld - tbrk2)) +
  #     rate4 * pmax(0, calc_inc_bef_tax_withheld - tbrk3),
  #   0)]

  
  # dt_scn[, pitax_w := (
  #   rate1 * pmin(calc_inc_bef_tax_withheld, tbrk1) +
  #     rate2 * pmin(tbrk2 - tbrk1, pmax(0, calc_inc_bef_tax_withheld - tbrk1)) +
  #     rate3 * pmin(tbrk3 - tbrk2, pmax(0, calc_inc_bef_tax_withheld - tbrk2)) +
  #     rate4 * pmax(0, calc_inc_bef_tax_withheld - tbrk3))]
  
  
  
  
  #dt_scn[, pitax := pmax(0, pitax_w + pitax_n_w),]
  dt_scn[, pitax :=  pitax_w + pitax_n_w]

  dt_scn[, calc_total_inc := gross_wage_w+calc_total_inc_non_witheld]
  
  
  # dt_scn[, `:=`(
  #   pitax          = pmax(0, pitax_w + pitax_n_w),
  #   calc_total_inc = pmax(0, gross_wage_w + calc_total_inc_non_witheld)
  # )]
  # 
  
  
  
  #calc_total_inc=gross_wage_w+calc_total_inc_non_witheld
  
}

# 2. Helper to Retrieve Growth Factors for Each Variable-------------------------------

vars_to_grow <- c(
  "gross_wage",
  "net_income_business",
  "net_income_partnership",
  "gross_rents",
  "gross_i_interest_pen_pay",
  "gross_i_interest",
  "gross_i_inta_prop",
  "capital_gain",
  "foreign_s_inc",
  "other_inc_gifts",
  "ded_rents_expen_rents10pct",
  "ded_pen_cont",
  "ded_exp_int_prop",
  "other_allowed_ded",
  "dis_charity_contribution",
  "loss_carried_for",
  # new
  "gross_wage_w",
  "tax_witheld",
  "employee_cont",
  "employer_cont" 
  
)

get_growth_factor_row <- function(scenario) {
  gf_row <- growth_factors_pit[scenarios == scenario]
  out <- numeric(length(vars_to_grow))
  names(out) <- vars_to_grow
  
  for (v in vars_to_grow) {
    gf_col <- sub("_adjusted", "", v)  # e.g. "g_Wages_l"
    out[v] <- gf_row[[gf_col]]
  }
  return(out)
}

# 3. Business as usual  ------------------------------------------------------

# Regardless of SimulationYear, we do the entire chain t0..t4 in PIT_BU_list
# so we have final data for each scenario if we need it in Block 2.

PIT_BU_list <- list()

# Start from baseline
dt_scn_BU <- copy(dt)

for (s in scenarios) {
  
  # 1) Retrieve scenario growth factors
  gf_values <- get_growth_factor_row(s)
  
  # 2) Multiply each variable by gf_values[v] * weights[[s]]
  for (v in vars_to_grow) {
    dt_scn_BU[, (v) := get(v) * gf_values[v] * weights_pit[[s]]]
  }
  
  # 3) Row-wise tax logic
  tax_calc_fun(dt_scn_BU, pit_simulation_parameters_raw)
  
  # 4) ADD a 'weight' column that references weights_pit[[s]]
  dt_scn_BU[, weight := weights_pit[[s]]]
  
  # 5) Store in PIT_BU_list
  PIT_BU_list[[s]] <- copy(dt_scn_BU)
}

# 4. Simulation --------------------------------------------------------------
start_index <- match(SimulationYear, scenario_years) # 1..5

PIT_SIM_list <- list()

# 1) Copy "early" scenarios (before SimulationYear) directly from PIT_BU_list.
#    e.g. if start_index=4 => we copy t0, t1, t2 (indexes 1..3).
if (start_index > 1) {
  for (i in seq_len(start_index - 1)) {
    s_early <- scenarios[i]
    PIT_SIM_list[[s_early]] <- copy(PIT_BU_list[[s_early]])
  }
}

# 2) Determine the starting data for re-simulation
if (start_index == 1) {
  # SimulationYear=2021 => start from original dt
  dt_scn_SIM <- copy(dt)
} else {
  # e.g. if start_index=4 => scenario t3 => the previous scenario is t2
  prev_scenario <- scenarios[start_index - 1]
  dt_scn_SIM <- copy(PIT_BU_list[[prev_scenario]])
}

# 3) Chain from scenario index = start_index .. 5
for (i in seq(from = start_index, to = length(scenarios))) {
  s <- scenarios[i]
  
  gf_values <- get_growth_factor_row(s)
  
  # Multiply each variable by growth factor * row-weight for scenario s
  for (v in vars_to_grow) {
    dt_scn_SIM[, (v) := get(v) * gf_values[v] * weights_pit[[s]]]
  }
  
  # Run row-wise calculations with updated parameters
  tax_calc_fun(dt_scn_SIM, pit_simulation_parameters_updated)
  
  # **Add a 'weight' column** with the row-specific weights_pit for scenario s
  dt_scn_SIM[, weight := weights_pit[[s]]]
  
  # Store final data in PIT_SIM_list
  PIT_SIM_list[[s]] <- copy(dt_scn_SIM)
}

message("Block 2 (PIT_SIM_list) done, including early years from PIT_BU_list, plus 'weight' column.\n")
message("All done!\n")

rm(dt_scn_BU, dt_scn_SIM)

# 5. Aggregation of simulated data -----------------------------------------------------

# Function to sum the specified columns in the list and store the results in a data frame
summary_SIM <- summarize_PIT_fun(PIT_SIM_list, "_sim")
summary_BU <- summarize_PIT_fun(PIT_BU_list, "_bu")

merged_PIT_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
merged_PIT_BU_SIM$year <- as.character(forecast_horizon)
merged_PIT_BU_SIM <- merged_PIT_BU_SIM[, c("year", names(merged_PIT_BU_SIM)[-length(merged_PIT_BU_SIM)])]

numeric_columns <- sapply(merged_PIT_BU_SIM, is.numeric)
merged_PIT_BU_SIM[, numeric_columns] <- merged_PIT_BU_SIM[, numeric_columns] / 1e06


#View(merged_PIT_BU_SIM)



# Decile & Centile --------------------------------------------------------


# keep_nonnegative <- function(lst, col = "net_income_business") {
#   lapply(lst, function(DT) {
#     setDT(DT)                
#     DT[get(col) >= 0]         
#   })
# }
# 
# 
# PIT_BU_list  <- keep_nonnegative(PIT_BU_list)
# PIT_SIM_list <- keep_nonnegative(PIT_SIM_list)

# keep_nonnegative <- function(lst, col = "net_income_business") {
#   lapply(lst, function(DT) {
#     setDT(DT)  # ensure it's a data.table
#     
#     # keep only rows where value > 0 and tax_payer_group == 0
#     #DT[get(col) >= 0 & tax_payer_group == "0"]
#     DT[get(col) >=0]
#   })
# }

# summary(PIT_BU_list$t0)
# 
# keep_nonnegative <- function(lst, col = "calc_total_inc") {
#   lapply(lst, function(DT) {
#     setDT(DT)  # ensure it's a data.table
#     
#     # keep only rows where value > 0 and tax_payer_group == 0
#     #DT[get(col) >0 & tax_payer_group == "0"]
#     DT[get(col) >0]
#   })
# }
# 
# # Apply the filtering
# PIT_BU_list  <- keep_nonnegative(PIT_BU_list)
# PIT_SIM_list <- keep_nonnegative(PIT_SIM_list)



#Apply the functions to each data frame in the PIT_BU_list
calc_weighted_groups_in_one_pass <- function(DT, inc_col = "calc_total_inc", w_col = "weight") {
  # 1. Keep track of original row order so we can restore it after sorting
  DT[, row_id__tmp := .I]
  
  # 2. Sort by income (use setorderv for a character column name)
  setorderv(DT, inc_col)
  
  # 3. Compute the cumulative sum of weight
  #    (handle NA weights as 0, adjust if you prefer a different approach)
  DT[, w_cumsum__tmp := cumsum(fifelse(is.na(get(w_col)), 0, get(w_col)))]
  
  # 4. Get the total weight
  total_w <- DT[.N, w_cumsum__tmp]
  
  # 5. Define breakpoints for deciles (10 groups) and centiles (100 groups)
  decile_breaks  <- seq(0, total_w, length.out = 11)   # 11 points => 10 intervals
  centile_breaks <- seq(0, total_w, length.out = 101)  # 101 points => 100 intervals
  
  # 6. Assign decile_group and centile_group
  DT[, decile_group  := findInterval(w_cumsum__tmp, decile_breaks,  rightmost.closed = TRUE)]
  DT[, centile_group := findInterval(w_cumsum__tmp, centile_breaks, rightmost.closed = TRUE)]
  
  # 7. Ensure the top boundary doesn't exceed the number of groups
  DT[, decile_group  := pmin(decile_group,  10)]
  DT[, centile_group := pmin(centile_group, 100)]
  
  # 8. Restore original row order
  setorder(DT, row_id__tmp)
  
  # 9. Clean up temporary columns
  DT[, c("row_id__tmp", "w_cumsum__tmp") := NULL]
  
  # Modifies DT in place, so no return() needed
  invisible(DT)
}

# -------------------------------------------------------------------
# Loop over lists in data.tables
# -------------------------------------------------------------------
for (i in seq_along(PIT_BU_list)) {
  calc_weighted_groups_in_one_pass(
    DT      = PIT_BU_list[[i]],
    inc_col = "calc_total_inc",
    w_col   = "weight"
  )
}


for (i in seq_along(PIT_BU_list)) {
  calc_weighted_groups_in_one_pass(
    DT      = PIT_SIM_list[[i]],
    inc_col = "calc_total_inc",
    w_col   = "weight"
  )
}




# Convert data for presentation in GUI
pit_summary_df <- merged_PIT_BU_SIM %>%
  pivot_longer(cols = -year, 
               names_to = c("variable", ".value"), 
               names_pattern = "(.*)_(bu|sim)")

# Calculate the difference between _sim and _bu columns
pit_summary_df <- pit_summary_df %>%
  mutate(difference = sim - bu)


pit_summary_df <- pit_summary_df %>%
  mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
  filter(variable=='pitax')

# Arrange the columns
pit_summary_df <- pit_summary_df %>%
  select(year, bu, sim, difference)%>%
  dplyr::rename(
    "Current law (LCU MIL)"="bu",
    "Simulation (LCU MIL)"="sim",
    "Fiscal impact (LCU MIL)"="difference",
  )


MACRO_FISCAL_INDICATORS$Year<-as.character(MACRO_FISCAL_INDICATORS$Year)

pit_summary_df<-left_join(pit_summary_df,MACRO_FISCAL_INDICATORS,by=c("year"="Year"))%>%
  select(year,"Current law (LCU MIL)","Simulation (LCU MIL)","Fiscal impact (LCU MIL)",Nominal_GDP)%>%
  dplyr::mutate( `Current law (Pct of GDP)`= round(`Current law (LCU MIL)`/Nominal_GDP*100,2),
                 `Simulation (Pct of GDP)`=round(`Simulation (LCU MIL)`/ Nominal_GDP*100,2),
                 `Fiscal impact (Pct of GDP)`=round(`Fiscal impact (LCU MIL)`/ Nominal_GDP*100,2))%>%
  dplyr::select(-c(Nominal_GDP))


pit_summary_df <- as.data.table(pit_summary_df)


# Kakwani ETR -------------------------------------------------------------


'Re-Distribution tables'
# Functions for calculation -----------------------------------------------
extract_filtered_re_df_fun <- function(PIT_BU_list, forecast_horizon, SimulationYear,
                                       filter_positive = FALSE) {
  # Validate SimulationYear: check if it is in the forecast horizon vector.
  if (!SimulationYear %in% forecast_horizon) {
    stop("The specified simulation year is not in the forecast horizons.")
  }
  
  # Find the index of the dataset that corresponds to SimulationYear.
  index <- which(forecast_horizon == SimulationYear)
  
  # Extract the specific data.table for the simulation year.
  PIT_BU_simulation_year_df <- PIT_BU_list[[index]]
  
  # Define the columns to keep.
  columns_to_keep <- c("id_n",
                       "calc_total_inc",
                       #"total_taxbase",
                       #"total_net",
                       "pitax")
  
  # Check for missing columns and issue a warning if any are not found.
  missing_columns <- setdiff(columns_to_keep, colnames(PIT_BU_simulation_year_df))
  if (length(missing_columns) > 0) {
    warning("The following columns are missing in the data frame: ",
            paste(missing_columns, collapse = ", "))
  }
  
  # Filter the data.table to keep only the specified columns.
  PIT_BU_simulation_year_df <- PIT_BU_simulation_year_df[, ..columns_to_keep, with = FALSE]
  
  # Optionally, filter rows where all numeric columns are greater than 0.
  if (filter_positive) {
    # Build a logical condition: for each column that is numeric, check that it is > 0.
    condition <- Reduce("&", lapply(columns_to_keep, function(col) {
      # If the column is numeric, return the boolean vector for > 0,
      # otherwise, return TRUE for all rows.
      if (is.numeric(PIT_BU_simulation_year_df[[col]])) {
        PIT_BU_simulation_year_df[[col]] > 0
      } else {
        rep(TRUE, nrow(PIT_BU_simulation_year_df))
      }
    }))
    PIT_BU_simulation_year_df <- PIT_BU_simulation_year_df[condition]
  }
  
  return(PIT_BU_simulation_year_df)
}



# 1.BU ----------------------------------------------------------------------

PIT_BU_simulation_year_df <- extract_filtered_re_df_fun(PIT_BU_list, forecast_horizon, SimulationYear)

PIT_BU_simulation_year_df<-PIT_BU_simulation_year_df%>%
  filter(pitax>0)

# Top 1

result <- PIT_BU_simulation_year_df %>%
  # Create 100 percentile groups based on calc_total_inc
  mutate(percentile = ntile(calc_total_inc, 100)) %>%
  # Keep only the group with the highest calc_total_inc
  filter(percentile == 100) %>%
  # Sum the PIT for observations in this percentile group
  summarise(total_citax = sum(pitax, na.rm = TRUE))


share_top1_bu<-result$total_citax/sum(PIT_BU_simulation_year_df$pitax)


# Gini gross income
gini_income_gross_bu <- round(ineq(PIT_BU_simulation_year_df$calc_total_inc, type = "Gini", na.rm = TRUE), 4)

# Calculate the Kakwani index
ineq<-calcSConc(PIT_BU_simulation_year_df$pitax, PIT_BU_simulation_year_df$calc_total_inc)
kakwani_index_BU <- round(ineq$ineq$index - gini_income_gross_bu, 4)
kakwani_index_BU <- unname(kakwani_index_BU)



etr_bu <- sum(PIT_BU_simulation_year_df$pitax) / sum(PIT_BU_simulation_year_df$calc_total_inc)

# calcAtkinson(PIT_BU_simulation_year_df$calc_total_inc, epsilon = 1)



# Calculate all indicators and store in a table
indicator_table_bu <- data.frame(
  Indicator = c(
    "Gini coefficient for pre-tax income",
    "Effective tax rate",
    "Kakwani Index"
  ),
  Name = c(
    "gini_income_gross_bu",
    "etr_bu",
    "kakwani_index_BU"
    
  ),
  Simulation = c(
    round(gini_income_gross_bu,4),
    round(etr_bu,4),
    round(kakwani_index_BU,4)
  )
)%>%data.table()%>%
dplyr::rename(`Business as usual`= "Simulation")



# 2.SIM -------------------------------------------------------------------


PIT_SIM_simulation_year_df <- extract_filtered_re_df_fun(PIT_SIM_list, forecast_horizon, SimulationYear)


PIT_SIM_simulation_year_df<-PIT_SIM_simulation_year_df%>%
  filter(pitax>0)


# TOP 1

result <- PIT_SIM_simulation_year_df %>%
  # Create 100 percentile groups based on calc_total_inc
  mutate(percentile = ntile(calc_total_inc, 100)) %>%
  # Keep only the group with the highest calc_total_inc
  filter(percentile == 100) %>%
  # Sum the PIT for observations in this percentile group
  summarise(total_citax = sum(pitax, na.rm = TRUE))


share_top1_sim<-result$total_citax/sum(PIT_SIM_simulation_year_df$pitax)




# Gini gross income
gini_income_gross_sim <- round(ineq(PIT_SIM_simulation_year_df$calc_total_inc, type = "Gini", na.rm = TRUE), 4)

# Calculate the Kakwani index
ineq<-calcSConc(PIT_SIM_simulation_year_df$pitax, PIT_SIM_simulation_year_df$calc_total_inc)
kakwani_index_SIM <- round(ineq$ineq$index - gini_income_gross_sim, 4)
kakwani_index_SIM <- unname(kakwani_index_SIM)



etr_SIM <- sum(PIT_SIM_simulation_year_df$pitax) / sum(PIT_SIM_simulation_year_df$calc_total_inc)

# calcAtkinson(PIT_SIM_simulation_year_df$calc_total_inc, epsilon = 1)



# Calculate all indicators and store in a table
indicator_table_SIM <- data.frame(
  Indicator = c(
    "Gini coefficient for pre-tax income",
    "Effective tax rate",
    "Kakwani Index"
  ),
  Name = c(
    "gini_income_gross_sim",
    "etr_SIM",
    "kakwani_index_SIM"
    
  ),
  Simulation = c(
    round(gini_income_gross_sim,4),
    round(etr_SIM,4),
    round(kakwani_index_SIM,4)
  )
)%>%data.table()



print(merged_PIT_BU_SIM)

end.time <- proc.time()
save.time <- end.time - start.time
cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")





# 7. Revenues by NACE sections----------------------------------------------------------------------

# Function to extract columns and add scenario identifier
extract_nace_rev_fun <- function(dt, scenario) {
  dt[, .(pitax, section,description, scenario = scenario)]
}

# Extract and process PIT_BU_list_TE
extracted_tables_bu <- mapply(extract_nace_rev_fun, PIT_BU_list, scenarios, SIMPLIFY = FALSE)
combined_data_bu <- rbindlist(extracted_tables_bu)
result_bu <- combined_data_bu[, .(total_calc_pitax_bu = sum(pitax)), by = .(scenario,description, section)]

# Extract and process PIT_SIM_list_TE

extracted_tables_sim <- mapply(extract_nace_rev_fun, PIT_SIM_list, scenarios, SIMPLIFY = FALSE)
combined_data_sim <- rbindlist(extracted_tables_sim)
result_sim <- combined_data_sim[, .(total_calc_pitax_sim = sum(pitax)), by = .(scenario,section)]

# Add year column to both results
result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
result_sim[, year := forecast_horizon[match(scenario, scenarios)]]

# Combine both results into one data frame
pit_nace_summary <- merge(result_bu, result_sim, by = c("scenario", "year","section"), all = TRUE)


pit_nace_summary<-pit_nace_summary%>%
  dplyr::filter(year==SimulationYear)

#pit_nace_summary$value<-"Value"
pit_nace_summary$value<-"In MIL LCU"


# 8. Revenues by Gender------------------------------------------------------
# Function to extract columns and add scenario identifier
extract_gender_rev_fun <- function(dt, scenario) {
  dt[, .(pitax, Gender,scenario = scenario)]
}

# Extract and process PIT_BU_list_TE
extracted_tables_bu <- mapply(extract_gender_rev_fun, PIT_BU_list, scenarios, SIMPLIFY = FALSE)
combined_data_bu <- rbindlist(extracted_tables_bu)
result_bu <- combined_data_bu[, .(total_calc_pitax_bu = sum(pitax)), by = .(scenario,Gender)]

# Extract and process PIT_SIM_list_TE

extracted_tables_sim <- mapply(extract_gender_rev_fun, PIT_SIM_list, scenarios, SIMPLIFY = FALSE)
combined_data_sim <- rbindlist(extracted_tables_sim)
result_sim <- combined_data_sim[, .(total_calc_pitax_sim = sum(pitax)), by = .(scenario,Gender)]

# Add year column to both results
result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
result_sim[, year := forecast_horizon[match(scenario, scenarios)]]

# Combine both results into one data frame
pit_gender_summary <- merge(result_bu, result_sim, by = c("scenario", "year","Gender"), all = TRUE)


pit_gender_summary<-pit_gender_summary%>%
  dplyr::filter(year==SimulationYear)
