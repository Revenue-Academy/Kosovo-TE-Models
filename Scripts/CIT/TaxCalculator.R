
# SimulationYear<-2023
# 
# library(readxl)
# library(dplyr)
# library(tidyverse)
# library(reshape2)
# library(data.table)
# library(plyr)
# library(purrr)
# library(Hmisc)
# 
# 
# cit_simulation_parameters_raw <- read_excel("CIT_Parameters.xlsx")
# cit_simulation_parameters_updated<-cit_simulation_parameters_raw

base_year <- unique(cit_raw$Year)[1]
end_year <- base_year + 4
SimulationYear <- SimulationYear  # Year from slider
forecast_horizon <- seq(base_year, end_year)

# Define the scenarios
scenarios <- c("t0", "t1", "t2", "t3", "t4")

# Simulation parameters must be in data.table
cit_simulation_parameters_raw <- cit_simulation_parameters_raw %>% data.table()
cit_simulation_parameters_updated <- cit_simulation_parameters_updated %>% data.table()

# 1. Tax Calculation Function -------------------------------------------------------

            start.time <- proc.time()
            
            tax_calc_fun <- function(dt_scn, params_dt) {
                                      cit_rate <- get_param_fun(params_dt, "cit_rate")
                                      cit_rate_ins <- get_param_fun(params_dt, "cit_rate_ins")
                                      rate_spl_all <- get_param_fun(params_dt, "rate_spl_all")
                                      rate_tax_rent <- get_param_fun(params_dt, "rate_tax_rent")
                                      rate_small_trans_ag <- get_param_fun(params_dt, "rate_small_trans_ag")
                                      min_payment_small <- get_param_fun(params_dt, "min_payment_small")
                                      rate_small_service_prof <- get_param_fun(params_dt, "rate_small_service_prof")
                                      rate_max_charitable_ded <- get_param_fun(params_dt, "rate_max_charitable_ded")
                                      Loss_CFLimit <- get_param_fun(params_dt, "Loss_CFLimit")
                                      rate_ded_culture_youth <- get_param_fun(params_dt, "rate_ded_culture_youth")
                                      rate_ded_sponsorship<- get_param_fun(params_dt, "rate_ded_sponsorship")
                          
# I. Corporations ----------------------------------------------
    # 1.[16] Total adjustment to income (add lines [11] to [15]) ----------------------------------------------------
      dt_scn[, calc_totadjinc_l11_l15 :=  forsourceincsch_a + recbaddebts_sch_b + capgain_sch_c + div_sch_d+otherincgain_sch_e]
    
    # 2.[17] Profit (loss) after adjustment to income (line [10] + line [16]) ------------------------------------------------------------------------
    dt_scn[, calc_profitloss_afteradjinc := profitloss_afteradjinc]
    ' 2. Adjustments to Expenses (mostly negative numbers shown in brackets - except line 24)'
    # 3.[27] Total adjustment to expenses (add lines [18] to [26]).------------------------------------------
    
    dt_scn[, calc_totadjexp_l18_l26 := disallowedexp_sch_f + repcosts_sch_g + reservefunds_sch_h + paymrelper_sch_i + amort_sch_j + dep_sch_k + (specallownewassets_sch_l * rate_spl_all) + caploss_sch_c + otherexp_sch_m]
  
    
    # 4.[28] Profit (loss) of business after adjustment to expenses (line [17]- line [27]), (When line 27 is in brackets add the whole numbers)---------------
    
    dt_scn[, calc_grossinc := calc_profitloss_afteradjinc - calc_totadjexp_l18_l26]
    
  
    # 5.[29] Charitable contributions (attach receipts), (limit of 5% of line 28) -----------------------------------
  
    dt_scn[, calc_charitycontrib_box28 := pmin(charitycontrib_box28, pmax((rate_max_charitable_ded*calc_grossinc), 0))]
  
    
    dt_scn[, calc_gti := calc_grossinc - calc_charitycontrib_box28]
  
  
    # 6.[31] Add lines [29] and [30]-------------------------------------
  
    dt_scn[, `:=`(
      newloss1 = fifelse(calc_gti < 0, Loss_lag1 + calc_gti, Loss_lag1 - pmin(abs(Loss_lag1), calc_gti)),
      calc_nti = fifelse(calc_gti < 0, 0, calc_gti - pmin(abs(Loss_lag1), calc_gti))
    )]
    
    
  
    # 7.[33].Corporate income tax (If line 32 is a profit, multiply by 10%. If line [32] is a loss, enter 0) -------------------------------
    
    dt_scn[, calc_cit_non_insurance :=  pmax(calc_nti * cit_rate, 0)]
    
    # For Insurance Companies only
  
    # 8.[35] Corporate Income Tax for Insurance Companies ( [34] * 5%) ------------------------------
  
          dt_scn[, calc_cit_insurance :=   pmax(gross_prem * cit_rate_ins, 0)]
          
          dt_scn[, calc_cit_large :=   calc_cit_non_insurance + calc_cit_insurance]
  
  
    
  
    # 9.[39] Total Credits for the period [37] +[38]------------------------
  
    dt_scn[, calc_totcredits_l37_l38 :=  forstattaxcredit_sch_o + taxwithheld_sch_p]
  
    # 10.[40] Line [36] less [39] ------------------------
    
    dt_scn[, calc_l36_minus_l39 :=  pmax(0, calc_cit_large - calc_totcredits_l37_l38)]
  
    
    # [43] Discounts for sponsorship in the field of sports (max.30% of the box [42]
    
    
    dt_scn[, calc_sportspodisc_max30pct_box42 :=  pmin(sportspodisc_max30pct_box42, (rate_ded_sponsorship * calc_cit_large))]
    
  # 44 Discounts for sponsorship in the field of Culture and Youth (Max. 20% of Box [42]
      dt_scn[, calc_culyouthspodisc_max20pct_box42 :=  pmin(culyouthspodisc_max20pct_box42, (rate_ded_culture_youth * calc_cit_large))]
      
    # 11.[45]	Total discounts for sponsorship ([43]+[44], max.30% of box [42]-------------------
  
      dt_scn[, calc_totalspodisc_max30pct_box42 := calc_sportspodisc_max30pct_box42 + calc_culyouthspodisc_max20pct_box42]
      
    # 12.Tax for payment with this form ([36]-[39]-[41]-[45]) ----------------
  
      dt_scn[, calc_cit := pmax((calc_cit_large - calc_totalspodisc_max30pct_box42), 0)  ]
      
  
      
      
# II.  Small Corporations ----------------------------------------
    # 1.Tax on quarterly income [10] = [8] x 3% (3% of gross income received from trade, transport, agricultural, or similar activities) -------------------------------
    dt_scn[, calc_tqi_3pct := g_inc_spcel*rate_small_trans_ag ]
    
    # 2.Tax on quarterly income [11]=[9] x 9% (9% of gross income for the quarter from services, professional, vocational, entertainment, or similar activities) -------------------------
    dt_scn[, calc_tqi_9pct := g_inc_q*rate_small_service_prof ]
    
    # 3.[12] Amount for payment in this statement [10]+[11] , but not less than 37.50 euro ---------------------------------------------------
    dt_scn[, calc_tot_amount_3pct_9pct := pmax(min_payment_small, pmax((calc_tqi_3pct + calc_tqi_9pct), 0)) ]
    
    # 4.[16] Amount for payment in this statement [14] - [15]----------------------------
      # da se najde od kade e ova rent_tax_held_oth ?????
      # namesto rent_tax_held_oth  vmetnav tir_hbo

      
      
    dt_scn[, calc_rent_tax := pmax((gross_inc_rent * rate_tax_rent - rent_tax_held_oth), 0) ]
    

    # 5.[17] Total tax Small Corporate ----------------------------

    dt_scn[, totax := (calc_tot_amount_3pct_9pct + calc_rent_tax)*4 ]
    

    dt_scn[, calc_turnover_small := (g_inc_q + g_inc_spcel)*4 ]
    
   
    
}

# 2. Helper to Retrieve Growth Factors for Each Variable-------------------------------

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
                  "gross_prem",
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
                  "tax_obligation",
                  "tax_refund",
                  "pun_installment",
                  "base_installment",
                  "grossinc",
                  "openinginv",
                  "costofprod",
                  "total_l61_l62",
                  "endinginventory",
                  "costofgoodssold_l63_l64",
                  "grossprofit_l60_l65",
                  "grosswages",
                  "depamortexp_notincost",
                  "sellingexp",
                  "genadminexp_notincost",
                  "rnd_costs",
                  "otheropexp",
                  "totalopexp_l67_l72",
                  "profitloss_oper_l66_l73",
                  "otherrevgains",
                  "otherexploss",
                  "profitloss_nonopact_l75_l76",
                  "netprofitloss_l74_l77",
                  "g_inc_spcel",
                  "g_inc_q",
                  "tqi_3pct",
                  "tqi_9pct",
                  "tot_amount_3pct_9pct",
                  "gross_inc_rent",
                  "tir_14pct",
                  #"tir_hbo",
                  "at_rental_incoe",
                  "tam_paid_stmt_17"
                  # "loss_quality",
                  # "loss_accidental"
                  )

get_growth_factor_row <- function(scenario) {
  gf_row <- growth_factors_cit[scenarios == scenario]
  out <- numeric(length(vars_to_grow))
  names(out) <- vars_to_grow
  
  for (v in vars_to_grow) {
    gf_col <- sub("_adjusted", "", v)  
    out[v] <- gf_row[[gf_col]]
  }
  return(out)
}

# 3. Business as usual  ------------------------------------------------------

# Regardless of SimulationYear, we do the entire chain t0..t4 in CIT_BU_list
# so we have final data for each scenario if we need it in Block 2.

CIT_BU_list <- list()

# Start from baseline
dt_cit_BU <- copy(cit_raw)

for (s in scenarios) {
  
  # 1) Retrieve scenario growth factors
  gf_values <- get_growth_factor_row(s)
  
  # 2) Multiply each variable by gf_values[v] * weights_cit[[s]]
  for (v in vars_to_grow) {
    dt_cit_BU[, (v) := get(v) * gf_values[v] * weights_cit[[s]]]
  }
  
  # 3) Row-wise tax logic
  tax_calc_fun(dt_cit_BU, cit_simulation_parameters_raw)
  
  # 4) ADD a 'weight' column that references weights_cit[[s]]
  dt_cit_BU[, weight := weights_cit[[s]]]
  
  # 5) Store in CIT_BU_list
  CIT_BU_list[[s]] <- copy(dt_cit_BU)
}

# 4. Simulation --------------------------------------------------------------
# (Starting from SimulationYear)
# Let's define SimulationYear. Suppose it's 2024 (but we can pick any 2021..2025).

# SimulationYear <- 2024

# We'll figure out which scenario index that corresponds to.
# scenario_years = c(2021, 2022, 2023, 2024, 2025)
# scenarios_5    = c("t0", "t1", "t2", "t3", "t4")
#
# If SimulationYear=2021 => start_index=1 => t0
# If SimulationYear=2022 => start_index=2 => t1
# If SimulationYear=2023 => start_index=3 => t2
# If SimulationYear=2024 => start_index=4 => t3
# If SimulationYear=2025 => start_index=5 => t4
start_index <- match(SimulationYear, scenario_years) # 1..5

CIT_SIM_list <- list()

# 1) Copy "early" scenarios (before SimulationYear) directly from CIT_BU_list.
#    e.g. if start_index=4 => we copy t0, t1, t2 (indexes 1..3).
if (start_index > 1) {
  for (i in seq_len(start_index - 1)) {
    s_early <- scenarios[i]
    CIT_SIM_list[[s_early]] <- copy(CIT_BU_list[[s_early]])
  }
}

# 2) Determine the starting data for re-simulation
if (start_index == 1) {
  # SimulationYear=2021 => start from original cit_raw
  dt_cit_SIM <- copy(cit_raw)
} else {
  # e.g. if start_index=4 => scenario t3 => the previous scenario is t2
  prev_scenario <- scenarios[start_index - 1]
  dt_cit_SIM <- copy(CIT_BU_list[[prev_scenario]])
}

# 3) Chain from scenario index = start_index .. 5
for (i in seq(from = start_index, to = length(scenarios))) {
  s <- scenarios[i]
  
  gf_values <- get_growth_factor_row(s)
  
  # Multiply each variable by growth factor * row-weight for scenario s
  for (v in vars_to_grow) {
    dt_cit_SIM[, (v) := get(v) * gf_values[v] * weights_cit[[s]]]
  }
  
  # Run row-wise calculations with updated parameters
  tax_calc_fun(dt_cit_SIM, cit_simulation_parameters_updated)
  
  # **Add a 'weight' column** with the row-specific weights_cit for scenario s
  dt_cit_SIM[, weight := weights_cit[[s]]]
  
  # Store final data in CIT_SIM_list
  CIT_SIM_list[[s]] <- copy(dt_cit_SIM)
}

message("Block 2 (CIT_SIM_list) done, including early years from CIT_BU_list, plus 'weight' column.\n")
message("All done!\n")

rm(dt_cit_BU, dt_cit_SIM)



sum(CIT_BU_list$t0$calc_cit)
sum(CIT_BU_list$t0$totax)



# 5. Aggregation of simulated data -----------------------------------------------------

              # Function to sum the specified columns in the list and store the results in a data frame
              summary_SIM <- summarize_CIT_fun(CIT_SIM_list, "_sim")
              summary_BU <- summarize_CIT_fun(CIT_BU_list, "_bu")
              
              merged_CIT_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
              merged_CIT_BU_SIM$year <- as.character(forecast_horizon)
              merged_CIT_BU_SIM <- merged_CIT_BU_SIM[, c("year", names(merged_CIT_BU_SIM)[-length(merged_CIT_BU_SIM)])]
              
              numeric_columns <- sapply(merged_CIT_BU_SIM, is.numeric)
              merged_CIT_BU_SIM[, numeric_columns] <- merged_CIT_BU_SIM[, numeric_columns] / LCU_unit



# Convert data for presentation in GUI
cit_summary_df <- merged_CIT_BU_SIM %>%
  pivot_longer(cols = -year, 
               names_to = c("variable", ".value"), 
               names_pattern = "(.*)_(bu|sim)")

# Calculate the difference between _sim and _bu columns
cit_summary_df <- cit_summary_df %>%
  mutate(difference = sim - bu)


cit_summary_df <- cit_summary_df %>%
  mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
  filter(variable=='calc_cit')

# Arrange the columns
cit_summary_df <- cit_summary_df %>%
            select(year, bu, sim, difference)%>%
            dplyr::rename(
              "Current law (LCU Mil)"="bu",
              "Simulation (LCU Mil)"="sim",
              "Fiscal impact (LCU Mil)"="difference",
            )


MACRO_FISCAL_INDICATORS$Year<-as.character(MACRO_FISCAL_INDICATORS$Year)

cit_summary_df<-left_join(cit_summary_df,MACRO_FISCAL_INDICATORS,by=c("year"="Year"))%>%
  select(year,"Current law (LCU Mil)","Simulation (LCU Mil)","Fiscal impact (LCU Mil)",Nominal_GDP)%>%
  dplyr::mutate( `Current law (Pct of GDP)`= round(`Current law (LCU Mil)`/Nominal_GDP*100,2),
                 `Simulation (Pct of GDP)`=round(`Simulation (LCU Mil)`/ Nominal_GDP*100,2),
                 `Fiscal impact (Pct of GDP)`=round(`Fiscal impact (LCU Mil)`/ Nominal_GDP*100,2))%>%
  dplyr::select(-c(Nominal_GDP))


cit_summary_df <- as.data.table(cit_summary_df)



print(merged_CIT_BU_SIM)

end.time <- proc.time()
save.time <- end.time - start.time
cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")

sum(CIT_BU_list$t0$calc_cit)
sum(CIT_BU_list$t0$totax)
 


# 6. Calculation of Deciles and Percentiles----------------------------------
          
          # 1.Preparation of subsets for estimation of decile and percentiles -------
          # 1.1 Big corporations ----------------------------------------------------
          # 1.1.1 Business as usual----------------------------------------------------
          
          # Preparation of subsets fo estimation of decile and percentiles
          big_corporations_CIT_BU_list <- lapply(CIT_BU_list, function(table) {
            table %>%
              filter(TypeOfTax == 0) %>%
              select(-calc_tqi_3pct, -calc_tqi_9pct, -calc_tot_amount_3pct_9pct, -calc_rent_tax, -totax, -calc_turnover_small)
          })
          
          # Estimation of decile and percentiles
          
          for (name in names(big_corporations_CIT_BU_list)) {
            df <- big_corporations_CIT_BU_list[[name]]
            df$decile_group <- cal_weighted_deciles_fun(df$calc_grossinc, df$weight)
            df$centile_group <- cal_weighted_centiles_fun(df$calc_grossinc, df$weight)
            big_corporations_CIT_BU_list[[name]] <- df
          }
          
          # 1.1.2 Simulation ------------------------------------------------------
          
          # Preparation of subsets fo estimation of decile and percentiles
          big_corporations_CIT_SIM_list <- lapply(CIT_SIM_list, function(table) {
            table %>%
              filter(TypeOfTax == 0) %>%
              select(-calc_tqi_3pct, -calc_tqi_9pct, -calc_tot_amount_3pct_9pct, -calc_rent_tax, -totax, -calc_turnover_small)
          })
          
          # Estimation of decile and percentiles
          for (name in names(big_corporations_CIT_SIM_list)) {
            df <- big_corporations_CIT_SIM_list[[name]]
            df$decile_group <- cal_weighted_deciles_fun(df$calc_grossinc, df$weight)
            df$centile_group <- cal_weighted_centiles_fun(df$calc_grossinc, df$weight)
            big_corporations_CIT_SIM_list[[name]] <- df
          }
          
          
          
          
          
          
          
          
          # 1.2 Small corporations  ----------------------------------------------------
          'ova ne e napraveno !!!'
          # # 1.2.1 Business as usual----------------------------------------------------
          #           
          #           # Preparation of subsets fo estimation of decile and percentiles
          #           small_corporations_CIT_BU_list <- lapply(CIT_BU_list, function(table) {
          #             table %>%
          #               filter(TypeOfTax == 1) %>%
          #               select(id_n,description,section,calc_tqi_3pct, calc_tqi_9pct, calc_tot_amount_3pct_9pct, calc_rent_tax, totax, calc_turnover_small)
          #           })
          #           
          #           # Estimation of decile and percentiles
          #           # Test with g_inc_spcel
          # 
          #           # Da se testira dali treba da se vkluci i g_inc_q
          # 
          #           
          #           
          #           for (name in names(small_corporations_CIT_BU_list)) {
          #             df <- small_corporations_CIT_BU_list[[name]]
          #             df$decile_group <- cal_weighted_deciles_fun(df$g_inc_q, df$weight)
          #             df$centile_group <- cal_weighted_centiles_fun(df$g_inc_q, df$weight)
          #             small_corporations_CIT_BU_list[[name]] <- df
          #           }
          #           
                     # 1.1.2 Simulation ------------------------------------------------------
          #           
          #           # Preparation of subsets fo estimation of decile and percentiles
          #           small_corporations_CIT_SIM_list <- lapply(CIT_SIM_list, function(table) {
          #             table %>%
          #               filter(TypeOfTax == 0) %>%
          #               select(-calc_tqi_3pct, -calc_tqi_9pct, -calc_tot_amount_3pct_9pct, -calc_rent_tax, -totax, -calc_turnover_small)
          #           })
          #           
          #           # Estimation of decile and percentiles
          #           for (name in names(small_corporations_CIT_SIM_list)) {
          #             df <- small_corporations_CIT_SIM_list[[name]]
          #             df$decile_group <- cal_weighted_deciles_fun(df$calc_grossinc, df$weight)
          #             df$centile_group <- cal_weighted_centiles_fun(df$calc_grossinc, df$weight)
          #             small_corporations_CIT_SIM_list[[name]] <- df
          #           }
          #           
          #           
          
          
          


# 7. Revenues by NACE sections (Big corporations) -----------------------------------------------------------------------
                        # Function to extract columns and add scenario identifier
            extract_nace_rev_big_coporations_fun <- function(cit_data, scenario) {
              cit_data[, .(calc_cit, section,description, scenario = scenario)]
            }

            # Extract and process PIT_BU_list_TE
            extracted_tables_bu <- mapply(extract_nace_rev_big_coporations_fun, CIT_BU_list, scenarios, SIMPLIFY = FALSE)
            combined_data_bu <- rbindlist(extracted_tables_bu)
            result_bu <- combined_data_bu[, .(total_calc_cit_bu = sum(calc_cit)), by = .(scenario,description, section)]
            
            # Extract and process cit_SIM_list_TE
            extracted_tables_sim <- mapply(extract_nace_rev_big_coporations_fun, CIT_SIM_list, scenarios, SIMPLIFY = FALSE)
            combined_data_sim <- rbindlist(extracted_tables_sim)
            result_sim <- combined_data_sim[, .(total_calc_cit_sim = sum(calc_cit)), by = .(scenario,section)]
            
            # Add year column to both results
            result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
            result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
            
            # Combine both results into one data frame
            cit_nace_summary_big_coporations <- merge(result_bu, result_sim, by = c("scenario", "year","section"), all = TRUE)


            cit_nace_summary_big_coporations<-cit_nace_summary_big_coporations%>%
              dplyr::filter(year==SimulationYear)

            cit_nace_summary_big_coporations$value<-"Value"
            
        
            nace_cit_big_corporations_summary_long <- cit_nace_summary_big_coporations %>%
              #dplyr::filter(year==simulation_year)%>%
              dplyr::select(-c("scenario","year","value"))%>%
              dplyr::rename(
                "baseline" ="total_calc_cit_bu",
                "simulation"="total_calc_cit_sim")%>%
              pivot_longer(cols = c(baseline, simulation), 
                           names_to = "category", 
                           values_to = "value")

# 8. Revenues by NACE sections (Small corporations) ------------------------------------------------------

            
            extract_nace_rev_small_coporations_fun <- function(cit_data, scenario) {
              cit_data[, .(totax, section,description, scenario = scenario)]
            }
            
          
            # Extract and process PIT_BU_list_TE
            extracted_tables_bu <- mapply(extract_nace_rev_small_coporations_fun, CIT_BU_list, scenarios, SIMPLIFY = FALSE)
            combined_data_bu <- rbindlist(extracted_tables_bu)
            result_bu <- combined_data_bu[, .(total_totax_bu = sum(totax)), by = .(scenario,description, section)]
            
            # Extract and process cit_SIM_list_TE
            extracted_tables_sim <- mapply(extract_nace_rev_small_coporations_fun, CIT_SIM_list, scenarios, SIMPLIFY = FALSE)
            combined_data_sim <- rbindlist(extracted_tables_sim)
            result_sim <- combined_data_sim[, .(total_totax_sim = sum(totax)), by = .(scenario,section)]
            
            # Add year column to both results
            result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
            result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
            
            # Combine both results into one data frame
            cit_nace_summary_small_coporations <- merge(result_bu, result_sim, by = c("scenario", "year","section"), all = TRUE)
            
            
            cit_nace_summary_small_coporations<-cit_nace_summary_small_coporations%>%
              dplyr::filter(year==SimulationYear)
            
            cit_nace_summary_small_coporations$value<-"Value"
            
            
            nace_cit_small_corporations_summary_long <- cit_nace_summary_small_coporations %>%
              #dplyr::filter(year==simulation_year)%>%
              dplyr::select(-c("scenario","year","value"))%>%
              dplyr::rename(
                "baseline" ="total_totax_bu",
                "simulation"="total_totax_sim")%>%
              pivot_longer(cols = c(baseline, simulation), 
                           names_to = "category", 
                           values_to = "value")
            




            
            
            
            