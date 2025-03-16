
# 0.Defining functions ----------------------------------------------------


                'Artifical data'
                HighestBaseSSC_Employment<-16
                
                PersonalAllowance<-101256
                
                average_wage <- 41141
                max_base_wage <- average_wage * HighestBaseSSC_Employment * 12 
                
                SSC_rate <- 0.28
                
                ssc_temp_rate <- 0
                
                base_year <- unique(pit_data$sample$Year)[1]  
                end_year <- base_year + 4
                SimulationYear <- SimulationYear  # Year from slider
                forecast_horizon <- seq(base_year, end_year)
                
                # Define the scenarios
                scenarios <- c("t0", "t1", "t2", "t3", "t4")
                
                PIT_BU_list <- list()
                PIT_SIM_list <- list()
                
                # Convert data to data.table
                pit_data$sample <- as.data.table(pit_data$sample)
                pit_data$weights <- lapply(pit_data$weights, as.data.table)
                pit_data$growth_factors <- as.data.table(pit_data$growth_factors)



# I. ESTIMATION TAX LIABILITY FOR INCOME FROM LABOR ----------------------
            filter_columns <- function(data) {
              col_names <- colnames(data)
              exclude_pattern <- "^(g_|d_).+(_l|_c)$|^(tax_base(?!_other))$|^deductions_|^Year$|^g_total_net_l$"
              cols_to_keep <- col_names[!grepl(exclude_pattern, col_names, perl = TRUE)]
              cols_to_keep <- c(cols_to_keep, "g_total_net_l")
              filtered_data <- data[, ..cols_to_keep, with = FALSE]
              return(filtered_data)
            }


# Define functions
            # 1.1 SSC -----------------------------------------------------------------
                      max_ssc <- max_base_wage * SSC_rate # '# <--------OVA TREBA DA SE SETIRA '
            
                      vec_cal_ssc_w_fun <- Vectorize(function(total_ssc, g_Wages_l_weighted, SSC_rate, max_base_wage, max_ssc) {
                        if (total_ssc == 0 && g_Wages_l_weighted > 0) {
                          calc_ssc <- 0
                        } else if (g_Wages_l_weighted < max_base_wage) {
                          calc_ssc <- SSC_rate * g_Wages_l_weighted
                        } else if (g_Wages_l_weighted > max_base_wage) {
                          calc_ssc <- max_ssc
                        }
                        return(calc_ssc)
                      })
            
            
            
            # 1.2 Weighting Function --------------------------------------------------
            cal_weighting_fun <- function(sample_data, weights, growth_factors, columns_to_process) {
              for (col in columns_to_process) {
                sample_data[, (paste0(col, "_adjusted")) := get(col) * weights * growth_factors]
              }
              
              return(sample_data)
            }
            
            
            # 1.3 Function to determine which dataset to use based on SimulationYear
            
            get_sim_dataset <- function(current_year, SimulationYear, base_year) {
              if (current_year < SimulationYear) {
                return(pit_simulation_parameters_raw)
              } else {
                return(pit_simulation_parameters_updated)
              }
            }
            
            
            
            # 1.4 Calculation for tax base for wages --------------------------
            
            vec_cal_tax_base_w_fun <- Vectorize(function(g_Wages_l_adjusted, calc_ssc, total_personal_allowance, weight_personal_allowance_w, g_d_total_tax_reduction_l, tax_credit_donation) {
              personal_allowance_new <- total_personal_allowance * weight_personal_allowance_w # Calculate the adjusted personal allowance
              tax_base_wages1 <- ifelse(g_d_total_tax_reduction_l > 0, # Calculate tax base wages1 (applies when g_d_total_tax_reduction_l > 0)
                                        g_Wages_l_adjusted - calc_ssc - personal_allowance_new - tax_credit_donation/0.1 ,
                                        0)
              tax_base_wages1 <- max(tax_base_wages1, 0)  # Ensure tax_base_wages1 is non-negative
              tax_base_wages2 <- ifelse(g_d_total_tax_reduction_l == 0, # Calculate tax base wages2 (applies when g_d_total_tax_reduction_l == 0)
                                        g_Wages_l_adjusted - calc_ssc - personal_allowance_new,
                                        0)
              tax_base_wages2 <- max(tax_base_wages2, 0)  # Ensure tax_base_wages2 is non-negative
              #tax_base_wages_diplomatic_consular_l <- g_WagesDiplomaticConsular_l_adjusted - g_d_WagesDiplomaticConsular_l_adjusted # Calculate the diplomatic/consular adjustment
              tax_base_w <- tax_base_wages1 + tax_base_wages2 #+ tax_base_wages_diplomatic_consular_l  # Sum tax_base_wages1 and tax_base_wages2 for the final tax base
              return(tax_base_w)
            })
            
            
            #  Donations to a legal entity .----------------------------
            # In accordance with the provisions of the Law on Donations and Sponsorship in Public Activities is creditable up to 20% of the personal income tax liability, up to a maximum annual credit of MKD 24,000
            
            vec_cal_tax_base_agr_fun <- Vectorize(function(g_AgriculturalProductsOwn_l_adjusted,rate_ded_income_agr_l) {
              tax_base_agr <- g_AgriculturalProductsOwn_l_adjusted-(g_AgriculturalProductsOwn_l_adjusted *rate_ded_income_agr_l)
              return(tax_base_agr)
            })
            
            
            # 1.6 Estimation of deductions (prescribed costs) and tax base for Agricultural/Copyright income  ------------------------------
            # 1.6.1 Income of the basis of sale of own agricultural products------------------------------
            
            
            
            vec_cal_tax_base_agr_fun <- Vectorize(function(g_AgriculturalProductsOwn_l_adjusted,rate_ded_income_agr_l) {
              tax_base_agr <- g_AgriculturalProductsOwn_l_adjusted-(g_AgriculturalProductsOwn_l_adjusted *rate_ded_income_agr_l)
              return(tax_base_agr)
            })
            
            
            # 3. Total tax base OTHER INCOME from labor (not include wages ) ------------------------------
            
            vec_cal_tax_base_other_fun <- Vectorize(function(tax_base_agr, 
                                                             g_TemporaryContracts_l) {
              tax_base_other <- (tax_base_agr + g_TemporaryContracts_l)
              tax_base_other <- max(tax_base_other, 0)
              return(tax_base_other)
            })
            
            
            # 1.5 Calculation for PIT for wages ------------------------------
            vec_cal_pit_w_fun <- Vectorize(function(tax_base_w,tax_base_other, rate1, rate2, rate3, rate4, tbrk1, tbrk2, tbrk3) {
              tti_w_I <- tax_base_w + tax_base_other
              pit_w <- (rate1 * min(tti_w_I, tbrk1) +
                          rate2 * min(tbrk2 - tbrk1, max(0, tti_w_I - tbrk1)) +
                          rate3 * min(tbrk3 - tbrk2, max(0, tti_w_I - tbrk2)) +
                          rate4 * max(0, tti_w_I - tbrk3))
              return(pit_w)
            })
            

                        
# II. ESTIMATION TAX LIABILITY FOR INCOME FROM CAPITAL ----------------------
          # 1. Estimation of tax base for capital incomes without deductions (prescribed cost)------------------------------         
          vec_cal_tax_base_IndPropRights_c_fun <- Vectorize(function(g_IndustrialPropertyRights_c_adjusted, rate_deductions_IndustrialPropertyRights_c) {
            tax_base_IndustrialPropertyRights_c <- g_IndustrialPropertyRights_c_adjusted - (g_IndustrialPropertyRights_c_adjusted*rate_deductions_IndustrialPropertyRights_c)
            return(tax_base_IndustrialPropertyRights_c)
          })  
          
          # 2.Income on the basis of lease----------------------------------
          vec_cal_tax_base_Lease_c_fun <- Vectorize(function(g_Lease_c_adjusted, rate_deductions_Lease_c) {
            tax_base_Lease_c <- g_Lease_c_adjusted - (g_Lease_c_adjusted*rate_deductions_Lease_c)
            return(tax_base_Lease_c)
          })
          
          
          
          # 4. Solid waste -------------------------------------------------------------
          vec_cal_tax_base_SolidWaste_c_fun <- Vectorize(function(g_SolidWaste_c_adjusted, rate_deductions_SolidWaste_c) {
            tax_base_SolidWaste_c <- g_SolidWaste_c_adjusted - (g_SolidWaste_c_adjusted*rate_deductions_SolidWaste_c)
            return(tax_base_SolidWaste_c)
          })
          
          
          
# 6. Capital gains ------------------------------------------------------

vec_cal_tax_base_CapGainsSaleShareCapital_c_fun <- Vectorize(function(g_CapitalGainsSaleShareCapital_c_adjusted, rate_deductions_CapitalGainsSaleShareCapital_c) {
  tax_base_CapitalGainsSaleShareCapital_c <- g_CapitalGainsSaleShareCapital_c_adjusted - (g_CapitalGainsSaleShareCapital_c_adjusted*rate_deductions_CapitalGainsSaleShareCapital_c)
  return(tax_base_CapitalGainsSaleShareCapital_c)
})




          # 7. Total tax base based on capital income-----------------------------------------
          # 7.1 Calculation for total tax base from capital income OTHER THAN games of chance------------------------------
          vec_cal_tti_c_a_fun <- Vectorize(function(g_IndustrialPropertyRightsSuccessor_c,tax_base_Lease_c,tax_base_SolidWaste_c,
                                                    g_OtherIncome_c, tax_base_IndustrialPropertyRights_c, 
                                                    tax_base_CapitalGainsSaleShareCapital_c
                                                    
          ) {
            tti_c_a <- g_IndustrialPropertyRightsSuccessor_c+tax_base_Lease_c+tax_base_SolidWaste_c+
              g_OtherIncome_c+ tax_base_IndustrialPropertyRights_c+ tax_base_SolidWaste_c+
              tax_base_CapitalGainsSaleShareCapital_c
            tti_c_a <- max(tti_c_a, 0)
            return(tti_c_a)
          })
          
          
          # 7.5 Total PIT on the base of income from capital  ---------------------------------------------------------------------
          vec_cal_pit_c_fun <- Vectorize(function(capital_income_rate_a, tti_c_a ) {
            pit_c <- (tti_c_a * capital_income_rate_a) # + (tti_c_g * capital_income_rate_g) #+ (tti_c_c * capital_income_rate_c)
            return(pit_c)
          })
          
# III. ESTIMATION TOTAL GROSS AND NET INCOME AND PIT --------------------------------

        # 5.Total PIT ------------------------------
vec_cal_total_pit_fun <- Vectorize(function(pit_w,pit_c) {
  # pitax <- pit_w + pit_c
  pitax <- pit_w+ pit_c
  return(pitax)
})





# Setup parallel backend to use multiple processors
cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(data.table)
  library(dplyr)
})
# Define functions in cluster
clusterExport(cl, c("pit_data", "scenarios", "base_year", "SimulationYear", "get_sim_dataset", 
                    "pit_simulation_parameters_raw", "pit_simulation_parameters_updated",
                    "cal_weighting_fun",
                    "SSC_rate", "max_base_wage", "max_ssc", "PersonalAllowance", 
                    "vec_cal_ssc_w_fun", 
                    # Labor
                    "vec_cal_tax_base_w_fun",
                    "vec_cal_pit_w_fun",
                    "vec_cal_tax_base_agr_fun",
                    "vec_cal_tax_base_other_fun",
                    # Capital
                    "vec_cal_tax_base_IndPropRights_c_fun",
                    "vec_cal_tax_base_Lease_c_fun",
                    "vec_cal_tax_base_SolidWaste_c_fun",
                    "vec_cal_tax_base_CapGainsSaleShareCapital_c_fun",
                    "vec_cal_tti_c_a_fun",
                    "vec_cal_pit_c_fun",
                    "vec_cal_total_pit_fun",
                    "filter_columns"
))

# Parallel computation for each scenario
parLapply(cl, seq_along(scenarios), function(scenario_index) {
  scenario <- scenarios[scenario_index]
  current_year <- base_year + (scenario_index - 1)
  weights <- pit_data$weights[[scenario]]
  growth_factors <- pit_data$growth_factors[Year == current_year & scenarios == scenario, g_Wages_l]
  simulation_parameters_raw <- pit_simulation_parameters_raw
  parameters_raw <- setNames(simulation_parameters_raw$Value, simulation_parameters_raw$Parameters)
  # Determine the parameters to use for PIT_SIM based on the current year and simulation year
  selected_parameters <- get_sim_dataset(current_year, SimulationYear, base_year)
  parameters_updated <- setNames(selected_parameters$Value, selected_parameters$Parameters)
  columns_to_process <- names(pit_data$sample)[grepl("^g_", names(pit_data$sample))]
  # Process PIT_BU ------------
  PIT_weighted <- cal_weighting_fun(pit_data$sample, weights, growth_factors, columns_to_process)
  PIT_BU<-PIT_weighted
  PIT_BU[, calc_ssc := vec_cal_ssc_w_fun(total_ssc, g_Wages_l_adjusted, parameters_raw["SSC_rate"], max_base_wage, max_ssc)]
  # Labor
  PIT_BU[, tax_base_w := vec_cal_tax_base_w_fun(g_Wages_l_adjusted,calc_ssc, g_total_personal_allowance_l_adjusted, parameters_raw["weight_personal_allowance_w"], g_d_total_tax_reduction_l, parameters_raw["tax_credit_donation"])] 
  
  # Donation test
  PIT_BU[, pit_tax_donation := ifelse(g_d_total_tax_reduction_l > 0, # Calculate tax base wages1 (applies when g_d_total_tax_reduction_l > 0)
                                      g_Wages_l_adjusted - calc_ssc - g_total_personal_allowance_l_adjusted - parameters_raw["tax_credit_donation"]/0.1 ,
                                      0)*parameters_raw["rate1"]] #NEW
  
  # Income on the basis of sale of own agricultural products 
  PIT_BU[, tax_base_agr := vec_cal_tax_base_agr_fun(g_AgriculturalProductsOwn_l_adjusted, parameters_raw["rate_ded_income_agr_l"])]
  PIT_BU[, pit_tax_agr := tax_base_agr*parameters_raw["rate1"]] #NEW
  
  # Sum of tax bases
  PIT_BU[, tax_base_other := vec_cal_tax_base_other_fun(tax_base_agr,  g_TemporaryContracts_l_adjusted )]
  PIT_BU[, pit_w := vec_cal_pit_w_fun(tax_base_w,tax_base_other, parameters_raw["rate1"], parameters_raw["rate2"], parameters_raw["rate3"], parameters_raw["rate4"], parameters_raw["tbrk1"], parameters_raw["tbrk2"], parameters_raw["tbrk3"])]
  
  # Capital
  PIT_BU[, tax_base_IndustrialPropertyRights_c := vec_cal_tax_base_IndPropRights_c_fun(g_IndustrialPropertyRights_c_adjusted, parameters_raw["rate_deductions_IndustrialPropertyRights_c"])]
  PIT_BU[, pit_tax_IndustrialPropertyRights_c := tax_base_IndustrialPropertyRights_c*parameters_raw["capital_income_rate_a"]] #NEW
  
  PIT_BU[, tax_base_Lease_c := vec_cal_tax_base_Lease_c_fun(g_Lease_c_adjusted, parameters_updated["rate_deductions_Lease_c"])]
  PIT_BU[, pit_tax_Lease_c := tax_base_Lease_c*parameters_raw["capital_income_rate_a"]] #NEW
  
  PIT_BU[, tax_base_SolidWaste_c := vec_cal_tax_base_SolidWaste_c_fun(g_SolidWaste_c_adjusted,  parameters_raw["rate_deductions_SolidWaste_c"])]
  PIT_BU[, pit_SolidWaste_c := tax_base_SolidWaste_c*parameters_raw["capital_income_rate_a"]] #NEW
  
  
  PIT_BU[, tax_base_CapitalGainsSaleShareCapital_c := vec_cal_tax_base_CapGainsSaleShareCapital_c_fun(g_CapitalGainsSaleShareCapital_c_adjusted, parameters_raw["rate_deductions_CapitalGainsSaleShareCapital_c"])]
  PIT_BU[, pit_CapitalGainsSaleShareCapital_c := tax_base_CapitalGainsSaleShareCapital_c*parameters_raw["capital_income_rate_a"]] #NEW
  
  PIT_BU[, tti_c_a := vec_cal_tti_c_a_fun(g_IndustrialPropertyRightsSuccessor_c,tax_base_Lease_c,tax_base_SolidWaste_c,
                                          g_OtherIncome_c, tax_base_IndustrialPropertyRights_c, 
                                          tax_base_CapitalGainsSaleShareCapital_c)]
  
  PIT_BU[, pit_c := vec_cal_pit_c_fun(parameters_raw["capital_income_rate_a"], tti_c_a)]
  PIT_BU[, calc_pitax := vec_cal_total_pit_fun(pit_w,pit_c)]
  PIT_BU <- filter_columns(PIT_BU)
  
  # Process PIT_SIM ------------------------------------------
  PIT_SIM<-PIT_weighted
  PIT_SIM[, calc_ssc := vec_cal_ssc_w_fun(total_ssc, g_Wages_l_adjusted, parameters_updated["SSC_rate"], max_base_wage, max_ssc)]
  # Labor
  PIT_SIM[, tax_base_w := vec_cal_tax_base_w_fun(g_Wages_l_adjusted,calc_ssc, g_total_personal_allowance_l_adjusted, parameters_updated["weight_personal_allowance_w"], g_d_total_tax_reduction_l, parameters_updated["tax_credit_donation"])] 
  
  # Donation test
  PIT_SIM[, pit_tax_donation := ifelse(g_d_total_tax_reduction_l > 0, # Calculate tax base wages1 (applies when g_d_total_tax_reduction_l > 0)
                                       g_Wages_l_adjusted - calc_ssc - g_total_personal_allowance_l_adjusted - parameters_updated["tax_credit_donation"]/0.1 ,
                                       0)*parameters_updated["rate1"]] #NEW
  
  
  
  # Income on the basis of sale of own agricultural products 
  PIT_SIM[, tax_base_agr := vec_cal_tax_base_agr_fun(g_AgriculturalProductsOwn_l_adjusted, parameters_updated["rate_ded_income_agr_l"])]
  PIT_SIM[, pit_tax_agr := tax_base_agr*parameters_updated["rate1"]] #NEW
  
  # Sum of tax bases
  PIT_SIM[, tax_base_other := vec_cal_tax_base_other_fun(tax_base_agr,  g_TemporaryContracts_l_adjusted )]
  
  PIT_SIM[, pit_w := vec_cal_pit_w_fun(tax_base_w,tax_base_other, parameters_updated["rate1"], parameters_updated["rate2"], parameters_updated["rate3"], parameters_updated["rate4"], parameters_updated["tbrk1"], parameters_updated["tbrk2"], parameters_updated["tbrk3"])]
  
  # Capital
  PIT_SIM[, tax_base_IndustrialPropertyRights_c := vec_cal_tax_base_IndPropRights_c_fun(g_IndustrialPropertyRights_c_adjusted, parameters_updated["rate_deductions_IndustrialPropertyRights_c"])]
  PIT_SIM[, pit_tax_IndustrialPropertyRights_c := tax_base_IndustrialPropertyRights_c*parameters_updated["capital_income_rate_a"]] #NEW
  
  PIT_SIM[, tax_base_Lease_c := vec_cal_tax_base_Lease_c_fun(g_Lease_c_adjusted, parameters_updated["rate_deductions_Lease_c"])]
  PIT_SIM[, pit_tax_Lease_c := tax_base_Lease_c*parameters_updated["capital_income_rate_a"]] #NEW
  
  
  PIT_SIM[, tax_base_SolidWaste_c := vec_cal_tax_base_SolidWaste_c_fun(g_SolidWaste_c_adjusted,  parameters_updated["rate_deductions_SolidWaste_c"])]
  PIT_SIM[, pit_SolidWaste_c := tax_base_SolidWaste_c*parameters_updated["capital_income_rate_a"]] #NEW
  
  
  PIT_SIM[, tax_base_CapitalGainsSaleShareCapital_c := vec_cal_tax_base_CapGainsSaleShareCapital_c_fun(g_CapitalGainsSaleShareCapital_c_adjusted, parameters_updated["rate_deductions_CapitalGainsSaleShareCapital_c"])]
  PIT_SIM[, pit_CapitalGainsSaleShareCapital_c := tax_base_CapitalGainsSaleShareCapital_c*parameters_updated["capital_income_rate_a"]] #NEW
  
  
  PIT_SIM[, tti_c_a := vec_cal_tti_c_a_fun(g_IndustrialPropertyRightsSuccessor_c,tax_base_Lease_c,tax_base_SolidWaste_c,
                                           g_OtherIncome_c, tax_base_IndustrialPropertyRights_c,
                                           tax_base_CapitalGainsSaleShareCapital_c)]
  
  
  PIT_SIM[, pit_c := vec_cal_pit_c_fun(parameters_updated["capital_income_rate_a"], tti_c_a)]
  PIT_SIM[, calc_pitax := vec_cal_total_pit_fun(pit_w,pit_c)]
  PIT_SIM <- filter_columns(PIT_SIM)
  
  list(PIT_BU = PIT_BU, PIT_SIM = PIT_SIM, scenario = scenario)
}) -> results

# Stop the cluster
stopCluster(cl)

# Aggregation of results in tables
for (res in results) {
  PIT_BU_list[[res$scenario]] <- res$PIT_BU
  PIT_SIM_list[[res$scenario]] <- res$PIT_SIM
}

# Function to sum the specified columns in the list and store the results in a data frame
summarize_PIT <- function(PIT_list, suffix) {
  result <- data.frame(
    scenarios = character(),
    stringsAsFactors = FALSE
  )
  
  for (scenario in names(PIT_list)) {
    df <- PIT_list[[scenario]]
    calc_columns <- names(df)[grepl("^(calc|pit)", names(df))]
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

summary_SIM <- summarize_PIT(PIT_SIM_list, "_sim")
summary_BU <- summarize_PIT(PIT_BU_list, "_bu")

merged_PIT_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
merged_PIT_BU_SIM$year <- as.character(forecast_horizon)
merged_PIT_BU_SIM <- merged_PIT_BU_SIM[, c("year", names(merged_PIT_BU_SIM)[-length(merged_PIT_BU_SIM)])]

numeric_columns <- sapply(merged_PIT_BU_SIM, is.numeric)
merged_PIT_BU_SIM[, numeric_columns] <- merged_PIT_BU_SIM[, numeric_columns] / 1e06

print(merged_PIT_BU_SIM)
rm(results,res)



