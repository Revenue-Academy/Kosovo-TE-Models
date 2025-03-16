
  # 1. Functions for calculation -----------------------------------------------

              # 1. Define the function to extract and filter columns for the given simulation year ----------------------------------
                        extract_filtered_re_df_fun <- function(CIT_BU_list, forecast_horizon, SimulationYear) {
                                                    # Validate inputs
                                                    if (!SimulationYear %in% forecast_horizon) {
                                                      stop("The specified simulation year is not in the forecast horizons.")
                                                    }
                                                    
                                                    # Find the index of the dataset that corresponds to the SimulationYear
                                                    index <- which(forecast_horizon == SimulationYear)
                                                    
                                                    # Extract the specific data frame for the SimulationYear
                                                    PIT_BU_simulation_year_df <- CIT_BU_list[[index]]
                                                    
                                                    # Define the columns to keep
                                                    #columns_to_keep <- c("id_n", "total_inc", "calc_pitax", "tti_c_c","tti_w_I")
                                                    columns_to_keep <- c("id_n",  "weight",
                                                                         "total_inc", "g_total_net_l",
                                                                         #"total_inc", "calc_pitax", "tax_base_other","tti_c_a","tti_c_g")
                                                                         "total_inc", "calc_pitax", "tax_base_other","tti_c_a")
                                                    
                                                    # Check if all columns exist in the data frame
                                                    missing_columns <- setdiff(columns_to_keep, colnames(PIT_BU_simulation_year_df))
                                                    if (length(missing_columns) > 0) {
                                                      warning("The following columns are missing in the data frame: ", paste(missing_columns, collapse = ", "))
                                                    }
                                                    
                                                    # Filter the data frame to keep only the specified columns
                                                    PIT_BU_simulation_year_df <- PIT_BU_simulation_year_df[, ..columns_to_keep, with = FALSE]
                                                    
                                                    return(PIT_BU_simulation_year_df)
                                                  }
              # 2. Function to calculate Gini coefficients for pre-tax and after-tax income -----
                        # weighted_gini_fun <- function(g_total_net_l, total_inc, w) {
                        #   order_index <- order(total_inc)
                        #   g_total_net_l <- g_total_net_l[order_index]
                        #   total_inc <- total_inc[order_index]
                        #   w <- w[order_index]
                        #   
                        #   cum_w <- cumsum(w) / sum(w)
                        #   cum_gross <- cumsum(total_inc * w) / sum(total_inc * w)
                        #   cum_net <- cumsum(g_total_net_l * w) / sum(g_total_net_l * w)
                        #   
                        #   area_lx <- sum(cum_gross[-1] * diff(cum_w))
                        #   area_lx_t <- sum(cum_net[-1] * diff(cum_w))
                        #   
                        #   gini_gx <- 1 - 2 * area_lx
                        #   gini_gx_t <- 1 - 2 * area_lx_t
                        #   
                        #   return(list(gini_gx = gini_gx, gini_gx_t = gini_gx_t))
                        # }
                        weighted_gini_fun <- function(g_total_net_l, total_inc, w) {
                          order_index <- order(total_inc)
                          g_total_net_l <- g_total_net_l[order_index]
                          total_inc <- total_inc[order_index]
                          w <- w[order_index]
                          
                          # Convert to numeric to avoid integer overflow
                          cum_w <- cumsum(as.numeric(w)) / sum(as.numeric(w))
                          cum_gross <- cumsum(as.numeric(total_inc * w)) / sum(as.numeric(total_inc * w))
                          cum_net <- cumsum(as.numeric(g_total_net_l * w)) / sum(as.numeric(g_total_net_l * w))
                          
                          area_lx <- sum(cum_gross[-1] * diff(cum_w))
                          area_lx_t <- sum(cum_net[-1] * diff(cum_w))
                          
                          gini_gx <- 1 - 2 * area_lx
                          gini_gx_t <- 1 - 2 * area_lx_t
                          
                          return(list(gini_gx = gini_gx, gini_gx_t = gini_gx_t))
                        }



              # 3. Function to calculate Concentration Coefficients ------------------------------
                        # weighted_concentration_fun <- function(g_total_net_l, total_inc, w) {
                        #   order_index <- order(total_inc)
                        #   g_total_net_l <- g_total_net_l[order_index]
                        #   total_inc <- total_inc[order_index]
                        #   w <- w[order_index]
                        #   
                        #   tax <- total_inc - g_total_net_l
                        #   cum_w <- cumsum(w) / sum(w)
                        #   cum_gross <- cumsum(total_inc * w) / sum(total_inc * w)
                        #   cum_net <- cumsum(g_total_net_l * w) / sum(g_total_net_l * w)
                        #   cum_tax <- cumsum(tax * w) / sum(tax * w)
                        #   
                        #   area_lx_t <- sum(cum_net[-1] * diff(cum_w))
                        #   area_lt <- sum(cum_tax[-1] * diff(cum_w))
                        #   
                        #   concentration_cx_t <- 1 - 2 * area_lx_t
                        #   concentration_ct <- 1 - 2 * area_lt
                        #   
                        #   return(list(concentration_cx_t = concentration_cx_t, concentration_ct = concentration_ct))
                        # }
                        
                        weighted_concentration_fun <- function(g_total_net_l, total_inc, w) {
                          order_index <- order(total_inc)
                          g_total_net_l <- g_total_net_l[order_index]
                          total_inc <- total_inc[order_index]
                          w <- w[order_index]
                          
                          tax <- total_inc - g_total_net_l
                          
                          # Convert to numeric to avoid integer overflow
                          cum_w <- cumsum(as.numeric(w)) / sum(as.numeric(w))
                          cum_gross <- cumsum(as.numeric(total_inc * w)) / sum(as.numeric(total_inc * w))
                          cum_net <- cumsum(as.numeric(g_total_net_l * w)) / sum(as.numeric(g_total_net_l * w))
                          cum_tax <- cumsum(as.numeric(tax * w)) / sum(as.numeric(tax * w))
                          
                          area_lx_t <- sum(cum_net[-1] * diff(cum_w))
                          area_lt <- sum(cum_tax[-1] * diff(cum_w))
                          
                          concentration_cx_t <- 1 - 2 * area_lx_t
                          concentration_ct <- 1 - 2 * area_lt
                          
                          return(list(concentration_cx_t = concentration_cx_t, concentration_ct = concentration_ct))
                        }
                        
                        
              # 4. Function to calculate the Atkinson Index ------------------------------
                        weighted_atkinson_fun <- function(x, w, epsilon = 0.5) {
                          weighted_mean <- sum(x * w) / sum(w)
                          
                          if (epsilon == 0) {
                            x_equiv <- weighted_mean
                          } else if (epsilon == 1) {
                            x_equiv <- exp(sum(w * log(x)) / sum(w))
                          } else if (epsilon == 2) {
                            x_equiv <- sum(w) / sum(w / x)
                          } else {
                            x_equiv <- (sum(w * x^(1 - epsilon)) / sum(w))^(1 / (1 - epsilon))
                          }
                          
                          atkinson_index <- 1 - (x_equiv / weighted_mean)
                          return(atkinson_index)
                        }
              
              # 5. Generalized Entropy Functions ----------------------------------
                        entropy_measure_fun <- function(income, theta, weights = NULL) {
                          if (is.null(weights)) {
                            mu <- mean(income)
                            n <- length(income)
                            weighted_sum <- sum((income / mu)^theta)
                          } else {
                            mu <- sum(income * weights) / sum(weights)
                            n <- sum(weights)
                            weighted_sum <- sum(weights * (income / mu)^theta)
                          }
                          
                          if (theta == 0) {
                            E_theta <- sum(weights * log(mu / income)) / sum(weights)
                          } else if (theta == 1) {
                            E_theta <- sum(weights * (income / mu) * log(income / mu)) / sum(weights)
                          } else {
                            E_theta <- (1 / (theta * (theta - 1))) * (weighted_sum / n - 1)
                          }
                          
                          return(E_theta)
                        }
                        
              # 6. Squared variation  -----------------------------------------------
              
                        coefficient_of_squared_variation_fun <- function(income, weights = NULL) {
                          I2 <- 2 * entropy_measure_fun(income, theta = 2, weights)
                          return(I2)
                        }
                        
              # 7. Mean logarithmic ------------------------------------------------------
              
                        mean_logarithmic_deviation_fun <- function(income, weights = NULL) {
                          I0 <- entropy_measure_fun(income, theta = 0, weights)
                          return(I0)
                        }
              
              # 8. Function to calculate the Kakwani Index ------------------------------------------
                        # calculate_kakwani_index_fun <- function(calc_pitax, total_inc, w) {
                        #   order_index <- order(total_inc)
                        #   calc_pitax <- calc_pitax[order_index]
                        #   total_inc <- total_inc[order_index]
                        #   w <- w[order_index]
                        #   
                        #   tax <- total_inc - calc_pitax
                        #   cum_w <- cumsum(w) / sum(w)
                        #   cum_gross <- cumsum(total_inc * w) / sum(total_inc * w)
                        #   cum_tax <- cumsum(tax * w) / sum(tax * w)
                        #   
                        #   area_lx <- sum(cum_gross[-1] * diff(cum_w))
                        #   area_lt <- sum(cum_tax[-1] * diff(cum_w))
                        #   
                        #   gini_gx <- 1 - 2 * area_lx
                        #   concentration_ct <- 1 - 2 * area_lt
                        #   
                        #   kakwani_index <- concentration_ct - gini_gx
                        #   return(kakwani_index)
                        # }
                        
                    calculate_kakwani_index_fun <- function(calc_pitax, total_inc, w) {
                      order_index <- order(total_inc)
                      calc_pitax <- calc_pitax[order_index]
                      total_inc <- total_inc[order_index]
                      w <- w[order_index]
                      
                      tax <- total_inc - calc_pitax
                      
                      # Convert to numeric to avoid integer overflow
                      cum_w <- cumsum(as.numeric(w)) / sum(as.numeric(w))
                      cum_gross <- cumsum(as.numeric(total_inc * w)) / sum(as.numeric(total_inc * w))
                      cum_tax <- cumsum(as.numeric(tax * w)) / sum(as.numeric(tax * w))
                      
                      area_lx <- sum(cum_gross[-1] * diff(cum_w))
                      area_lt <- sum(cum_tax[-1] * diff(cum_w))
                      
                      gini_gx <- 1 - 2 * area_lx
                      concentration_ct <- 1 - 2 * area_lt
                      
                      kakwani_index <- concentration_ct - gini_gx
                      return(kakwani_index)
                    }

              
              # 9. Function to calculate the Average tax rate ------------------------------------
              
                        average_tax_rate_fun <- function(pre_tax_income, after_tax_income) {
                                              Q <- sum(pre_tax_income - after_tax_income)  # Total tax revenue
                                              m <- sum(pre_tax_income)  # Total pre-tax income
                                              t <- Q / m  # Average tax rate
                                              return(t)
                        }
                        
              
              # 10.Function to calculate the Re distributive Effect (R), H, and V -------------------------------------
              
                        calculate_redistributive_effect_fun <- function(concentration_cx_t, gini_gx_t, gini_gx, kakwani_index, t) {
                                                H <- (concentration_cx_t - gini_gx_t) / gini_gx
                                                V <- (t * kakwani_index) / ((1 - t) * gini_gx)
                                                R <- H + V
                                                t<-t
                                                return(list(H = H, V = V, R = R,t=t))
                        }
                        
              
          
    # 2. BU -------------------------------------------------------------------------
              PIT_BU_simulation_year_df <- extract_filtered_re_df_fun(CIT_BU_list, forecast_horizon, SimulationYear)
              
              average_tax_rate_bu <- sum(PIT_BU_simulation_year_df$calc_pitax) / sum(PIT_BU_simulation_year_df$total_inc)
              
              # Calculate all indicators and store in a table
              indicator_table_bu <- data.frame(
                            Indicator = c(
                                          "Gini coefficient for pre-tax income",
                                          "Gini coefficient for after-tax income",
                                          "Concentration coefficient for after-tax income w.r.t. pre-tax income",
                                          "Concentration coefficient for tax w.r.t. pre-tax income",
                                          "Atkinson Index for pre-tax income",
                                          "Atkinson Index for after-tax income",
                                          "Coefficient of squared variation (I2)",
                                          "Mean logarithmic deviation (I0)",
                                          "Kakwani Index"),
                               Name = c(
                                        "gini_gx",
                                        "gini_gx_t",
                                        "concentration_cx_t",
                                        "concentration_ct",
                                        "atkinson_index_pre_tax_income",
                                        "atkinson_index_after_tax_income",
                                        "I2",
                                        "I0",
                                        "kakwani_index"),
                              Value = c(
                                          weighted_gini_fun(PIT_BU_simulation_year_df$g_total_net_l, PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight)$gini_gx,
                                          weighted_gini_fun(PIT_BU_simulation_year_df$g_total_net_l, PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight)$gini_gx_t,
                                          weighted_concentration_fun(PIT_BU_simulation_year_df$g_total_net_l, PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight)$concentration_cx_t,
                                          weighted_concentration_fun(PIT_BU_simulation_year_df$g_total_net_l, PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight)$concentration_ct,
                                          weighted_atkinson_fun(PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight, epsilon = 0.5),
                                          weighted_atkinson_fun(PIT_BU_simulation_year_df$g_total_net_l, PIT_BU_simulation_year_df$weight, epsilon = 0.5),
                                          coefficient_of_squared_variation_fun(PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight),
                                          mean_logarithmic_deviation_fun(PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight),
                                          calculate_kakwani_index_fun(PIT_BU_simulation_year_df$calc_pitax, PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight)
                            )
                          )
    
              
              # Calculate the required indicators
              concentration_cx_t <- weighted_concentration_fun(PIT_BU_simulation_year_df$g_total_net_l, PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight)$concentration_cx_t
              gini_gx_t <- weighted_gini_fun(PIT_BU_simulation_year_df$g_total_net_l, PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight)$gini_gx_t
              gini_gx <- weighted_gini_fun(PIT_BU_simulation_year_df$g_total_net_l, PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight)$gini_gx
              kakwani_index <- calculate_kakwani_index_fun(PIT_BU_simulation_year_df$calc_pitax, PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$weight)
              t <- average_tax_rate_fun(PIT_BU_simulation_year_df$total_inc, PIT_BU_simulation_year_df$calc_pitax)
              
              # Calculate H, V, and R
              redistributive_effects <- calculate_redistributive_effect_fun(concentration_cx_t, gini_gx_t, gini_gx, kakwani_index, t)
              
              # Add the results to the indicator table
              indicator_table_bu <- rbind(indicator_table_bu, data.frame(
                Indicator = c("Horizontal Inequality (H)", "Vertical Inequality (V)", "Redistributive Effect (R)","Average Tax Rate"),
                Name = c("H", "V", "R","T"),
                #Value = c(redistributive_effects$H, redistributive_effects$V, redistributive_effects$R)
                Value = c(redistributive_effects$H, redistributive_effects$V, redistributive_effects$R,redistributive_effects$t)
              ))
              
              indicator_table_bu<-indicator_table_bu%>%
              dplyr::rename("Business as usual"="Value")
              
              # Print the updated table
              #print(indicator_table_bu)
              
              
    
    # 3. SIM ----------------------------------------------------------
    
                       PIT_SIM_simulation_year_df <- extract_filtered_re_df_fun(CIT_SIM_list, forecast_horizon, SimulationYear)
                       
              
              'CALC AVERAGE TAX RATE FOR INFOBOXES'
           
              average_tax_rate_sim <- sum(PIT_SIM_simulation_year_df$calc_pitax) / sum(PIT_SIM_simulation_year_df$total_inc)
              
    
              # Calculate all indicators and store in a table
              indicator_table_sim <- data.frame(
                Indicator = c(
                              "Gini coefficient for pre-tax income",
                              "Gini coefficient for after-tax income",
                              "Concentration coefficient for after-tax income w.r.t. pre-tax income",
                              "Concentration coefficient for tax w.r.t. pre-tax income",
                              "Atkinson Index for pre-tax income",
                              "Atkinson Index for after-tax income",
                              "Coefficient of squared variation (I2)",
                              "Mean logarithmic deviation (I0)",
                              "Kakwani Index"
                            ),
                Name = c(
                                "gini_gx",
                                "gini_gx_t",
                                "concentration_cx_t",
                                "concentration_ct",
                                "atkinson_index_pre_tax_income",
                                "atkinson_index_after_tax_income",
                                "I2",
                                "I0",
                                "kakwani_index"
                              ),
                Value = c(
                          weighted_gini_fun(PIT_SIM_simulation_year_df$g_total_net_l, PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight)$gini_gx,
                          weighted_gini_fun(PIT_SIM_simulation_year_df$g_total_net_l, PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight)$gini_gx_t,
                          weighted_concentration_fun(PIT_SIM_simulation_year_df$g_total_net_l, PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight)$concentration_cx_t,
                          weighted_concentration_fun(PIT_SIM_simulation_year_df$g_total_net_l, PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight)$concentration_ct,
                          weighted_atkinson_fun(PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight, epsilon = 0.5),
                          weighted_atkinson_fun(PIT_SIM_simulation_year_df$g_total_net_l, PIT_SIM_simulation_year_df$weight, epsilon = 0.5),
                          coefficient_of_squared_variation_fun(PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight),
                          mean_logarithmic_deviation_fun(PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight),
                          calculate_kakwani_index_fun(PIT_SIM_simulation_year_df$calc_pitax, PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight)
                )
              )
              
    
              # Calculate the required indicators
              concentration_cx_t <- weighted_concentration_fun(PIT_SIM_simulation_year_df$g_total_net_l, PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight)$concentration_cx_t
              gini_gx_t <- weighted_gini_fun(PIT_SIM_simulation_year_df$g_total_net_l, PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight)$gini_gx_t
              gini_gx <- weighted_gini_fun(PIT_SIM_simulation_year_df$g_total_net_l, PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight)$gini_gx
              kakwani_index <- calculate_kakwani_index_fun(PIT_SIM_simulation_year_df$calc_pitax, PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$weight)
              t <- average_tax_rate_fun(PIT_SIM_simulation_year_df$total_inc, PIT_SIM_simulation_year_df$calc_pitax)
              
              # Calculate H, V, and R
              redistributive_effects <- calculate_redistributive_effect_fun(concentration_cx_t, gini_gx_t, gini_gx, kakwani_index, t)
              
              # Add the results to the indicator table
              indicator_table_sim <- rbind(indicator_table_sim, data.frame(
                Indicator = c("Horizontal Inequality (H)", "Vertical Inequality (V)", "Redistributive Effect (R)","Average Tax Rate"),
                Name = c("H", "V", "R","T"),
                Value = c(redistributive_effects$H, redistributive_effects$V, redistributive_effects$R, redistributive_effects$t)
              ))
              
              indicator_table_sim<-indicator_table_sim%>%
                dplyr::rename("Simulation"="Value")
              
              # Print the updated table
             # print(indicator_table_sim)          
              
              
    # 4. Merge the two data frames --------------------------------------------------------
                
              
              re_effects_final <- merge(indicator_table_bu, indicator_table_sim[, c("Indicator", "Name", "Simulation")], 
                                    by = c("Indicator", "Name"), 
                                    suffixes = c("_bu", "_sim"))
              
              re_effects_final$`Business as usual`<-round(re_effects_final$`Business as usual`,4)
              re_effects_final$Simulation<-round(re_effects_final$Simulation,4)
              
              re_effects_final$Name<-NULL
              
              
              re_effects_final <- re_effects_final %>%
                dplyr::filter(Indicator != "Mean logarithmic deviation (I0)" & 
                                Indicator != "Average Tax Rate") %>% # Calculation of average tax rate is need to be double check due to strange results
                dplyr::mutate(`Percentage Difference (%)` = round((`Simulation` - `Business as usual`) / `Business as usual` * 100, 2))
              
              
              re_effects_final$`Percentage Difference (%)`<-NULL
              
              ## 
              
          
              rm(CIT_BU_list,CIT_SIM_list,PIT_BU_simulation_year_df,
                combined_data_bu,combined_data_sim,combined_dt,combined_dt_bins_fun,
                 PIT_SIM_simulation_year_df,selected_gross_desc_tbl,selected_gross_nace_tbl,types_labor_capital_tbl,df)
              
    