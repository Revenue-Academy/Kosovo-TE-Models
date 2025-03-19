" Data prep Distribution Dashboard "

# I. Labor-capital -----------------------------------------------------

                  columns_gross_income <- c( 
                                                "gross_wage" , 
                                                "net_income_business" , 
                                                "net_income_partnership" ,
                                                "gross_rents" , 
                                                "gross_i_interest_pen_pay" , 
                                                "gross_i_interest" ,
                                                "gross_i_inta_prop" , 
                                                "capital_gain" , 
                                                "foreign_s_inc" ,
                                                "other_inc_gifts",
                                                "decile_group",
                                                "total_inc_adjusted"

                                              )

                  # Select columns 
                  selected_gross_desc_tbl <- select(PIT_BU_list$t0, all_of(columns_gross_income))

              
                  selected_gross_desc_tbl <- selected_gross_desc_tbl %>%
                    rowwise() %>%
                    mutate(net_income_business_sum = sum(ifelse(net_income_business < 0, 0, net_income_business)))
                  
                  
                  labor_capital <- selected_gross_desc_tbl %>%
                    dplyr::group_by(decile_group) %>%
                    dplyr::select(-c(total_inc_adjusted))%>%
                    dplyr::summarize(across(c(gross_wage, net_income_business_sum, net_income_partnership, gross_rents, gross_i_interest_pen_pay,
                                              gross_i_interest, gross_i_inta_prop, capital_gain, foreign_s_inc, other_inc_gifts), 
                                            sum))
                  

                  #       # Reshape the data into long format
                        labor_capital_type <- labor_capital %>%
                          gather(key = "gross_income", value = "value",  gross_wage, net_income_business_sum, net_income_partnership, gross_rents, gross_i_interest_pen_pay,gross_i_interest, gross_i_inta_prop, capital_gain, foreign_s_inc, other_inc_gifts)

                        # Reverse the order of the factors for 'gross_income'
                       # labor_capital_type$gross_income <- factor(labor_capital_type$gross_income, levels = c("labor_wages","labor_temporary_contract","labor_agricultural","capital_dividends_profits","capital_property_income","capital_other"))


# II. Type of Income ---------------------------------------------------------------------

                        gross_income_BU_SIM<-merged_PIT_BU_SIM%>%
                          dplyr::select(year,calc_total_inc_bu,calc_total_inc_sim)
                        
                        
                        
                        


# III. Treemap GROSS INCOME --------------------------------------------------------------------


                        
                        
                        long_labor_capital_type <- labor_capital %>%
                          dplyr::select(-c(decile_group)) %>%
                          dplyr::summarise(gross_wage  = sum(gross_wage , na.rm = TRUE),
                                           net_income_business_sum = sum(net_income_business_sum, na.rm = TRUE),
                                           net_income_partnership = sum(net_income_partnership, na.rm = TRUE),
                                           gross_rents = sum(gross_rents, na.rm = TRUE),
                                           gross_i_interest_pen_pay = sum(gross_i_interest_pen_pay, na.rm = TRUE),
                                           gross_i_interest = sum(gross_i_interest, na.rm = TRUE),
                                           gross_i_inta_prop = sum(gross_i_inta_prop, na.rm = TRUE),
                                           capital_gain = sum(capital_gain, na.rm = TRUE),
                                           foreign_s_inc = sum(foreign_s_inc, na.rm = TRUE),
                                           other_inc_gifts = sum(other_inc_gifts, na.rm = TRUE)

                          )

                  

# Convert the data to long format
long_labor_capital_type <- long_labor_capital_type %>%
  gather(key = "income_type", value = "value")


long_labor_capital_type$income_type<-factor( long_labor_capital_type$income_type)



#long_labor_capital_type$TypeOfIncome <- "In million LCU"
long_labor_capital_type$TypeOfIncome <- "In LCU"



# IV. Structure of gross income by NACE sections ---------------------------------------------------------------------

columns_gross_nace_income <- c(
                              "section",
                              #"g_total_gross"
                              "total_inc_adjusted"
                            )


# Create a select statement with all the patterns

selected_gross_nace_tbl <- select(PIT_BU_list$t0, all_of(columns_gross_nace_income))


gross_nace_tbl <- selected_gross_nace_tbl %>%
  dplyr::group_by(section) %>%
  #dplyr::summarise(g_total_gross = sum(g_total_gross, na.rm = TRUE)
  dplyr::summarise(total_inc = sum(total_inc_adjusted, na.rm = TRUE)
  )

# # Convert the data to long format
gross_nace_tbl <- na.omit(gross_nace_tbl)
gross_nace_tbl<-left_join(gross_nace_tbl,df_nace_names,by=c("section"="section"))
gross_nace_tbl$nace_section<-factor(gross_nace_tbl$section)
#gross_nace_tbl$TypeOfIncome <- "In billion LCU"

gross_nace_tbl$TypeOfIncome <- "In LCU"

