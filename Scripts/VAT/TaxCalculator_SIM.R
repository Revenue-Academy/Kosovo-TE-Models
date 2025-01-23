suppressMessages({
  
 # 1.1 Pre-processing CPA and setting parameters for simulation ------------------------------------------
            # 1.1.0 Estimation of Calibration factor -----------------------------------------------------
            '
                                            Warning
                                            This part begins with copy-paste of the parameters entered previously and from this point begins with their new calculation with the aim of obtaining TE
                                            '
  
             # Setting zero's for toggles
            CPA_TAXABLE_PROPORTIONS_SIM[45,7]<-0
            CPA_TAXABLE_PROPORTIONS_SIM[45,6]<-0
  
            SIM_SCENARIO_EST_CAL_FACTOR_SIM<-CPA_TAXABLE_PROPORTIONS_SIM

        # new 7/12/2024
        SIM_SCENARIO_EST_CAL_FACTOR_SIM <- SIM_SCENARIO_EST_CAL_FACTOR_SIM %>%
          dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(ProportionExempted), Current_Policy_Exempt, ProportionExempted),
                        Simulated_Policy_Reduced_Rate = ifelse(is.na(ProportionPreferentialRate1), Current_Policy_Reduced_Rate, ProportionPreferentialRate1),
                        Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
            
            
            # 1.1.2 Estimation of TE part -----------------------------------------------------
            '
                                            Warning
                                            This part begins with copy-paste of the parameters entered previously and from this point begins with their new calculation with the aim of obtaining TE
                                            '
            
            #NEW

            SIM_SCENARIO_EST_TE <-CPA_TAXABLE_PROPORTIONS_SIM
            
            SIM_SCENARIO_EST_TE$ProportionPreferentialRate1<-TE_EXEMPT
            SIM_SCENARIO_EST_TE$ProportionPreferentialRate2<-TE_EXEMPT
            SIM_SCENARIO_EST_TE$Simulated_Policy_Exempt<-TE_REDUCED_RATE
            

            # new 7/12/2024
            SIM_SCENARIO_EST_TE <- SIM_SCENARIO_EST_TE %>%
              dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(ProportionExempted), Current_Policy_Exempt, ProportionExempted),
                            #Simulated_Policy_Reduced_Rate = ifelse(is.na(PreferentialVATRate_1), Current_Policy_Reduced_Rate, PreferentialVATRate_1),
                            Simulated_Policy_Reduced_Rate = ifelse(is.na(ProportionPreferentialRate1), Current_Policy_Reduced_Rate, ProportionPreferentialRate1),
                            Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
            
       
            # manual input 9/12/2024
            
            SIM_SCENARIO_EST_TE$Simulated_Policy_Fully_Taxable<-1
            SIM_SCENARIO_EST_TE$Simulated_Policy_Reduced_Rate<-0
            SIM_SCENARIO_EST_TE$Simulated_Policy_Exempt<-0
            
            
            # 1.1.3 Estimation of effective VAT RATE ----------------------------------------------------------
   
            # new 7/12/2024
            SIM_SCENARIO_EFFECTIVE_VAT_RATE = SIM_SCENARIO_EST_CAL_FACTOR_SIM
            
           # SIMULATION_2$Simulation_Toggles_Reduced_Rate<-TE_EXEMPT
            
            
            SIM_SCENARIO_EFFECTIVE_VAT_RATE$ProportionPreferentialRate1<-TE_EXEMPT
            SIM_SCENARIO_EFFECTIVE_VAT_RATE$ProportionPreferentialRate2<-TE_EXEMPT
            
            SIM_SCENARIO_EFFECTIVE_VAT_RATE <- SIM_SCENARIO_EFFECTIVE_VAT_RATE %>%
              dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(ProportionExempted), Current_Policy_Exempt, ProportionExempted),
                            #Simulated_Policy_Reduced_Rate = ifelse(is.na(PreferentialVATRate_1), Current_Policy_Reduced_Rate, PreferentialVATRate_1),
                            Simulated_Policy_Reduced_Rate = ifelse(is.na(ProportionPreferentialRate1), Current_Policy_Reduced_Rate, ProportionPreferentialRate1),
                            Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
            
            
  # 1.2 Benchmark revenues -----

            CPA_PRODUCTS_SIM<-CPA_PRODUCTS
            
            CPA_PRODUCTS_SIM$BM_Rev <- CPA_PRODUCTS_SIM$Use_K_NetPurch %>%
              dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Final_consumption_expenditure_by_households) %>%
              dplyr::mutate(Final_Demand_HH = Final_consumption_expenditure_by_households*benchmark_tax_rate) %>%
              dplyr::select(-Final_consumption_expenditure_by_households)
            
            
            CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_HH[is.na(CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_HH)] <- 0
            CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_HH[grepl("Imputed rent", CPA_PRODUCTS_SIM$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0
            
            
            CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_NPISH <- CPA_PRODUCTS_SIM$Use_K_NetPurch$Final_consumption_expenditure_NPISH*benchmark_tax_rate
            
            
            CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_NPISH[is.na(CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_NPISH)] <- 0
            CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_NPISH[grepl("Imputed rent", CPA_PRODUCTS_SIM$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0
            
            CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_Government <- CPA_PRODUCTS_SIM$Use_K_NetPurch$Final_consumption_expenditure_by_government*benchmark_tax_rate
            
            
            
            CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_Government[is.na(CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_Government)] <- 0
            CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_Government[grepl("Imputed rent", CPA_PRODUCTS_SIM$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0
            
            
            CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_HH[grepl("Constructions and construction works", CPA_PRODUCTS_SIM$BM_Rev$PRODUCT_INDUSTRY_NAME)] = 0
            
            
            CPA_PRODUCTS_SIM$BM_Rev <- CPA_PRODUCTS_SIM$BM_Rev %>%
              dplyr::mutate(Final_Demand_Total = psum(Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government))
            
            Sum_of_Final_Demand_Total = sum(CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_Total)
            
  # 1.3 Est.Industry Share ----
            # 2.3.0 Estimation of Calibration factor ---------------------------------------------------
            
            # Copy paste table
            SUPPLY_DOM_EST_CAL_FACTOR = copy(SUPPLY_DOM)
            NACE_INDUSTRIES_EST_CAL_FACTOR = copy(NACE_INDUSTRIES)
            
            EST.IS_EST_CAL_FACTOR <- merge.data.frame(SUPPLY_DOM_EST_CAL_FACTOR, SIM_SCENARIO_EST_CAL_FACTOR_SIM, key = "PRODUCT_INDUSTRY_CODE")
            EST.IS_EST_CAL_FACTOR$value = EST.IS_EST_CAL_FACTOR$value*EST.IS_EST_CAL_FACTOR$Simulated_Policy_Exempt
            
            EST.IS_EST_CAL_FACTOR <- EST.IS_EST_CAL_FACTOR %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
            EST.IS_EST_CAL_FACTOR$value[is.na(EST.IS_EST_CAL_FACTOR$value)] <- 0
            
            NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS <- EST.IS_EST_CAL_FACTOR %>% 
              dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
              dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
              dplyr::summarise(Industry_Share = sum(value, na.rm = T))
            
            NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS <- NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS %>%
              dplyr::arrange(INDUSTRY_CODE)
            
            NACE_INDUSTRIES_EST_CAL_FACTOR$Supply_Dom <- NACE_INDUSTRIES_EST_CAL_FACTOR$Supply_Dom %>%
              dplyr::arrange(INDUSTRY_CODE)
            
            NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS$Industry_Share <- NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS$Industry_Share/NACE_INDUSTRIES_EST_CAL_FACTOR$Supply_Dom$Total_output_by_industries_at_basic_prices
            
            NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS$Industry_Share)] <- 0
            
            # 2.3.1 Main estimation  --------------------------------------------------
            # old
            #EST.IS <- merge.data.frame(SUPPLY_DOM, SIMULATION, key = "PRODUCT_INDUSTRY_CODE")
            # new 7.12.2024
            
            EST.IS <- merge.data.frame(SUPPLY_DOM, SIM_SCENARIO_EST_CAL_FACTOR_SIM, key = "PRODUCT_INDUSTRY_CODE")
            EST.IS$value = EST.IS$value*EST.IS$Simulated_Policy_Exempt
            
            EST.IS <- EST.IS %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
            EST.IS$value[is.na(EST.IS$value)] <- 0
            
            NACE_INDUSTRIES$Est.IS <- EST.IS %>% 
              dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
              dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
              dplyr::summarise(Industry_Share = sum(value, na.rm = T))
            
            NACE_INDUSTRIES$Est.IS <- NACE_INDUSTRIES$Est.IS %>%
              dplyr::arrange(INDUSTRY_CODE)
            
            NACE_INDUSTRIES$Supply_Dom <- NACE_INDUSTRIES$Supply_Dom %>%
              dplyr::arrange(INDUSTRY_CODE)
            
            NACE_INDUSTRIES$Est.IS$Industry_Share <- NACE_INDUSTRIES$Est.IS$Industry_Share/NACE_INDUSTRIES$Supply_Dom$Total_output_by_industries_at_basic_prices
            
            NACE_INDUSTRIES$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES$Est.IS$Industry_Share)] <- 0
            
            # 2.3.2 Estimation of TE part ---------------------------------------------------
            
            # Copy paste table
            SUPPLY_DOM_EST_TE = copy(SUPPLY_DOM)
            NACE_INDUSTRIES_EST_TE = copy(NACE_INDUSTRIES)
            
            EST.IS_EST_TE <- merge.data.frame(SUPPLY_DOM_EST_TE, SIM_SCENARIO_EST_TE, key = "PRODUCT_INDUSTRY_CODE")
            EST.IS_EST_TE$value = EST.IS_EST_TE$value*EST.IS_EST_TE$Simulated_Policy_Exempt
            
            EST.IS_EST_TE <- EST.IS_EST_TE %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
            EST.IS_EST_TE$value[is.na(EST.IS_EST_TE$value)] <- 0
            
            NACE_INDUSTRIES_EST_TE$Est.IS <- EST.IS_EST_TE %>% 
              dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
              dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
              dplyr::summarise(Industry_Share = sum(value, na.rm = T))
            
            NACE_INDUSTRIES_EST_TE$Est.IS <- NACE_INDUSTRIES_EST_TE$Est.IS %>%
              dplyr::arrange(INDUSTRY_CODE)
            
            NACE_INDUSTRIES_EST_TE$Supply_Dom <- NACE_INDUSTRIES_EST_TE$Supply_Dom %>%
              dplyr::arrange(INDUSTRY_CODE)
            
            NACE_INDUSTRIES_EST_TE$Est.IS$Industry_Share <- NACE_INDUSTRIES_EST_TE$Est.IS$Industry_Share/NACE_INDUSTRIES_EST_TE$Supply_Dom$Total_output_by_industries_at_basic_prices
            
            NACE_INDUSTRIES_EST_TE$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES_EST_TE$Est.IS$Industry_Share)] <- 0
            
            # 2.3.3 Estimation of effective VAT RATE ----------------------------------------
            # Copy paste table
            SUPPLY_DOM_EFFECTIVE_VAT_RATE = copy(SUPPLY_DOM)
            NACE_INDUSTRIES_EFFECTIVE_VAT_RATE = copy(NACE_INDUSTRIES)
            
            EST.IS_EFFECTIVE_VAT_RATE <- merge.data.frame(SUPPLY_DOM_EFFECTIVE_VAT_RATE, SIM_SCENARIO_EFFECTIVE_VAT_RATE, key = "PRODUCT_INDUSTRY_CODE")
            EST.IS_EFFECTIVE_VAT_RATE$value = EST.IS_EFFECTIVE_VAT_RATE$value*EST.IS_EFFECTIVE_VAT_RATE$Simulated_Policy_Exempt
            
            EST.IS_EFFECTIVE_VAT_RATE <- EST.IS_EFFECTIVE_VAT_RATE %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
            EST.IS_EFFECTIVE_VAT_RATE$value[is.na(EST.IS_EFFECTIVE_VAT_RATE$value)] <- 0
            
            NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS <- EST.IS_EFFECTIVE_VAT_RATE %>% 
              dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
              dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
              dplyr::summarise(Industry_Share = sum(value, na.rm = T))
            
            NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS <- NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS %>%
              dplyr::arrange(INDUSTRY_CODE)
            
            NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Supply_Dom <- NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Supply_Dom %>%
              dplyr::arrange(INDUSTRY_CODE)
            
            NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS$Industry_Share <- NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS$Industry_Share/NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Supply_Dom$Total_output_by_industries_at_basic_prices
            
            NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS$Industry_Share)] <- 0
            
  # 1.4 Est Rev ----
            # 2.4.0 Estimation of Calibration factor ---------------------------------------------------
            USE_K_DOM_NETPURCH_EST_CAL_FACTOR = copy(USE_K_DOM_NETPURCH)
            CPA_PRODUCTS_EST_CAL_FACTOR_SIM = copy(CPA_PRODUCTS_SIM)
            
            EST_REV_EST_CAL_FACTOR <- USE_K_DOM_NETPURCH_EST_CAL_FACTOR %>% 
              merge.data.frame(NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS, key = "INDUSTRY_NAME") %>%
              merge.data.frame(SIM_SCENARIO_EST_CAL_FACTOR_SIM, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*Industry_Share*value) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
            
            CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev <- EST_REV_EST_CAL_FACTOR %>% 
              dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
              dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_CODE)
            
            CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev <- CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Use_K_NetPurch %>% 
              merge.data.frame(SIM_SCENARIO_EST_CAL_FACTOR_SIM, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_by_households,
                            Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_NPISH,
                            Final_Demand_Government = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_by_government) %>%
              merge.data.frame(CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_NAME)
            
            
            CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
              CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
              #(1-SIMULATION$Simulated_Policy_Exempt[62])*vat_rate_on_residential_construction
              # NEW 12/07/2024
              (1-SIM_SCENARIO_EST_CAL_FACTOR_SIM$Simulated_Policy_Exempt[27])*vat_rate_on_residential_construction
              
              
            
            CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                             CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev$Final_Demand_HH, 
                                                             CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev$Final_Demand_NPISH, 
                                                             CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev$Final_Demand_Government, na.rm = T) 
            
            
            # 2.4.1 Main estimation ----------------------------------------------------
            EST_REV <- USE_K_DOM_NETPURCH %>% 
              merge.data.frame(NACE_INDUSTRIES$Est.IS, key = "INDUSTRY_NAME") %>%
              # old
              #merge.data.frame(SIMULATION, key = "PRODUCT_INDUSTRY_NAME") %>%
              # new 12.07/2024
              merge.data.frame(SIM_SCENARIO_EST_CAL_FACTOR_SIM, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*Industry_Share*value) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
            
            CPA_PRODUCTS_SIM$Est_Rev <- EST_REV %>% 
              dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
              dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_CODE)
            
            CPA_PRODUCTS_SIM$Est_Rev <- CPA_PRODUCTS_SIM$Use_K_NetPurch %>% 
              # OLD
              #merge.data.frame(SIMULATION, key = "PRODUCT_INDUSTRY_NAME") %>%
              # new 12.07/2024
              merge.data.frame(SIM_SCENARIO_EST_CAL_FACTOR_SIM, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_by_households,
                            Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_NPISH,
                            Final_Demand_Government = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_by_government) %>%
              merge.data.frame(CPA_PRODUCTS_SIM$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_NAME)
            
            
            CPA_PRODUCTS_SIM$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_SIM$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
              CPA_PRODUCTS_SIM$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_SIM$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
              #(1-SIMULATION$Simulated_Policy_Exempt[62])*vat_rate_on_residential_construction
              # neW 07/12/2024
              (1-SIM_SCENARIO_EST_CAL_FACTOR_SIM$Simulated_Policy_Exempt[27])*vat_rate_on_residential_construction
            
            
            CPA_PRODUCTS_SIM$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_SIM$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                           CPA_PRODUCTS_SIM$Est_Rev$Final_Demand_HH, 
                                                           CPA_PRODUCTS_SIM$Est_Rev$Final_Demand_NPISH, 
                                                           CPA_PRODUCTS_SIM$Est_Rev$Final_Demand_Government, na.rm = T)  
            
            # Extracting NACE names
            NACE_NAMES<-CPA_PRODUCTS_SIM$Est_Rev%>%
              select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME)

            # 2.4.2 Estimation of TE part ---------------------------------------------------
            USE_K_DOM_NETPURCH_EST_TE = copy(USE_K_DOM_NETPURCH)
            CPA_PRODUCTS_EST_TE_SIM = copy(CPA_PRODUCTS_SIM)
            
            EST_REV_EST_TE <- USE_K_DOM_NETPURCH_EST_TE %>% 
              merge.data.frame(NACE_INDUSTRIES_EST_TE$Est.IS, key = "INDUSTRY_NAME") %>%
              merge.data.frame(SIM_SCENARIO_EST_TE, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*Industry_Share*value) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
            
            CPA_PRODUCTS_EST_TE_SIM$Est_Rev <- EST_REV_EST_TE %>% 
              dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
              dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_CODE)
            
            CPA_PRODUCTS_EST_TE_SIM$Est_Rev <- CPA_PRODUCTS_EST_TE_SIM$Use_K_NetPurch %>% 
              merge.data.frame(SIM_SCENARIO_EST_TE, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_by_households,
                            Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_NPISH,
                            Final_Demand_Government = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_by_government) %>%
              merge.data.frame(CPA_PRODUCTS_EST_TE_SIM$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_NAME)
            
            
            CPA_PRODUCTS_EST_TE_SIM$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_EST_TE_SIM$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
              CPA_PRODUCTS_EST_TE_SIM$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_EST_TE_SIM$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
              #(1-SIMULATION$Simulated_Policy_Exempt[62])*vat_rate_on_residential_construction
              # NEW 7/12/2024
              #(1-SIMULATION_EST_CAL_FACTOR$Simulated_Policy_Exempt[62])*vat_rate_on_residential_construction
              (1-SIM_SCENARIO_EST_TE$Simulated_Policy_Exempt[27])*vat_rate_on_residential_construction
            
   
            
            CPA_PRODUCTS_EST_TE_SIM$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_EST_TE_SIM$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                             CPA_PRODUCTS_EST_TE_SIM$Est_Rev$Final_Demand_HH, 
                                                             CPA_PRODUCTS_EST_TE_SIM$Est_Rev$Final_Demand_NPISH, 
                                                             CPA_PRODUCTS_EST_TE_SIM$Est_Rev$Final_Demand_Government, na.rm = T) 
            
            # 2.4.3 Estimation of effective VAT RATE ----------------------------------------
            
            USE_K_DOM_NETPURCH_EFFECTIVE_VAT_RATE = copy(USE_K_DOM_NETPURCH)
            CPA_PRODUCTS_EFFECTIVE_VAT_RATE = copy(CPA_PRODUCTS_SIM)
            
            EST_REV_EFFECTIVE_VAT_RATE <- USE_K_DOM_NETPURCH_EFFECTIVE_VAT_RATE %>% 
              merge.data.frame(NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS, key = "INDUSTRY_NAME") %>%
              merge.data.frame(SIM_SCENARIO_EFFECTIVE_VAT_RATE, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*Industry_Share*value) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
            
            CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev <- EST_REV_EFFECTIVE_VAT_RATE %>% 
              dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
              dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_CODE)
            
            CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev <- CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Use_K_NetPurch %>% 
              merge.data.frame(SIM_SCENARIO_EFFECTIVE_VAT_RATE, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_by_households,
                            Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_NPISH,
                            Final_Demand_Government = (Simulated_Policy_Reduced_Rate*PreferentialVATRate_1+Simulated_Policy_Fully_Taxable*StandardVATRate)*
                              Final_consumption_expenditure_by_government) %>%
              merge.data.frame(CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
              dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
              dplyr::arrange(PRODUCT_INDUSTRY_NAME)
            
            
            CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
              CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
             # (1-SIMULATION$Simulated_Policy_Exempt[62])*vat_rate_on_residential_construction
              # new 07/12/2024
              (1-SIM_SCENARIO_EST_CAL_FACTOR_SIM$Simulated_Policy_Exempt[27])*vat_rate_on_residential_construction
            
            CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                             CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Final_Demand_HH, 
                                                             CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Final_Demand_NPISH, 
                                                             CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Final_Demand_Government, na.rm = T) 
            
            
            
  # 2. SIMULATION RESULTS ----  
            
            'DO TUKA !!! DA SE VIDI KOJ OD OVIE Results TREBA DA OSTANAT !! TIE STO NE TREBA DA SE TRGNAT
             ISTO TAKA I KAJ OVIE MOZE DA SE NAPRAVAT IZMENI I DA NE SE KORISTAT BROEVI KAKO SUFIKSI
             
              CPA_PRODUCTS - MAIN ESTIMATION
              CPA_PRODUCTS_0->CPA_PRODUCTS_EST_CAL_FACTOR
              CPA_PRODUCTS_1->CPA_PRODUCTS_EST_TE_SIM
              CPA_PRODUCTS_2->CPA_PRODUCTS_EFFECTIVE_VAT_RATE
            
            '
            
            # 2.1.0 Estimation of Calibration factor ---------------------------------------------------
            Results_EST_CAL_FACTOR <- as.list(c("VAT_Gap", "Simulation"))
            names(Results_EST_CAL_FACTOR) <- c("VAT_Gap", "Simulation")
            
            # NEW
            Results_EST_CAL_FACTOR$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_EST_CAL_FACTOR_SIM$BM_Rev$Final_Demand_Total))
            colnames(Results_EST_CAL_FACTOR$VAT_Gap) <- "Benchmark_VAT_LCU"
            
            Uncalibrated_VAT<- sum(CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev$Final_Demand_Total, na.rm = T)
            VAT_control<-sum(CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Use_VAT$Exports_FOB, na.rm = T)
            
            Locked_Calibration_Factor_TEST<-VAT_control/Uncalibrated_VAT
            
            Locked_Calibration_Factor<-Locked_Calibration_Factor_TEST
            
            #View(Locked_Calibration_Factor_TEST)
            Results_EST_CAL_FACTOR$VAT_Gap$Calibrated_VAT_Est.LCU <- sum(CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
            Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU <- sum(CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Use_VAT$Exports_FOB, na.rm = T)
            Results_EST_CAL_FACTOR$VAT_Gap$Total_VAT_Gap.LCU <- Results_EST_CAL_FACTOR$VAT_Gap$Benchmark_VAT_LCU - Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU
            Results_EST_CAL_FACTOR$VAT_Gap$Total_VAT_Gap.Prc <- Results_EST_CAL_FACTOR$VAT_Gap$Total_VAT_Gap.LCU/Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU
            
            
            # 2.1.1 Main estimation --------------------------------------------------
            
            Results <- as.list(c("VAT_Gap", "Simulation"))
            names(Results) <- c("VAT_Gap", "Simulation")
            
            
            Results$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_SIM$BM_Rev$Final_Demand_Total))
            colnames(Results$VAT_Gap) <- "Benchmark_VAT_LCU"
            
            Results$VAT_Gap$Uncalibrated_VAT_Est.LCU <- sum(CPA_PRODUCTS_SIM$Est_Rev$Final_Demand_Total, na.rm = T)
            
            
            # Test new 24/12/2024
            
            #Results$VAT_Gap$Calibrated_VAT_Est.LCU <- sum(CPA_PRODUCTS_SIM$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
            
            Results$VAT_Gap$Calibrated_VAT_Est.LCU <- sum(CPA_PRODUCTS_SIM$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor_BU
            
            
            
            Results$VAT_Gap$VAT_Control_Total.LCU <- sum(CPA_PRODUCTS_SIM$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_SIM$Use_VAT$Exports_FOB, na.rm = T)
            Results$VAT_Gap$Total_VAT_Gap.LCU <- Results$VAT_Gap$Benchmark_VAT_LCU - Results$VAT_Gap$VAT_Control_Total.LCU
            Results$VAT_Gap$Total_VAT_Gap.Prc <- Results$VAT_Gap$Total_VAT_Gap.LCU/Results$VAT_Gap$VAT_Control_Total.LCU
            
            
            # Final output - Change in Revenues
            
            
            Results$Simulation <- as.data.frame(as.integer(Results$VAT_Gap$Calibrated_VAT_Est.LCU-Results$VAT_Gap$VAT_Control_Total.LCU))
            colnames(Results$Simulation) <- "Simulated_Change_in_Revenues.LCU"
            
            Results$Simulation$Simulated_Change_in_Revenues.Prc <- (Results$Simulation$Simulated_Change_in_Revenues.LCU/Results$VAT_Gap$VAT_Control_Total.LCU)*100
            
            
            #View(Results$Simulation)
            
            
            # 2.1.2 Estimation of TE part ---------------------------------------------------
            
            Results_EST_TE <- as.list(c("VAT_Gap", "Simulation"))
            names(Results_EST_TE) <- c("VAT_Gap", "Simulation")
            
            # NEW
            Results_EST_TE$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_EST_TE_SIM$BM_Rev$Final_Demand_Total))
            colnames(Results_EST_TE$VAT_Gap) <- "Benchmark_VAT_LCU"
            
            Results_EST_TE$VAT_Gap$Uncalibrated_VAT_Est.LCU <- sum(CPA_PRODUCTS_EST_TE_SIM$Est_Rev$Final_Demand_Total, na.rm = T)
            Results_EST_TE$VAT_Gap$Calibrated_VAT_Est.LCU <- sum(CPA_PRODUCTS_EST_TE_SIM$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
            Results_EST_TE$VAT_Gap$VAT_Control_Total.LCU <- sum(CPA_PRODUCTS_EST_TE_SIM$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_EST_TE_SIM$Use_VAT$Exports_FOB, na.rm = T)
            Results_EST_TE$VAT_Gap$Total_VAT_Gap.LCU <- Results_EST_TE$VAT_Gap$Benchmark_VAT_LCU - Results_EST_TE$VAT_Gap$VAT_Control_Total.LCU
            Results_EST_TE$VAT_Gap$Total_VAT_Gap.Prc <- Results_EST_TE$VAT_Gap$Total_VAT_Gap.LCU/Results_EST_TE$VAT_Gap$VAT_Control_Total.LCU
            
            
            # Final output - Change in Revenues
            
            
            Results_EST_TE$Simulation <- as.data.frame(as.integer(Results_EST_TE$VAT_Gap$Calibrated_VAT_Est.LCU-Results_EST_TE$VAT_Gap$VAT_Control_Total.LCU))
            colnames(Results_EST_TE$Simulation) <- "Simulated_Change_in_Revenues.LCU"
            
            Results_EST_TE$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_EST_TE$Simulation$Simulated_Change_in_Revenues.LCU/Results_EST_TE$VAT_Gap$VAT_Control_Total.LCU)*100
            
            
            
            # 2.1.1 Adding TE'S ----------------------------------------------------------------
            # This part is from 1.1.1 main estimation
            Results$VAT_Gap$Policy_Gap.LCU <-Results_EST_TE$Simulation$Simulated_Change_in_Revenues.LCU
            Results$VAT_Gap$Policy_Gap.Prc <- Results$VAT_Gap$Policy_Gap.LCU/Results$VAT_Gap$VAT_Control_Total.LCU
            Results$VAT_Gap$Compliance_Gap.LCU <- Results$VAT_Gap$Total_VAT_Gap.LCU-Results$VAT_Gap$Policy_Gap.LCU
            Results$VAT_Gap$Compliance_Gap.Prc <- Results$VAT_Gap$Compliance_Gap.LCU/Results$VAT_Gap$VAT_Control_Total.LCU
            Results$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor
            
            # This part is for calibration factor
            Results_EST_CAL_FACTOR$VAT_Gap$Policy_Gap.LCU <- Results_EST_TE$Simulation$Simulated_Change_in_Revenues.LCU
            
            #New
            Results_EST_CAL_FACTOR$VAT_Gap$Policy_Gap.Prc <- Results_EST_CAL_FACTOR$VAT_Gap$Policy_Gap.LCU/Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU
            Results_EST_CAL_FACTOR$VAT_Gap$Compliance_Gap.LCU <- Results_EST_CAL_FACTOR$VAT_Gap$Total_VAT_Gap.LCU-Results_EST_CAL_FACTOR$VAT_Gap$Policy_Gap.LCU
            Results_EST_CAL_FACTOR$VAT_Gap$Compliance_Gap.Prc <- Results_EST_CAL_FACTOR$VAT_Gap$Compliance_Gap.LCU/Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU
            Results_EST_CAL_FACTOR$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor
            
            
            # Final output - Change in Revenues
            
            
            Results_EST_CAL_FACTOR$Simulation <- as.data.frame(as.integer(Results_EST_CAL_FACTOR$VAT_Gap$Calibrated_VAT_Est.LCU-Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU))
            colnames(Results_EST_CAL_FACTOR$Simulation) <- "Simulated_Change_in_Revenues.LCU"
            
            Results_EST_CAL_FACTOR$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_EST_CAL_FACTOR$Simulation$Simulated_Change_in_Revenues.LCU/Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU)*100
            
            
            
            # 2.1.4 Other files -------------------------------------------------------
            
            Export_Main_Results<- Results$VAT_Gap 
            Simulation_Results<-Results$Simulation
            
            
            Revenue_VAT_TOTAL_SIM<-CPA_PRODUCTS_SIM$Est_Rev%>%
              dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_Revenues_from_Intermediate_Inputs,Final_Demand_HH,Final_Demand_NPISH,Final_Demand_Government,Final_Demand_Total)
            
            # Without text column
            Revenue_VAT_TOTAL_SIM[3:7]<-Revenue_VAT_TOTAL_SIM[3:7]*Locked_Calibration_Factor
            
            #  Result from tax expenditures estimation
            # Result 1
            # NEW
            Simulation_Results_EST_TE<-Results_EST_TE$Simulation
            #Simulation_Results_EST_TE_te<-Results_EST_TE$VAT_Gap$Policy_Gap.LCU
            Simulation_Results_EST_TE_te<-Results_EST_TE$Simulation$Simulated_Change_in_Revenues.LCU 
            
            # Old
            #Est_Rev1<-CPA_PRODUCTS_EST_TE_SIM$Est_Rev
            # New 22/04/2024
            #Est_Rev1<-CPA_PRODUCTS_EST_TE_SIM$Est_Rev
            
            #View(Est_Rev1)
            

# 3.Aggregation of data -----------------------------------------------------
            MainResultsVATFinal_SIM<-data.frame(
                                  Descriptions=c("Benchmark VAT rate",
                                                 "Benchmark VAT",
                                                 "Uncalibrated VAT",
                                                 "Calibrated VAT",
                                                 "Total VAT Gap",
                                                 "Policy Gap",
                                                 "Compliance Gap",
                                                 "Total VAT Gap Pct",
                                                 "Policy Gap Pct",
                                                 "Compliance Gap Pct"
                                                   ),
                                  Values= c(benchmark_tax_rate,
                                            round(Results$VAT_Gap$Benchmark_VAT_LCU,0),
                                            round(Results$VAT_Gap$Uncalibrated_VAT_Est.LCU),
                                            round(Results$VAT_Gap$Calibrated_VAT_Est.LCU,0),
                                            round(Results$VAT_Gap$Total_VAT_Gap.LCU,0),
                                            round(Results$VAT_Gap$Policy_Gap.LCU,0),
                                            round(Results$VAT_Gap$Compliance_Gap.LCU,0),
                                            round(Results$VAT_Gap[1,6]*100,1),
                                            round(Results$VAT_Gap[1,8]*100,1),
                                            round(Results$VAT_Gap[1,10]*100,1)
                                         
                                  ))%>%
                                  data.table()
                                
})                                     
            print("Script TAX CALCULATOR-MODULE SIM Done !")     
            print(MainResultsVATFinal_SIM)
            
            end.time <- proc.time()
            save.time <- end.time - start.time
            cat("\n Time of calculation:", save.time[3] / 60, "\n \n")
            