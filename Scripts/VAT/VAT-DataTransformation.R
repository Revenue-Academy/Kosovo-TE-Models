'
VAT-DATA TRANSFORMATION
'
start.time <- proc.time()
suppressMessages({

# I.PART  ----------------------------------------------------------------

  CPA_TAXABLE_PROPORTIONS_BU <- CPA_TAXABLE_PROPORTIONS_BU %>%
    dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(ProportionExempted), Current_Policy_Exempt, ProportionExempted),
                  Simulated_Policy_Reduced_Rate = ifelse(is.na(PreferentialVATRate_1), Current_Policy_Reduced_Rate, PreferentialVATRate_1),
                  Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
  
  
  # not in function !!
  
  # t1
  simulationSlider_VAT_1_t1 <- 1
  simulationSlider_VAT_2_t1 <- 1
  simulationSlider_VAT_3_t1 <- 1
  simulationSlider_VAT_4_t1 <- 1
  simulationSlider_VAT_5_t1 <- 1
  simulationSlider_VAT_6_t1 <- 1
  simulationSlider_VAT_7_t1 <- 1
  
  # t2
  simulationSlider_VAT_1_t2 <- 1
  simulationSlider_VAT_2_t2 <- 1
  simulationSlider_VAT_3_t2 <- 1
  simulationSlider_VAT_4_t2 <- 1
  simulationSlider_VAT_5_t2 <- 1
  simulationSlider_VAT_6_t2 <- 1
  simulationSlider_VAT_7_t2 <- 1
  
  # t3
  simulationSlider_VAT_1_t3 <- 1
  simulationSlider_VAT_2_t3 <- 1
  simulationSlider_VAT_3_t3 <- 1
  simulationSlider_VAT_4_t3 <- 1
  simulationSlider_VAT_5_t3 <- 1
  simulationSlider_VAT_6_t3 <- 1
  simulationSlider_VAT_7_t3 <- 1
  
  # t4
  simulationSlider_VAT_1_t4 <- 1
  simulationSlider_VAT_2_t4 <- 1
  simulationSlider_VAT_3_t4 <- 1
  simulationSlider_VAT_4_t4 <- 1
  simulationSlider_VAT_5_t4 <- 1
  simulationSlider_VAT_6_t4 <- 1
  simulationSlider_VAT_7_t4 <- 1
  
  # t5
  simulationSlider_VAT_1_t5 <- 1
  simulationSlider_VAT_2_t5 <- 1
  simulationSlider_VAT_3_t5 <- 1
  simulationSlider_VAT_4_t5 <- 1
  simulationSlider_VAT_5_t5 <- 1
  simulationSlider_VAT_6_t5 <- 1
  simulationSlider_VAT_7_t5 <- 1
  
  simulationSlider_VAT_7_t<-1
  simulationSlider_VAT_1_t<-1
  
  benchmark_tax_rate_bu <- 0.18
  # standard_VAT_rate = 0.18
  # preferential_VAT_rate = 0.05
  
  TE_EXEMPT<-0.00
  TE_REDUCED_RATE<-0.00
  
  # 2.CALCULATION BASE FOR TAXABLE PROPORTIONS --------------------
        # 2.1 AGGREGATE DATA IN LISTS -----
        
        CPA_PRODUCTS_raw <- as.list(c(1:4))
        names(CPA_PRODUCTS_raw) = c("Supply", "Use_Purchaser", "Use_VAT", "Use_Basic")
        
        NACE_INDUSTRIES_raw <- as.list(c(1:4))
        names(NACE_INDUSTRIES_raw) = c("Supply", "Use_Purchaser", "Use_VAT", "Use_Basic")
        
        # TEST
        RC_prc_of_Constructions_and_construction_works = 0
        vat_rate_on_residential_construction = 0.18
        
        # 2.2  SUPPLY MATRIX -----
        # 2.2.1 Adding weights -----------------------------------------------------------
        
        # WARNING MANUAL ADDED DATA !!!!

        weight_use_purchaser<-1
        
        SUPPLY<-SUPPLY_raw%>%
          dplyr::mutate(value=value*weight_use_purchaser)
        
        # 2.2.2 Data prep ---------------------------------------------------------------
        
        CPA_PRODUCTS_raw$Supply <- SUPPLY %>% 
          dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
          dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
          dplyr::summarise(Total_output = sum(value, na.rm = T))
        
        CPA_PRODUCTS_raw$Supply <- SUPPLY %>%
          dplyr::filter(INDUSTRY_NAME == "Imports CIF") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS_raw$Supply, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Imports_CIF = value)
        
        CPA_PRODUCTS_raw$Supply <- SUPPLY %>%
          dplyr::filter(INDUSTRY_NAME == "Trade and transport margins") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS_raw$Supply, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Trade_and_transport_margins = value)
        
        CPA_PRODUCTS_raw$Supply <- SUPPLY %>%
          dplyr::filter(INDUSTRY_NAME == "Taxes less subsidies on products") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS_raw$Supply, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Taxes_less_subsidies_on_products = value)
        
        
        # Replaced in Data preprocessing-Module 
        
        CPA_PRODUCTS_raw$Supply <- CPA_PRODUCTS_raw$Supply %>%
          dplyr::mutate(Total_supply_at_basic_prices = psum(Total_output,Imports_CIF, na.rm=TRUE),
                        Total_supply_at_purchasers_prices = psum(Total_supply_at_basic_prices,
                                                                 Trade_and_transport_margins,
                                                                 Taxes_less_subsidies_on_products, na.rm=TRUE))
        
        NACE_INDUSTRIES_raw$Supply <- SUPPLY %>%
          dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
          dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
          dplyr::summarise(Total_output_by_industries_at_basic_prices = sum(value, na.rm = T))
        
        
        
  # 3. USE PURCHASER MATRIX ----
      # 3.1.1 Adding weights -----------------------------------------------------------
      'PRODUCT_INDUSTRY_CODE'
      'Final consumption expenditure by households'
      'Final consumption expenditure by non-profit organisations serving households (NPISH)'
      'Final consumption expenditure by government'
      #'Gross capital formation'
      'Gross fixed capital formation'
      "Changes in inventories and acquisition less disposals of valuables" 
      'Exports FOB'
      'INDUSTRY_CODE'
      
      
      industry_codes <- c(
                          "A01", "A02", "A03", "B", "C10T12", "C13T15", "C16", "C17", "C18", "C19", "C20",
                          "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31_32",
                          "C33", "D35", "E36", "E37T39", "F", "G45", "G46", "G47", "H49", "H50", "H51", "H52",
                          "H53", "I", "J58", "J59_60", "J61", "J62_63", "K64", "K65", "K66", "L68B", "L68A",
                          "M69_70", "M71", "M72", "M73", "M74_75", "N77", "N78", "N79", "N80T82", "O84", "P85",
                          "Q86", "Q87_88", "R90T92", "R93", "S94", "S95", "S96", "T"
                        )
      
      
      
      # Mutate the value column based on the multiple conditions
      USE_PURCHASER <- USE_PURCHASER_raw %>%
        mutate(value = case_when(
          INDUSTRY_CODE %in% industry_codes ~ value * 1, #<---Intermediate consumption
          INDUSTRY_NAME == "Final consumption expenditure by households" ~ value * 1,
          INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)" ~ value * 1,
          INDUSTRY_NAME == "Final consumption expenditure by government" ~ value * 1,
          INDUSTRY_NAME == "Gross fixed capital formation" ~ value * 1,
          INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables" ~ value * 1,
          INDUSTRY_NAME == "Exports FOB" ~ value * 1,
          TRUE ~ value
        ))
      
      
      # 3.1.2 Data prep ---------------------------------------------------------------
      
      
      
      CPA_PRODUCTS_raw$Use_Purchaser <- USE_PURCHASER %>% 
        dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
        dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
        dplyr::summarise(Total_intermediate_consumption_at_purchasers_prices = sum(value, na.rm = T))
      
      CPA_PRODUCTS_raw$Use_Purchaser <-  USE_PURCHASER %>%
        dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by households") %>%
        dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
        merge.data.frame(CPA_PRODUCTS_raw$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
        dplyr::rename(Final_consumption_expenditure_by_households = value)
      
      CPA_PRODUCTS_raw$Use_Purchaser <- USE_PURCHASER %>%
        dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)") %>%
        dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
        merge.data.frame(CPA_PRODUCTS_raw$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
        dplyr::rename(Final_consumption_expenditure_NPISH = value)
      
      CPA_PRODUCTS_raw$Use_Purchaser <- USE_PURCHASER %>%
        dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by government") %>%
        dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
        merge.data.frame(CPA_PRODUCTS_raw$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
        dplyr::rename(Final_consumption_expenditure_by_government = value)
      
      CPA_PRODUCTS_raw$Use_Purchaser <- USE_PURCHASER %>%
        dplyr::filter(INDUSTRY_NAME == "Gross fixed capital formation") %>%
        dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
        merge.data.frame(CPA_PRODUCTS_raw$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
        dplyr::rename(Gross_fixed_capital_formation = value)
      
      CPA_PRODUCTS_raw$Use_Purchaser <- USE_PURCHASER %>%
        dplyr::filter(INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables") %>%
        dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
        merge.data.frame(CPA_PRODUCTS_raw$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
        dplyr::rename(Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value)
      
      CPA_PRODUCTS_raw$Use_Purchaser <- USE_PURCHASER %>%
        dplyr::filter(INDUSTRY_NAME == "Exports FOB") %>%
        dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
        merge.data.frame(CPA_PRODUCTS_raw$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
        dplyr::rename(Exports_FOB = value)
      
      
      # Replaced in Data preprocessing-Module 
      
      
      CPA_PRODUCTS_raw$Use_Purchaser <- CPA_PRODUCTS_raw$Use_Purchaser %>%
        dplyr::mutate(Total_final_consumption_expenditure_at_purchasers_prices = psum(Final_consumption_expenditure_by_households,
                                                                                      Final_consumption_expenditure_NPISH,
                                                                                      Final_consumption_expenditure_by_government, na.rm=TRUE),
                      Gross_capital_formation = psum(Gross_fixed_capital_formation,
                                                     Changes_in_inventories_and_acquisition_less_disposals_of_valuables, na.rm=TRUE),
                      Total_final_uses_at_purchasers_prices = psum(Total_final_consumption_expenditure_at_purchasers_prices,
                                                                   Gross_capital_formation,
                                                                   Exports_FOB, na.rm=TRUE),
                      Total_use_at_purchasers_prices = psum(Total_intermediate_consumption_at_purchasers_prices,
                                                            Total_final_uses_at_purchasers_prices, na.rm=TRUE))
      
      
      NACE_INDUSTRIES_raw$Use_Purchaser <- USE_PURCHASER %>%
        dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
        dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
        dplyr::summarise(Total_intermediate_consumption_by_industries_at_purchasers_prices = sum(value, na.rm = T))
      
      
      
  # 4. USE BASIC MATRIX ----
        # 4.1.1 Adding weights -----------------------------------------------------------
        USE_BASIC<- USE_BASIC_raw %>%
          mutate(value = case_when(
            INDUSTRY_NAME == "Exports FOB" ~ value * 1,
            TRUE ~ value
          ))
        
        # 4.1.1 Data prep ---------------------------------------------------------------
        
        CPA_PRODUCTS_raw$Use_Basic <- USE_BASIC %>% 
          dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
          dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
          dplyr::summarise(Total_intermediate_consumption_at_basic_prices = sum(value, na.rm = T))
        
        CPA_PRODUCTS_raw$Use_Basic <- USE_BASIC %>%
          dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by households") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS_raw$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Final_consumption_expenditure_by_households = value)
        
        CPA_PRODUCTS_raw$Use_Basic <- USE_BASIC %>%
          dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS_raw$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Final_consumption_expenditure_NPISH = value)
        
        CPA_PRODUCTS_raw$Use_Basic <- USE_BASIC %>%
          dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by government") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS_raw$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Final_consumption_expenditure_by_government = value)
        
        CPA_PRODUCTS_raw$Use_Basic <- USE_BASIC %>%
          dplyr::filter(INDUSTRY_NAME == "Gross fixed capital formation") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS_raw$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Gross_fixed_capital_formation = value)
        
        CPA_PRODUCTS_raw$Use_Basic <- USE_BASIC %>%
          dplyr::filter(INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS_raw$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value)
        
        CPA_PRODUCTS_raw$Use_Basic <- USE_BASIC %>%
          dplyr::filter(INDUSTRY_NAME == "Exports FOB") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS_raw$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Exports_FOB = value)
        
        CPA_PRODUCTS_raw$Use_Basic$Exports_FOB[is.na(CPA_PRODUCTS_raw$Use_Basic$Exports_FOB)] <- 0
        
        # Replaced in Data preprocessing-Module 
        
        CPA_PRODUCTS_raw$Use_Basic <- CPA_PRODUCTS_raw$Use_Basic %>%
          dplyr::mutate(Total_final_consumption_expenditure_at_basic_prices = psum(
            Final_consumption_expenditure_by_households,
            Final_consumption_expenditure_NPISH,
            Final_consumption_expenditure_by_government,
            na.rm=TRUE),
            Gross_capital_formation = psum(
              Gross_fixed_capital_formation,
              Changes_in_inventories_and_acquisition_less_disposals_of_valuables,
              na.rm=TRUE),
            Total_final_uses_at_basic_prices = psum(Total_final_consumption_expenditure_at_basic_prices,
                                                    Gross_capital_formation,
                                                    Exports_FOB,
                                                    na.rm=TRUE),
            Total_use_at_basic_prices = psum(Total_intermediate_consumption_at_basic_prices,
                                             Total_final_uses_at_basic_prices,
                                             na.rm=TRUE))
        
        
        NACE_INDUSTRIES_raw$Use_Basic <- USE_BASIC %>%
          dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
          dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
          dplyr::summarise(Total_intermediate_consumption_by_industries_at_basic_prices = sum(value, na.rm = T))
        
        
        
  # 5. USE VAT MATRIX ----
  
  USE_VAT<-USE_VAT_raw%>%
    dplyr::mutate(value=value*simulationSlider_VAT_7_t1)
  
  
  CPA_PRODUCTS_raw$Use_VAT <- USE_VAT %>% 
    dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
    dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
    dplyr::summarise(Total_intermediate_consumption_at_basic_prices = sum(value, na.rm = T))
  
  CPA_PRODUCTS_raw$Use_VAT <- USE_VAT %>%
    dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by households") %>%
    dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
    merge.data.frame(CPA_PRODUCTS_raw$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
    dplyr::rename(Final_consumption_expenditure_by_households = value)
  
  
  CPA_PRODUCTS_raw$Use_VAT <- USE_VAT %>%
    dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)") %>%
    dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
    merge.data.frame(CPA_PRODUCTS_raw$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
    dplyr::rename(Final_consumption_expenditure_NPISH = value)
  
  
  CPA_PRODUCTS_raw$Use_VAT <- USE_VAT %>%
    dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by government") %>%
    dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
    merge.data.frame(CPA_PRODUCTS_raw$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
    dplyr::rename(Final_consumption_expenditure_by_government = value)
  
  
  CPA_PRODUCTS_raw$Use_VAT <- USE_VAT %>%
    dplyr::filter(INDUSTRY_NAME == "Gross fixed capital formation") %>%
    dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
    merge.data.frame(CPA_PRODUCTS_raw$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
    dplyr::rename(Gross_fixed_capital_formation = value)
  
  
  CPA_PRODUCTS_raw$Use_VAT <- USE_VAT %>%
    dplyr::filter(INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables") %>%
    dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
    merge.data.frame(CPA_PRODUCTS_raw$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
    dplyr::rename(Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value)
  
  
  CPA_PRODUCTS_raw$Use_VAT <- USE_VAT %>%
    dplyr::filter(INDUSTRY_NAME == "Exports FOB") %>%
    dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
    merge.data.frame(CPA_PRODUCTS_raw$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
    dplyr::rename(Exports_FOB = value)
  
  
  # Replaced in Data preprocessing-Module 
  
  CPA_PRODUCTS_raw$Use_VAT <- CPA_PRODUCTS_raw$Use_VAT %>%
    dplyr::mutate(Total_final_consumption_expenditure_at_basic_prices = psum(Final_consumption_expenditure_by_households,
                                                                             Final_consumption_expenditure_NPISH,
                                                                             Final_consumption_expenditure_by_government,
                                                                             na.rm=TRUE),
                  Gross_capital_formation = psum(Gross_fixed_capital_formation,
                                                 Changes_in_inventories_and_acquisition_less_disposals_of_valuables,
                                                 na.rm=TRUE),
                  Total_final_uses_at_basic_prices = psum(Total_final_consumption_expenditure_at_basic_prices,
                                                          Gross_capital_formation,
                                                          Exports_FOB,
                                                          na.rm=TRUE),
                  Total_use_at_basic_prices = psum(Total_intermediate_consumption_at_basic_prices,
                                                   Total_final_uses_at_basic_prices,
                                                   na.rm=TRUE))
  
  NACE_INDUSTRIES_raw$Use_VAT <- USE_VAT %>%
    dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
    dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
    dplyr::summarise(Total_VAT = sum(value, na.rm = T))
  
  
  
  
  # Raw data comes from IntialLoad Moudule
  CPA_PRODUCTS<-CPA_PRODUCTS_raw
  NACE_INDUSTRIES<-NACE_INDUSTRIES_raw
  

# II.PART  ---------------------------------------------------------------


# 1. ESTIMATION OF VAT -----
      # 1.1 Arrange Aggregate Industries and Products ---------------------------
      
        # Arrange Aggregate Industries
              NACE_INDUSTRIES$Supply <- NACE_INDUSTRIES$Supply %>%
                dplyr::arrange(INDUSTRY_CODE)
              NACE_INDUSTRIES$Use_Purchaser <- NACE_INDUSTRIES$Use_Purchaser %>%
                dplyr::arrange(INDUSTRY_CODE)
              NACE_INDUSTRIES$Use_VAT <- NACE_INDUSTRIES$Use_VAT %>%
                dplyr::arrange(INDUSTRY_CODE)
              NACE_INDUSTRIES$Use_Basic <- NACE_INDUSTRIES$Use_Basic %>%
                dplyr::arrange(INDUSTRY_CODE)
      
      # Arrange Aggregate Products
              CPA_PRODUCTS$Supply <- CPA_PRODUCTS$Supply %>%
                dplyr::arrange(PRODUCT_INDUSTRY_CODE)
              CPA_PRODUCTS$Use_Purchaser <- CPA_PRODUCTS$Use_Purchaser %>%
                dplyr::arrange(PRODUCT_INDUSTRY_CODE)
              CPA_PRODUCTS$Use_VAT <- CPA_PRODUCTS$Use_VAT %>%
                dplyr::arrange(PRODUCT_INDUSTRY_CODE)
              CPA_PRODUCTS$Use_Basic <- CPA_PRODUCTS$Use_Basic %>%
                dplyr::arrange(PRODUCT_INDUSTRY_CODE)
      
      
      
      # 1.2 Domestic Share matrix -----
      
      CPA_PRODUCTS$Dom_Share <- CPA_PRODUCTS$Supply %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
            dplyr::arrange(PRODUCT_INDUSTRY_CODE)
      
      CPA_PRODUCTS$Dom_Share$Domestic_share_of_output_commodity <- 
            pmax(1-CPA_PRODUCTS$Use_Basic$Exports_FOB/CPA_PRODUCTS$Supply$Total_output,0)
      
      
      DOM_SHARE <- SUPPLY %>%
            dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
            dplyr::group_by(PRODUCT_INDUSTRY_CODE) %>%
            dplyr::arrange(INDUSTRY_CODE) %>%
            dplyr::mutate(value = value/NACE_INDUSTRIES$Supply$Total_output_by_industries_at_basic_prices) %>%
            merge.data.frame(CPA_PRODUCTS$Dom_Share, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::mutate(value = value*Domestic_share_of_output_commodity) %>%
            dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
      
      
      NACE_INDUSTRIES$Dom_Share <- DOM_SHARE %>% 
            dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
            dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
            dplyr::summarise(Domestic_Share_of_Output_Industry = sum(value, na.rm = T))
      
      
      # 1.3 Net purchaser prices -----------
      
      USE_NETPURCH <- USE_PURCHASER %>%
            dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
            dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
      
      
      USE_VAT_temp <- USE_VAT %>% 
            dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
            dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
      
      USE_NETPURCH$value[is.na(USE_NETPURCH$value)] <- 0
      USE_VAT_temp$value[is.na(USE_VAT_temp$value)] <- 0
      
      USE_NETPURCH$value <- USE_NETPURCH$value - USE_VAT_temp$value 
      
      CPA_PRODUCTS$Use_NetPurch <- USE_NETPURCH %>% 
            dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
            dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
            dplyr::summarise(Total_intermediate_consumption_net_purchasers_prices = sum(value, na.rm = T))
      
      
      CPA_PRODUCTS$Use_NetPurch <- CPA_PRODUCTS$Use_NetPurch %>% 
            dplyr::arrange(PRODUCT_INDUSTRY_CODE)
      
      
      CPA_PRODUCTS$Use_Purchaser$Gross_fixed_capital_formation[is.na(CPA_PRODUCTS$Use_Purchaser$Gross_fixed_capital_formation)] <- 0
      CPA_PRODUCTS$Use_VAT$Gross_fixed_capital_formation[is.na(CPA_PRODUCTS$Use_VAT$Gross_fixed_capital_formation)] <- 0
      
      
      CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation = 
        CPA_PRODUCTS$Use_Purchaser$Gross_fixed_capital_formation - 
        CPA_PRODUCTS$Use_VAT$Gross_fixed_capital_formation    
      
      
      CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation[is.na(CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation)] <- 0
      
      CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_households[is.na( CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_households)] <- 0
      CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_households[is.na(CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_households)] <- 0
      
      CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_households = 
        CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_households - 
        CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_households
      
      CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_NPISH[is.na(CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_NPISH)] <- 0
      CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_NPISH[is.na(CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_NPISH)] <- 0
      
      
      CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_NPISH = 
        CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_NPISH - 
        CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_NPISH
      
      CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_government[is.na(CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_government)] <- 0
      CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_government[is.na( CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_government)] <- 0
      
      CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_government =
        CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_government - 
        CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_government
      
      
      # 1.4 Capital Shift Map ----
      
      
      CPA_PRODUCTS$K_Shift_Map <- USE_NETPURCH %>% 
        dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
        dplyr::distinct(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
        dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
        dplyr::mutate(Share_of_K_in_HH_FD = 0)
      
      CPA_PRODUCTS$K_Shift_Map$Share_of_K_in_HH_FD[grepl("Constructions and construction works", CPA_PRODUCTS$K_Shift_Map$PRODUCT_INDUSTRY_NAME)] = 
        RC_prc_of_Constructions_and_construction_works
      
      K_SHIFT_MAP <- USE_NETPURCH %>%
        merge.data.frame(CPA_PRODUCTS$Use_NetPurch, key = "PRODUCT_INDUSTRY_CODE") %>%
        merge.data.frame(CPA_PRODUCTS$K_Shift_Map, key = "PRODUCT_INDUSTRY_CODE") %>%
        dplyr::mutate(value = value/Total_intermediate_consumption_net_purchasers_prices*(1-Share_of_K_in_HH_FD)) %>%
        dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE) %>%
        dplyr::select(-Share_of_K_in_HH_FD)
      
      
      CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_households = CPA_PRODUCTS$K_Shift_Map$Share_of_K_in_HH_FD
      CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_NPISH = 0
      CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_government = 0
      
      # 1.5 Use Capital net purchaser prices -----
      
      USE_K_NETPURCH <- K_SHIFT_MAP %>%
            merge.data.frame(CPA_PRODUCTS$Use_NetPurch, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::mutate(value = value*Gross_fixed_capital_formation) %>%
            dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
      
      USE_NETPURCH <- USE_NETPURCH %>% dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
      
      
      USE_K_NETPURCH$value <- USE_K_NETPURCH$value + USE_NETPURCH$value 
      
      
      CPA_PRODUCTS$Use_NetPurch <- CPA_PRODUCTS$Use_NetPurch %>%
          dplyr::arrange(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME)
      
      CPA_PRODUCTS$K_Shift_Map <- CPA_PRODUCTS$K_Shift_Map %>%
         dplyr::arrange(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME)
      
      
      CPA_PRODUCTS$Use_K_NetPurch <- CPA_PRODUCTS$Use_NetPurch %>%
         dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME)
      
      CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_households <-
        CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_households+
        CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_households*
        CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation
      
      CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH <-
        CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_NPISH+
        CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_NPISH*
        CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation
      
      
      # Replace NAs with zero
      CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH[is.na(CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH)] <- 0
      
      
      CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government <-
        CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_government+
        CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_government*
        CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation
      
      # Replace NAs with zero
      CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government[is.na(CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government)] <- 0
      
      # 1.6 Use_K_Dom_NetPurch -----
      
      USE_K_DOM_NETPURCH <- USE_K_NETPURCH %>%
          dplyr::select(PRODUCT_INDUSTRY_NAME, PRODUCT_INDUSTRY_CODE, INDUSTRY_NAME, INDUSTRY_CODE, value) %>%
          merge.data.frame(NACE_INDUSTRIES$Dom_Share, key = "INDUSTRY_CODE") %>%
          dplyr::mutate(value = value*Domestic_Share_of_Output_Industry) %>%
          dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
      
      
      # 1.7 Supply_Dom -----
      
      SUPPLY_DOM <- SUPPLY %>%
          dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
          merge.data.frame(CPA_PRODUCTS$Dom_Share, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::mutate(value = value*Domestic_share_of_output_commodity) %>%
          dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
      
      NACE_INDUSTRIES$Supply_Dom <- SUPPLY_DOM %>% 
          dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
          dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
          dplyr::summarise(Total_output_by_industries_at_basic_prices = sum(value, na.rm = T))
      
})     
      print("Script Data Transformation Done !")