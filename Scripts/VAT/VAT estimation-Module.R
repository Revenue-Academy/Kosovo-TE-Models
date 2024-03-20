'
    ESTIMATION-MODULE
                        '
# From Data processing

# nEW
#standard_VAT_rate = VAT_Input

#suppressMessages({ 
  

colnames(SIMULATION) <- column_names
          SIMULATION <- SIMULATION %>%
            dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(Simulation_Toggles_Exempt), Current_Policy_Exempt, Simulation_Toggles_Exempt),
                          Simulated_Policy_Reduced_Rate = ifelse(is.na(Simulation_Toggles_Reduced_Rate), Current_Policy_Reduced_Rate, Simulation_Toggles_Reduced_Rate),
                          Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)

# 1. ESTIMATION -----
      # 1.1 Arrange Aggregate Industries and Products ---------------------------
      # 1.1.1 Arrange Aggregate Industries

      NACE_INDUSTRIES$Supply$INDUSTRY_CODE<-as.numeric(NACE_INDUSTRIES$Supply$INDUSTRY_CODE) #New
      NACE_INDUSTRIES$Supply <- NACE_INDUSTRIES$Supply %>%
        dplyr::arrange(INDUSTRY_CODE)
      
      NACE_INDUSTRIES$Use_Purchaser$INDUSTRY_CODE<-as.numeric(NACE_INDUSTRIES$Use_Purchaser$INDUSTRY_CODE) #New
      NACE_INDUSTRIES$Use_Purchaser <- NACE_INDUSTRIES$Use_Purchaser %>%
        dplyr::arrange(INDUSTRY_CODE)
      
      
      NACE_INDUSTRIES$Use_VAT$INDUSTRY_CODE<-as.numeric(NACE_INDUSTRIES$Use_VAT$INDUSTRY_CODE) #New
      NACE_INDUSTRIES$Use_VAT <- NACE_INDUSTRIES$Use_VAT %>%
        dplyr::arrange(INDUSTRY_CODE)
      
      
      NACE_INDUSTRIES$Use_Basic$INDUSTRY_CODE<-as.numeric(NACE_INDUSTRIES$Use_Basic$INDUSTRY_CODE) #New
      NACE_INDUSTRIES$Use_Basic <- NACE_INDUSTRIES$Use_Basic %>%
        dplyr::arrange(INDUSTRY_CODE)
      

      # 1.1.2 Arrange Aggregate Products
      CPA_PRODUCTS$Supply <- CPA_PRODUCTS$Supply %>%
        dplyr::arrange(PRODUCT_INDUSTRY_CODE)
      
      CPA_PRODUCTS$Use_Purchaser <- CPA_PRODUCTS$Use_Purchaser %>%
        dplyr::arrange(PRODUCT_INDUSTRY_CODE)
      
      CPA_PRODUCTS$Use_VAT <- CPA_PRODUCTS$Use_VAT %>%
        dplyr::arrange(PRODUCT_INDUSTRY_CODE)
      
      CPA_PRODUCTS$Use_Basic <- CPA_PRODUCTS$Use_Basic %>%
        dplyr::arrange(PRODUCT_INDUSTRY_CODE)

      # 1.2 Domestic Share matrix -----
      
      # Creating of a domestic share matrix, extracting names of Products and Industries and arrange by Product industry code
      
      
      CPA_PRODUCTS$Dom_Share <- CPA_PRODUCTS$Supply %>%
        dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
        dplyr::arrange(PRODUCT_INDUSTRY_CODE)
      
       'Estimation of Domestic share of output (commodity).Pmax function is used as function which task is to skipped NaN values'
   
      CPA_PRODUCTS$Dom_Share$Domestic_share_of_output_commodity <- 
                pmax(1-CPA_PRODUCTS$Use_Basic$Exports_FOB/CPA_PRODUCTS$Supply$Total_output,0)
      
      
      ' Do ovde da se prodolzi so objasnuvanja na celoto'
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
      

      # New 
      CPA_PRODUCTS$K_Shift_Map$Share_of_K_in_HH_FD[grepl("Constructions and construction works", CPA_PRODUCTS$K_Shift_Map$PRODUCT_INDUSTRY_NAME)] =
      #CPA_PRODUCTS$K_Shift_Map$Share_of_K_in_HH_FD[grepl("Construction services", CPA_PRODUCTS$K_Shift_Map$PRODUCT_INDUSTRY_NAME)] = 
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
      
      ' Is missing ??? 
       Effective VAT Rate in FD
       Implied Max HH Share'
      
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
      
      

      # 1.6 Use_K_Dom_NetPurch----------------------------------------------

      
      #  ----- OLD
      # USE_K_DOM_NETPURCH <- USE_K_NETPURCH %>%
      #   dplyr::select(PRODUCT_INDUSTRY_NAME, PRODUCT_INDUSTRY_CODE, INDUSTRY_NAME, INDUSTRY_CODE, value) %>%
      #   merge.data.frame(NACE_INDUSTRIES$Dom_Share, key = "INDUSTRY_CODE") %>%
      #   dplyr::mutate(value = value*Domestic_Share_of_Output_Industry) %>%
      #   dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)


# NEW (SEGA NOVA GRESKA OVDE POJAVUVA !!! SEGA GRESKATA VEROJATNO E VO DOMESTIV SHARE )
# DO TUKA !!!!
      
      USE_K_DOM_NETPURCH_1 <- USE_K_NETPURCH %>%
        dplyr::select(PRODUCT_INDUSTRY_NAME, PRODUCT_INDUSTRY_CODE, INDUSTRY_NAME, INDUSTRY_CODE, value) 
      
      
      USE_K_DOM_NETPURCH_2<-NACE_INDUSTRIES$Dom_Share
      
      
      USE_K_DOM_NETPURCH_merged<-inner_join(USE_K_DOM_NETPURCH_1,USE_K_DOM_NETPURCH_2,by = c("INDUSTRY_CODE"="INDUSTRY_CODE"))
      
      USE_K_DOM_NETPURCH<-USE_K_DOM_NETPURCH_merged %>%
        dplyr::mutate(value = value*Domestic_Share_of_Output_Industry) %>%
        dplyr::rename(c("INDUSTRY_NAME"="INDUSTRY_NAME.x"))%>%
        dplyr::select(-c("INDUSTRY_NAME.y"))%>%
        dplyr::select("INDUSTRY_NAME","INDUSTRY_CODE","PRODUCT_INDUSTRY_NAME","PRODUCT_INDUSTRY_CODE","value","Domestic_Share_of_Output_Industry")%>%
        dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
      

    # # 
      ## Missing  
      ## Ne e vnesensena vo presmetkata 
      # INDUSTRY_CODE=85, 86 i 93
      
      # Najdena e greskata !!!! Treba ovie tri da se dodadat vo presmetkata !!!!
      # Ovde nedostasuvaat  unique(USE_K_DOM_NETPURCH$INDUSTRY_CODE)
      
      # test<-USE_K_DOM_NETPURCH%>%
      #   dplyr::filter(INDUSTRY_CODE=='86')%>%
      #   dplyr::select(INDUSTRY_CODE,value)
      # 
      # sum(test$value,na.rm = TRUE)

      
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
      
      
#})    

    
      
      # We use the data from "Use_K_NetPurch" instead of "Use_K_Dom_NetPurch" to avoid redundancies because it is the same data.