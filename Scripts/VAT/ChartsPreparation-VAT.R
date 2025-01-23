
# I.Dashboard-VAT metrics -----------------------------------------------------------
suppressMessages({

      # Chart 1.VAT Revenue Comparison -----------------------------------------------------------------
  
    # vat_rev_tbl_bu<-MainResultsVATFinal_BU%>%
    #     dplyr::filter(Descriptions %in% c("Benchmark VAT","Uncalibrated VAT","Calibrated VAT"))
  
  vat_gap_metrics_tbl<-forecast_combined_agg%>%
                #dplyr::filter(year==SimulationYear)%>%
                dplyr::filter(year==2022)%>%
                dplyr::filter(scenario=='Baseline')%>%
                dplyr::filter(Descriptions %in% c("Benchmark VAT","Uncalibrated VAT","Calibrated VAT"))%>%
                dplyr::select(-c(scenario,year))
 
      # Chart 2.VAT GAP Comparison (in LCU Millions)--------------------------------
    
      # 
      # vat_gap_tbl_bu<-MainResultsVATFinal_BU%>%
      #   dplyr::filter(Descriptions %in% c("Policy Gap","Compliance Gap"))
      # 
      #   vat_gap_tbl_bu$label<-"VAT"
  
  
  
  
  # Simulation scenario 22/1/2025- Simulation
  
          # vat_gap_metrics_tbl_treemap<-forecast_combined_agg%>%
          #   dplyr::filter(year==SimulationYear)%>%
          #   dplyr::filter(scenario=='Simulation')%>%
          #   dplyr::filter(Descriptions %in% c("Policy Gap","Compliance Gap"))%>%
          #   dplyr::select(-c(scenario,year))
          # 
          # vat_gap_metrics_tbl_treemap$label<-"VAT Gap Comparison"

  # New 
          vat_gap_metrics_tbl_treemap<-forecast_combined_agg%>%
            dplyr::filter(year==2022)%>%
            dplyr::filter(scenario=='Baseline')%>%
            dplyr::filter(Descriptions %in% c("Policy Gap","Compliance Gap"))%>%
            dplyr::select(-c(scenario,year))
          
          vat_gap_metrics_tbl_treemap$label<-"VAT Gap Comparison"
  
  
      
      # Chart 3.Structure of VAT by sector ----------------------------------------------
      
        
        # vat_rev_agg_pie_bu<-Revenue_VAT_TOTAL_BU%>%
        #   dplyr::select(-c("PRODUCT_INDUSTRY_CODE","Final_Demand_Total"))%>%
        #   dplyr::summarise(
        #                 Businesses=sum(Total_Revenues_from_Intermediate_Inputs,na.rm = TRUE),
        #                 NPISHs=sum(Final_Demand_NPISH,na.rm = TRUE),
        #                 Households=sum(Final_Demand_HH ,na.rm = TRUE),
        #                 Government=sum(Final_Demand_Government,na.rm = TRUE))%>%
        #   melt()
        
  vat_sectors_pie_tbl<-forecast_sim_cpa%>%
    dplyr::filter(year==SimulationYear)%>%
    dplyr::select(-c("PRODUCT_INDUSTRY_CODE","PRODUCT_INDUSTRY_NAME"))%>%
    dplyr::select(-c("value",
                     "scenario"))%>%
    dplyr::summarise(
      Businesses=sum(Total_Revenues_from_Intermediate_Inputs,na.rm = TRUE),
      Households=sum(Final_Demand_HH,na.rm = TRUE),
      NPISHs=sum(Final_Demand_NPISH,na.rm = TRUE),
      Government=sum(Final_Demand_Government,na.rm = TRUE))%>%
    melt()
  
      # Chart 4.Breakdown of VAT by Sector ---------------------------------------------
      
        
        # vat_rev_agg_bu<-Revenue_VAT_TOTAL_BU%>%
        #   dplyr::select(c("PRODUCT_INDUSTRY_CODE","PRODUCT_INDUSTRY_NAME",
        #            "Final_Demand_Total"))%>%
        #   dplyr::group_by(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME)%>%
        #   dplyr::summarise(
        #     VAT=sum(Final_Demand_Total,na.rm = TRUE)
        #   )%>%
        #   melt()
  
  # vat_sectors_treemapl_tbl<-forecast_sim_cpa%>%
  #   dplyr::filter(year==SimulationYear)%>%
  #   dplyr::select(-c("year","Total_Revenues_from_Intermediate_Inputs","Final_Demand_HH","Final_Demand_NPISH","Final_Demand_Government",
  #                    "scenario"))%>%
  #   
  #   melt()
  #       
          
          vat_sectors_structure_tbl<-forecast_combined_cpa_selected%>%
            dplyr::filter(year==SimulationYear)%>%
            dplyr::filter(scenario=='Simulation')%>%
            #dplyr::select(-c("PRODUCT_INDUSTRY_CODE","PRODUCT_INDUSTRY_NAME"))%>%
            dplyr::select(-c(
              "scenario"))#%>%
          # dplyr::rename("Businesses"="Total_Revenues_from_Intermediate_Inputs",
          #               "Households"="Final_Demand_HH",
          #               "NPISHs"="Final_Demand_NPISH",
          #               "Government" ="Final_Demand_Government")
          
          # Inspect the Total column and percentages
          vat_sectors_normalized <- vat_sectors_structure_tbl %>%
            mutate(
              #Total = Businesses_VAT + Households_VAT + NPISHs + Government,  # Calculate total for each row
              Businesses_pct = (Businesses_VAT / Total_VAT) * 100,            # Convert to percentage
              Households_pct = (Households_VAT / Total_VAT) * 100,
              NPISHs_pct = (NPISH_VAT / Total_VAT) * 100,
              Government_pct = (Goverment_VAT / Total_VAT) * 100
            )
          
          # Check if the sum of percentages equals 100 for each row
          vat_sectors_normalized <- vat_sectors_normalized %>%
            mutate(Percentage_Check = Businesses_pct + Households_pct + NPISHs_pct + Government_pct)
          
          # Display rows where percentages do not add up to 100
          vat_sectors_normalized %>%
            filter(abs(Percentage_Check - 100) > 0.01)
          
          
          vat_sectors_normalized <- vat_sectors_normalized %>%
            mutate(
              #Total = Businesses + Households + NPISHs + Government,
              Businesses_pct = (Businesses_VAT / Total_VAT) * 100,
              Households_pct = (Households_VAT / Total_VAT) * 100,
              NPISHs_pct = (NPISH_VAT / Total_VAT) * 100,
              Government_pct = (Goverment_VAT / Total_VAT) * 100,
              Adjusted_Factor = 100 / (Businesses_pct + Households_pct + NPISHs_pct + Government_pct),
              Businesses_pct = Businesses_pct * Adjusted_Factor,
              Households_pct = Households_pct * Adjusted_Factor,
              NPISHs_pct = NPISHs_pct * Adjusted_Factor,
              Government_pct = Government_pct * Adjusted_Factor
            )      
          
      
        
# II.Dashboard- Sectoral Distribution of VAT Revenues ---------------------------------------------------------------------
      # Chart 1.Final Demand by Product Industry Code -----------------------------------------------------------------
      
              # Business as usual !! CPA_PRODUCTS_0
              vat_nace_tbl_total_bu<-CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev%>%
                       dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_Total)
              
              # Simulation
               vat_nace_tbl_total_sim<-CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev%>%
                 dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_Total)

               # Merging data for Charts
               vat_nace_tbl_total_bu <- vat_nace_tbl_total_bu %>%
                 dplyr::mutate(Scenario = "Baseline")
               
               vat_nace_tbl_total_sim <- vat_nace_tbl_total_sim %>%
                 dplyr::mutate(Scenario = "Simulation")
               
               # Combine the two tables
               vat_nace_tbl_total_combined <- dplyr::bind_rows(vat_nace_tbl_total_bu, vat_nace_tbl_total_sim)
               
              
        
      # Chart 2.Breakdown of VAT revenues by Sector, Generated by Businesses--------------------------------------------------
      
              # Business as usual !! CPA_PRODUCTS_0
              vat_nace_tbl_businesses_bu<-CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev%>%
                dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_Revenues_from_Intermediate_Inputs)
              
               
               # Business as usual !! CPA_PRODUCTS_0
               vat_nace_tbl_businesses_sim<-CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev%>%
                 dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_Revenues_from_Intermediate_Inputs)
               
               
               # Merging data for Charts
               vat_nace_tbl_businesses_bu <- vat_nace_tbl_businesses_bu %>%
                 dplyr::mutate(Scenario = "Baseline")
               
               vat_nace_tbl_businesses_sim <- vat_nace_tbl_businesses_sim %>%
                 dplyr::mutate(Scenario = "Simulation")
               
               # Combine the two tables
               vat_nace_tbl_businesses_combined <- dplyr::bind_rows(vat_nace_tbl_businesses_bu, vat_nace_tbl_businesses_sim)
               
      # Chart 3.Breakdown of VAT Revenues by Sector, Generated by Households---------------------------------------
      
              # Business as usual !! CPA_PRODUCTS_0
              vat_nace_tbl_hh_bu<-CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev%>%
                dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_HH)
               
               
               # # Business as usual !! CPA_PRODUCTS_0
               vat_nace_tbl_hh_sim<-CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev%>%
                 dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_HH)
              
               
               
               # Merging data for Charts
               vat_nace_tbl_hh_bu <- vat_nace_tbl_hh_bu %>%
                 dplyr::mutate(Scenario = "Baseline")
               
               vat_nace_tbl_hh_sim <- vat_nace_tbl_hh_sim %>%
                 dplyr::mutate(Scenario = "Simulation")
               
               # Combine the two tables
               vat_nace_tbl_hh_combined <- dplyr::bind_rows(vat_nace_tbl_hh_bu, vat_nace_tbl_hh_sim)
               
               
      # Chart 4.Breakdown of VAT Revenues by Sector, Generated by Government--------------------------------
      
              # Business as usual !! CPA_PRODUCTS_0
              vat_nace_tbl_gov_bu<-CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev%>%
                dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_Government)
              
             
               vat_nace_tbl_gov_sim<-CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev%>%
                 dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_Government)
               
               
               # Merging data for Charts
               vat_nace_tbl_gov_bu <- vat_nace_tbl_gov_bu %>%
                 dplyr::mutate(Scenario = "Baseline")
               
               vat_nace_tbl_gov_sim <- vat_nace_tbl_gov_sim %>%
                 dplyr::mutate(Scenario = "Simulation")
               
               # Combine the two tables
               vat_nace_tbl_gov_combined <- dplyr::bind_rows(vat_nace_tbl_gov_bu, vat_nace_tbl_gov_sim)
               

# III. --------------------------------------------------------------------

             
  
})               
               
               print("Script Charts Preparation Done !")
               
              
               
               
               
        