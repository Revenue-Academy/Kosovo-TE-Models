'Export-Module'

#  EXPORT RESULTS IN EXCEL -----------------------------------------------
    setwd(path)
      getwd()
  
      wb <- createWorkbook()
      addWorksheet(wb, "Main_Results")
      addWorksheet(wb, "Revenue_VAT_TOTAL")
      addWorksheet(wb, "Result_Simulation")
      addWorksheet(wb, "Results_1")
      addWorksheet(wb, "Est_Rev1")
      addWorksheet(wb, "Est_Rev_BU")
      addWorksheet(wb, "revenue_vat_total_bu")
      addWorksheet(wb, "Est_Rev_BU_TE")
      addWorksheet(wb, "Supply_export")
      addWorksheet(wb, "Use_Purchaser_export")
      
    # Name of worksheet in Excel
      writeData(wb, "Main_Results", Export_Main_Results, startRow = 1, startCol = 1)
      writeData(wb, "Revenue_VAT_TOTAL", Revenue_VAT_TOTAL, startRow = 1, startCol = 1)
      writeData(wb, "Result_Simulation", Simulation_Results, startRow = 1, startCol = 1)
      writeData(wb, "Results_1", Simulation_Results_1, startRow = 1, startCol = 1)
      writeData(wb, "Est_Rev1", Est_Rev1, startRow = 1, startCol = 1)
      writeData(wb, "Est_Rev_BU", Est_Rev_BU, startRow = 1, startCol = 1)
      # NEw
      # Est_Rev_BU<-CPA_PRODUCTS_BU$Est_Rev
      # writeData(wb, "Est_Rev_BU", Est_Rev_BU, startRow = 1, startCol = 1)
     
      writeData(wb, "revenue_vat_total_bu", revenue_vat_total_bu, startRow = 1, startCol = 1)
      writeData(wb, "Est_Rev_BU_TE", Est_Rev_BU_TE, startRow = 1, startCol = 1)
      writeData(wb, "Supply_export", Supply_export, startRow = 1, startCol = 1)
      writeData(wb, "Use_Purchaser_export", Use_Purchaser_export, startRow = 1, startCol = 1)
     # writeData(wb, "effective_vat_rates", effective_vat_rates, startRow = 1, startCol = 1)   
    #  writeData(wb, "hbs", data4_hbs_wider_merged_deciles1, startRow = 1, startCol = 1)   
      # Export and save 
      saveWorkbook(wb, file = "export_data.xlsx", overwrite = TRUE)


  wb1 <- createWorkbook()
      addWorksheet(wb1, "simulation")
      addWorksheet(wb1, "NACE_NAMES")
      addWorksheet(wb1, "FINAL_VAT_REVENUES")

      # Name of worksheet in Excel
      writeData(wb1, "simulation", SIMULATION, startRow = 1, startCol = 1)
      writeData(wb1, "NACE_NAMES", NACE_NAMES, startRow = 1, startCol = 1)
      #writeData(wb1, "FINAL_VAT_REVENUES", FINAL_COMPARISON_VAT_REVENUES, startRow = 1, startCol = 1)
      # Export and save 
      saveWorkbook(wb1, file = "simulation.xlsx", overwrite = TRUE)


      
      print("Simulation is done")

      
      # New
      rm(TAXABLE_PROPORTION_IMPORT)
      
      gc(TRUE)
      setwd(path1)
      getwd()
      
      #rm(list=ls()[! ls() %in% c("path","path1")])
      #rm(list = ls()[!ls() %in% c("path", "path1", "SUPPLY", "USE_BASIC", "USE_PURCHASER", "USE_VAT", "TAXABLE_PROPORTION_IMPORT", "NACE_INDUSTRIES", "CPA_PRODUCTS")])

