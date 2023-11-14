'Export-Module'

#  EXPORT RESULTS IN EXCEL -----------------------------------------------
    setwd(path)
      getwd()
  
      wb <- createWorkbook()
                            addWorksheet(wb, "CustomsDuties_TE_Sections")
                            addWorksheet(wb, "CustomsDuties_TE_Chapters")
                            addWorksheet(wb, "CustomsDuties_TE_MTN")
                            addWorksheet(wb, "CustomsDuties_TE_agg_countries")
                            addWorksheet(wb, "CustomsDuties_TE_agg_HS_subset")
                            addWorksheet(wb, "MainResultsFinal")
                            
                            

    # Name of worksheet in Excel
      writeData(wb, "CustomsDuties_TE_Sections", CustomsDuties_TE_Sections, startRow = 1, startCol = 1)
      writeData(wb, "CustomsDuties_TE_Chapters", CustomsDuties_TE_Chapters, startRow = 1, startCol = 1)
      writeData(wb, "CustomsDuties_TE_MTN", CustomsDuties_TE_MTN, startRow = 1, startCol = 1)
      writeData(wb, "CustomsDuties_TE_agg_countries", CustomsDuties_TE_agg_countries, startRow = 1, startCol = 1)
      writeData(wb, "CustomsDuties_TE_agg_HS_subset", CustomsDuties_TE_agg_HS_subset, startRow = 1, startCol = 1)
      writeData(wb, "MainResultsFinal", MainResultsFinal, startRow = 1, startCol = 1)

      # Export and save 
      saveWorkbook(wb, file = "CustomsDuties_TE.xlsx", overwrite = TRUE)
      

      gc(TRUE)
      setwd(path1)
      getwd()
      
    
      
      print("Simulation is done")
