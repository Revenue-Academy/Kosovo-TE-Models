setwd(path)
getwd()

wb <- createWorkbook()

addWorksheet(wb, "Excise_TE_Chapters")
addWorksheet(wb, "Excise_TE_MTN")
addWorksheet(wb, "ExciseMineralOils_TE")
addWorksheet(wb, "TobaccoProducts_TE")
addWorksheet(wb, "AlcoholProducts_TE")
addWorksheet(wb, "Estimation_Excise_TE_HS")
addWorksheet(wb, "MainResultsExciseFinal")



# Name of worksheet in Excel

writeData(wb, "Excise_TE_Chapters", Excise_TE_Chapters, startRow = 1, startCol = 1)
writeData(wb, "Excise_TE_MTN", Excise_TE_MTN, startRow = 1, startCol = 1)
writeData(wb, "ExciseMineralOils_TE", ExciseMineralOils_TE, startRow = 1, startCol = 1)
writeData(wb, "TobaccoProducts_TE", TobaccoProducts_TE, startRow = 1, startCol = 1)
writeData(wb, "AlcoholProducts_TE", AlcoholProducts_TE, startRow = 1, startCol = 1)
writeData(wb, "Estimation_Excise_TE_HS", Estimation_Excise_TE_HS, startRow = 1, startCol = 1)
writeData(wb, "MainResultsExciseFinal", MainResultsExciseFinal, startRow = 1, startCol = 1)

# Export and save 
saveWorkbook(wb, file = "Excise_TE.xlsx", overwrite = TRUE)


gc(TRUE)
setwd(path1)
getwd()

setwd(path)
getwd()

wb <- createWorkbook()

addWorksheet(wb, "Excise_TE_Chapters")
addWorksheet(wb, "Excise_TE_MTN")
addWorksheet(wb, "ExciseMineralOils_TE")
addWorksheet(wb, "TobaccoProducts_TE")
addWorksheet(wb, "AlcoholProducts_TE")
addWorksheet(wb, "Estimation_Excise_TE_HS")
addWorksheet(wb, "MainResultsExciseFinal")



# Name of worksheet in Excel

writeData(wb, "Excise_TE_Chapters", Excise_TE_Chapters, startRow = 1, startCol = 1)
writeData(wb, "Excise_TE_MTN", Excise_TE_MTN, startRow = 1, startCol = 1)
writeData(wb, "ExciseMineralOils_TE", ExciseMineralOils_TE, startRow = 1, startCol = 1)
writeData(wb, "TobaccoProducts_TE", TobaccoProducts_TE, startRow = 1, startCol = 1)
writeData(wb, "AlcoholProducts_TE", AlcoholProducts_TE, startRow = 1, startCol = 1)
writeData(wb, "Estimation_Excise_TE_HS", Estimation_Excise_TE_HS, startRow = 1, startCol = 1)
writeData(wb, "MainResultsExciseFinal", MainResultsExciseFinal, startRow = 1, startCol = 1)

# Export and save 
saveWorkbook(wb, file = "Excise_TE.xlsx", overwrite = TRUE)


gc(TRUE)
setwd(path1)
getwd()


print("Simulation is done")        