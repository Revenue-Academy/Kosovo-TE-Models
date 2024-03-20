# I.Importing libraries ------------------------------------------------

#library(DataEditR)
library(rccmisc) 
#library(knitr)
library(rpivotTable)
library(reshape2)
options(warn=-1)
library(sm)
library(ks)
library(plyr)
library(openxlsx)

# II. Simulation parameters ------------------------------------------------
# getwd()
# load(file=".RData") 

standard_VAT_rate = 0.18
preferential_VAT_rate = 0.08


Benchmark_tax_rate_bu <- 0.18
Standard_VAT_rate_bu <-0.18
Preferential_VAT_rate_bu <- 0.08

# Tax rates for simulation
Benchmark_tax_rate <- VAT_benchmark_tax_rate


TE_EXEMPT<-0.00
TE_REDUCED_RATE<-0.00

# New
# SIMULATION[is.na(SIMULATION)] <- 0
TAXABLE_PROPORTION_IMPORT$Simulation_Toggles_Exempt<-as.numeric(TAXABLE_PROPORTION_IMPORT$Simulation_Toggles_Exempt)
TAXABLE_PROPORTION_IMPORT$Simulation_Toggles_Reduced_Rate<-as.numeric(TAXABLE_PROPORTION_IMPORT$Simulation_Toggles_Reduced_Rate)

# New  
  TAXABLE_PROPORTION_IMPORT<-TAXABLE_PROPORTION_IMPORT%>%
  #dplyr::select("...1","...2","...10","...11","...12")%>%
  # dplyr:: rename(c("PRODUCT_INDUSTRY_CODE"= "CPA",
  #                  "Products(CPA)"="Products (CPC 1.1)",
  #                  "Current_Policy_Exempt"="Exempt Share",
  #                  "Current_Policy_Reduced_Rate"= "Reduced Rate Share",
  #                  "Current_Policy_Fully_Taxable"="Fully Taxable Share"))%>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)
  

# TAXABLE_PROPORTION_IMPORT<-TAXABLE_PROPORTION_IMPORT%>%
#   dplyr::select(PRODUCT_INDUSTRY_CODE,`Products(CPA)`,Current_Policy_Exempt,Current_Policy_Reduced_Rate,Current_Policy_Fully_Taxable)

#Original
#TAXABLE_PROPORTION_IMPORT[2:4]<-as.numeric(unlist(TAXABLE_PROPORTION_IMPORT[2:4]))

#TAXABLE_PROPORTION_IMPORT[3:5]<-as.numeric(unlist(TAXABLE_PROPORTION_IMPORT[3:5]))

SIMULATION<-TAXABLE_PROPORTION_IMPORT
#SIMULATION$Current_Policy_Exempt[42] = 0 #  Imputed rents of owner-occupied dwellings with the industry code: 68A/ 68A

#SIMULATION$Standard_VAT_Rate[42] = 0 # This is the industry: Imputed rents of owner-occupied dwellings with the industry code: 68A/ 68A

# NEW
# SIMULATION$Standard_VAT_Rate = standard_VAT_rate
# SIMULATION$Preferential_VAT_Rate = preferential_VAT_rate
# SIMULATION$Simulation_Toggles_Exempt = NA
# SIMULATION$Simulation_Toggles_Reduced_Rate = NA

# SIMULATION$Simulation_Toggles_Exempt[SIMULATION$PRODUCT_INDUSTRY_CODE == "92"] = 1
# SIMULATION$Simulation_Toggles_Exempt[SIMULATION$PRODUCT_INDUSTRY_CODE == "93"] = 1

#RC_prc_of_Constructions_and_construction_works = 0.3
RC_prc_of_Constructions_and_construction_works = 0

vat_rate_on_residential_construction = 0.18


# # Base for calculation 
SIMULATION_CALIBRATION_FACTOR<-SIMULATION


# Extract column names from SIMULATION
column_names <- colnames(SIMULATION_CALIBRATION_FACTOR)

##
## Tri tabeli ovde se output

# Da se prodolzi so ovie ponatamu i da se dozavrsat do tuka !!!
# TAXABLE_PROPORTION_IMPORT (OVA E SAMO NA POCETOK NIGDE GO NEMA PONATAKA)
# SIMULATION (OVA E KOPIRANO POSLE VO  SIMULATION_1 A POTOA SE KORISTI ZA TE. Potoa SIMULATION se kopira vo SIMULATION_2
# Isto Simulation se koristi vo : Main estimation , Business as Usual,
# Simulation 2 se koristi za VAT EFFECTIVE RATE
# SIMULATION_CALIBRATION_FACTOR


# III. Interactive table -------------------------------------------------------


# SIMULATION<-SIMULATION %>%
# dplyr:: rename(c("CPA"="PRODUCT_INDUSTRY_CODE",
#                  "Products(CPA)"="Products(CPA)",
#                  "Exempt Share"="Current_Policy_Exempt",
#                  "Reduced Rate Share"= "Current_Policy_Reduced_Rate",
#                  "Fully Taxable Share"="Current_Policy_Fully_Taxable",
#                  "Standard VAT Rate"="Standard_VAT_Rate",
#                  "Reduced VAT Rate"="Preferential_VAT_Rate",
#                  "Exempt Proportion"="Simulation_Toggles_Exempt",
#                  "Reduced Rate Proportion"="Simulation_Toggles_Reduced_Rate"
#                     ))


# Download pic
#logo_pic <- 'https://seeklogo.com/images/T/the-world-bank-logo-73595059AD-seeklogo.com.png'



# SIMULATION<-data_edit(SIMULATION,logo =logo_pic,
#                       logo_size = 80,
#                       logo_side = "left",
#                       title="Input policy parametars",
#                       viewer_height = 1000,
#                       viewer_width = 4900,
#                       col_stretch = TRUE,
#                       col_readonly = c("CPA","Products(CPA)","Exempt Share","Reduced Rate Share","Fully Taxable Share")
#                       ) 



