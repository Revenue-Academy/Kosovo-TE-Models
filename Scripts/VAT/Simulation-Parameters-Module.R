# I.Importing libraries ------------------------------------------------

library(rccmisc) 
library(rpivotTable)
library(reshape2)
options(warn=-1)
library(sm)
library(ks)
library(plyr)
library(openxlsx)

# II. Simulation parameters ------------------------------------------------

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
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)
  

SIMULATION<-TAXABLE_PROPORTION_IMPORT

RC_prc_of_Constructions_and_construction_works = 0

vat_rate_on_residential_construction = 0.18


# # Base for calculation 
SIMULATION_CALIBRATION_FACTOR<-SIMULATION


# Extract column names from SIMULATION
column_names <- colnames(SIMULATION_CALIBRATION_FACTOR)



