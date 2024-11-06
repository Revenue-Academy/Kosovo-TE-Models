library(tidyverse)
library(readxl)

# 1.CA Data ---------------------------------------------------------------
#customs_raw <- read_excel("Carinski Davacki_2021.xlsx")

customs_raw <-read_excel("Data/Customs/Carinski Davacki_2019.xlsx")

# Adding desegregation by HS codes
customs_raw_en<- mutate(customs_raw,
                         Chapter = substr(hs_10, 1, 2),
                         Four_digit = substr(hs_10, 1, 4),
                         Six_digit = substr(hs_10, 1, 6),
                         Eight_digit = paste0(substr(hs_10, 1, 4),
                                              "",
                                              substr(hs_10, 5, 7),
                                              " ",
                                              substr(hs_10, 8, 9)))



# 2.SSO-Data ----------------------------------------------------------------
'Import name of English'
 # names_descriptions_en<- read_excel("Data/Customs/NomenclatureCustomTariff.xls")%>%
 #                         dplyr::rename("hs_10"="tarif",
 #                                       "description"="name",
 #                                       "supplementary_unit"="em")%>%
 #                                   select(-c("del"))%>%
 #                                   select(hs_10,description,supplementary_unit)

names_descriptions_en<- read_excel("Data/Customs/WB_2019_CPA_TARIFF.xlsx")%>%
  dplyr::rename("hs_10"="tarifa"
                )%>%
  dplyr::select(-c(neto_kg,regular_import_us))

 
 # customs_subset_names<-left_join(customs_raw,names_descriptions_en,by=c("hs_10"))
 
 
# 3.Tariff rates ------------------------------------------

tariff_rates2019 <- read.csv("~/Models/Simulation-Model/Data/Customs/tariff_rates2019.csv")%>%
                dplyr::select(NEW_10_DIGITS,RATE,SPECIFIC_RATE,MAXIMUM_RATE)%>%
                dplyr::rename(
                              'hs_10'='NEW_10_DIGITS',
                              'CustomsRate'='RATE',
                              'SpecificRate'='SPECIFIC_RATE',
                              'MaximumRate'='MAXIMUM_RATE'
                                )
  
 
 

# 4.HS sections and chapters descriptions -----------------------------------------------------------

 hs_descriptions <- read_excel("Data/Customs/WTO_HS.xlsx")

 
 
 
# 3.WTO ---------------------------------------------------------------------

WTO_HS <- read_excel("Data/Customs/WTO_HS.xlsx", 
                        sheet = "WTO_HS_1")
  
WTO_HS_subset<-WTO_HS%>%
  dplyr::filter(HS_year=="<2022")%>%
  dplyr::select(Six_digit,Product_group)


View(WTO_HS_subset)



# 4.Merging of data ------------------------------------------------------
# Set scipen to a high value
options(scipen = 999)
'This include only non-preferential origin !!!'

customs_subset_final<-customs_raw_en%>%
          dplyr::left_join(tariff_rates2019,by=c("hs_10"))%>%
          dplyr::left_join(names_descriptions_en,by=c("hs_10"))%>%
          dplyr::left_join(hs_descriptions,by=c("Chapter"))%>%
          dplyr::left_join(WTO_HS_subset,by=c("Six_digit"))%>%
  distinct()%>%
  dplyr::filter(importRegime=="non-preferential")%>%
  na.omit()%>%
  mutate(description_hs = paste(hs_10, description_hs, sep = "-"))%>%
  mutate(Section_description = paste(Sections, Section_description, sep = "-"))%>%
  mutate(Chapters_description = paste(Chapter, Chapters_description, sep = "-"))

# adding concated names



   
# sum(customs_subset_final$statisticalValue)-937543591180
# [1] 1627446
# Small difference !!!

str(customs_subset_final)

View(customs_subset_final)

# da se prodolzi so concatenate za da se spojat za vo Shiny


write.csv(customs_subset_final,"customs_subset_final4.csv")

