" Data prep TE Dashboard "

# Start parallel processing
start.time <- proc.time()


# I.Customs duties --------------------------------------------------------
        # 1.Data preparation ------------------------------------------------------
                                # 1.1 Adding FreeTrade agreements --------------------------------------------
                                
                                Benchmark_Customs_Rate<-0.1
                                
                                # Estimation of tax expenditures 
                                CustomsDuties_base<-Import_raw_monthly%>%
                                  dplyr::select(HS_code,Description,iso2c,Month,Year,Value,Quantity,Netweight,CustomsRevenue,ExciseRevenue,VAT_Revenue)%>%
                                  dplyr::mutate(Effective_VAT_rate=round(VAT_Revenue/(Value+ExciseRevenue+CustomsRevenue),2),
                                                Effective_Customs_rate=round(CustomsRevenue/(Value),2),
                                                CustomsDuties_Benchmark=Value*Benchmark_Customs_Rate,
                                                CustomsDuties_TE=round(CustomsDuties_Benchmark-CustomsRevenue,1))
                                
                                # Merging with GeoDimension
                                CustomsDuties_base<-left_join(CustomsDuties_base,GeoDimension,by =c("iso2c"))
                                
                                # Replace NA values in CustomsDuties_TE with "FreeTradeAgreements" where NoFreeTradeAgreement is NA
                                CustomsDuties_base$FreeTradeAgreements[is.na(CustomsDuties_base$FreeTradeAgreements)] <- "NoFreeTradeAgreement"
                                CustomsDuties_base$HS_code_s <-gsub('(.{4})', '\\1 ', CustomsDuties_base$HS_code)
                                
                                #rm(split_columns_hs,split_columns_countries)
                                
                                
                                # 1.2 Adding FreeTrade agreements --------------------------------------------
                                
                                TreatmentOfGoods<-data.frame(FreeTradeAgreements=c("EU27","CEFTA","NoFreeTradeAgreement","TR","GBR"),
                                                             Treatment=c("Preferential","Preferential","NonPreferential",
                                                                         "Preferential","Preferential"))
                                
                                CustomsDuties_base<-left_join(CustomsDuties_base,TreatmentOfGoods,by =c("FreeTradeAgreements"))
                                
                                
                                CustomsDuties_base <- CustomsDuties_base %>%
                                  mutate(
                                    FreeTradeAgreements = ifelse(iso3c == 'MNE', 'CEFTA', FreeTradeAgreements),
                                    Treatment = ifelse(iso3c == 'MNE', 'Preferential', Treatment)
                                  )
                                
                                # Assuming your data frame is named CustomsDuties_base
                                CustomsDuties_base$FreeTradeAgreements <- ifelse(is.na(CustomsDuties_base$FreeTradeAgreements), 'NoFreeTradeAgreement', CustomsDuties_base$FreeTradeAgreements)
                                CustomsDuties_base$Treatment <- ifelse(is.na(CustomsDuties_base$Treatment), 'NonPreferential', CustomsDuties_base$Treatment)
                                
                                
                                
                                
                                
                                # 1.3 Estimation of Tax Expenditures for Customs duties -----------------------
                                # 1.4 Countries -----------------------------------------------------------
                                          
                                          CustomsDuties_TE_agg_countries<-CustomsDuties_base%>%
                                            dplyr::group_by(HS_code,HS_code_s,iso2c,iso3c,Treatment,countries)%>%
                                            dplyr::filter(Treatment=="NonPreferential")%>%
                                            dplyr::summarise(Value=sum(Value,na.rm = TRUE),
                                                             Quantity=sum(Quantity,na.rm = TRUE),
                                                             Netweight=sum(Netweight,na.rm = TRUE),
                                                             CustomsRevenue=sum(CustomsRevenue,na.rm = TRUE),
                                                             ExciseRevenue=sum(ExciseRevenue,na.rm = TRUE),
                                                             VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE),
                                                             CustomsDuties_Benchmark=sum(CustomsDuties_Benchmark,na.rm = TRUE),
                                                             CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                                          
                                          
                                          CustomsDuties_TE_agg_countries$HS_code<-NULL
                                          CustomsDuties_TE_agg_countries$HS_code_s<-NULL
                                          
                                          
                                          CustomsDuties_TE_agg_countries <- CustomsDuties_TE_agg_countries %>%
                                            dplyr::select(iso3c,CustomsDuties_TE)%>%
                                            dplyr::group_by(iso3c) %>%
                                            dplyr::summarise(across(where(is.numeric), sum))
                                          
                                          CustomsDuties_TE_agg_HS<-CustomsDuties_base%>%
                                            dplyr::group_by(HS_code,Treatment,HS_code_s)%>%
                                            dplyr::filter(Treatment=="NonPreferential")%>%
                                            dplyr::summarise(Value=sum(Value,na.rm = TRUE),
                                                             Quantity=sum(Quantity,na.rm = TRUE),
                                                             Netweight=sum(Netweight,na.rm = TRUE),
                                                             CustomsRevenue=sum(CustomsRevenue,na.rm = TRUE),
                                                             ExciseRevenue=sum(ExciseRevenue,na.rm = TRUE),
                                                             VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE),
                                                             CustomsDuties_Benchmark=sum(CustomsDuties_Benchmark,na.rm = TRUE),
                                                             CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                                          
                                          
                                          CustomsDuties_TE_agg_HS<-left_join(CustomsDuties_TE_agg_HS,taric_data,by =c("HS_code"))  
                                          
                                          
                                          
                                          # Adding desegregation by HS codes
                                          CustomsDuties_TE_agg_HS <- mutate(CustomsDuties_TE_agg_HS,
                                                                            Chapter = substr(HS_code_s, 1, 2),
                                                                            Four_digit = substr(HS_code_s, 1, 4),
                                                                            Six_digit = substr(HS_code_s, 1, 7),
                                                                            Eight_digit = paste0(substr(HS_code_s, 1, 4),
                                                                                                 "",
                                                                                                 substr(HS_code_s, 5, 7),
                                                                                                 " ",
                                                                                                 substr(HS_code_s, 8, 9)))
                                          
                                          
                                          
                                          
          # 2.Preparing data for charts and for the main table --------------------------------------------
                                # 2.1 Preparing data for Charts ---------------------------------------
                                # 2.1 Regular Import ------------------------------------------
                                
                                CustomsValue<-CustomsDuties_TE_agg_HS%>%
                                  dplyr::select(Chapter,Value)%>%
                                  dplyr::group_by(Chapter)%>%
                                  dplyr::summarise(Value=sum(Value,na.rm = TRUE))

                                CustomsValue<-left_join(CustomsValue,HS_Sections,by =c("Chapter"))%>%
                                  dplyr::select(Chapter,Chapters_description,Value)%>%
                                  dplyr::group_by(Chapter,Chapters_description)%>%
                                  dplyr::summarise(Value=sum(Value,na.rm = TRUE))


                                # Factor the "Sections" column without ordering other strings
                                CustomsValue$Chapter <- factor(CustomsValue$Chapter)

                                CustomsValue$HS<-"Chapters by Harmonized System "

                                
                                #2.2 Pie chart  --------------------------------------------------------------

                                CustomsDuties_base_pie<-CustomsDuties_base%>%
                                  dplyr::select(Treatment,Value)%>%
                                  dplyr::group_by(Treatment)%>%
                                  dplyr::summarise(Value=sum(Value,na.rm = TRUE))

                                CustomsDuties_base_pie<-melt(CustomsDuties_base_pie)

                                # 2.3 TE's by HS Type of products -----------------------------------------

                                CustomsDuties_TE_type_products<-CustomsDuties_TE_agg_HS%>%
                                  dplyr::select(Chapter,Treatment,CustomsDuties_TE)%>%
                                  dplyr::group_by(Chapter,Treatment)%>%
                                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))

                                # Select Agricultural
                                CustomsDuties_TE_Agricultural<-CustomsDuties_TE_type_products%>%
                                  dplyr::filter(Chapter %in% c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))

                                # Select Industrial
                                CustomsDuties_TE_Industrial<-CustomsDuties_TE_type_products%>%
                                  dplyr::filter(!Chapter %in% c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))

                                CustomsDuties_TE_Agricultural$type_products<-"AgriculturalProducts"

                                CustomsDuties_TE_Agricultural<-CustomsDuties_TE_Agricultural%>%
                                  select(Treatment,type_products,CustomsDuties_TE)%>%
                                  dplyr::group_by(Treatment,type_products)%>%
                                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))


                                CustomsDuties_TE_Industrial$type_products<-"Non_AgriculturalProducts"

                                CustomsDuties_TE_Industrial<-CustomsDuties_TE_Industrial%>%
                                  select(Treatment,type_products,CustomsDuties_TE)%>%
                                  dplyr::group_by(Treatment,type_products)%>%
                                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))



                                CustomsDuties_TE_type_products<-rbind(CustomsDuties_TE_Agricultural,CustomsDuties_TE_Industrial)
                                CustomsDuties_TE_type_products$Chapter<-NULL

                                CustomsDuties_TE_type_products<-melt(CustomsDuties_TE_type_products)

                                CustomsDuties_TE_type_products$Treatment<- factor(CustomsDuties_TE_type_products$Treatment)


                                # 2.4 TE's by HS Sections --------------------------------------------------------------------

                                CustomsDuties_TE_Sections<-CustomsDuties_TE_agg_HS%>%
                                  dplyr::select(Chapter,Treatment,CustomsDuties_TE)%>%
                                  dplyr::group_by(Chapter,Treatment)%>%
                                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))


                                CustomsDuties_TE_Sections<-left_join(CustomsDuties_TE_Sections,HS_Sections,by =c("Chapter"))%>%
                                  dplyr::select(Sections,Section_description,Treatment,CustomsDuties_TE)%>%
                                  dplyr::group_by(Sections,Section_description,Treatment)%>%
                                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))%>%
                                  data.table()

                                CustomsDuties_TE_Sections$Treatment<- factor(CustomsDuties_TE_Sections$Treatment)


                                # 2.5 TE's by Chapters ---------------------------------------------------------
                                CustomsDuties_TE_Chapters<-CustomsDuties_TE_agg_HS%>%
                                  dplyr::select(Chapter,Treatment,CustomsDuties_TE)%>%
                                  dplyr::group_by(Chapter,Treatment)%>%
                                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))

                                CustomsDuties_TE_Chapters<-left_join(CustomsDuties_TE_Chapters,HS_Sections,by =c("Chapter"))%>%
                                  dplyr::select(Chapter,Chapters_description,Treatment,CustomsDuties_TE)%>%
                                  dplyr::group_by(Chapter,Chapters_description,Treatment)%>%
                                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))

                                # Factor the "Sections" column without ordering other strings
                                CustomsDuties_TE_Chapters$Chapter <- factor(CustomsDuties_TE_Chapters$Chapter)

                                # 2.6 TE's by WTO classification (MTN) Categories -----------------------------------------

                                actual_year_simulation <- unique(Import_raw_monthly$Year)

                                # Multilateral Trade Negotiations (MTN) Categories, HS 2017
                                # Multilateral Trade Negotiations (MTN) categories were first defined in the Tokyo Round and adapted for the Harmonized System in the Uruguay Round.
                                # The product group breakdown in this publication deviates slightly from the previous definition, which was based on the HS 1992 nomenclature.

                                CustomsDuties_TE_MTN<-CustomsDuties_TE_agg_HS%>%
                                  dplyr::select(Six_digit,Treatment,CustomsDuties_TE)%>%
                                  dplyr::group_by(Six_digit,Treatment)%>%
                                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))

                                # New
                                WTO_MTN_subset <- WTO_MTN %>%
                                  filter(
                                    (actual_year_simulation >= 2022 & str_detect(HS_year, ">2022")) |
                                      (actual_year_simulation < 2022 & str_detect(HS_year, "<2022"))
                                  )

                                #CustomsDuties_TE_MTN<-left_join(CustomsDuties_TE_MTN,WTO_MTN,by =c("Six_digit"))%>%
                                CustomsDuties_TE_MTN<-left_join(CustomsDuties_TE_MTN,WTO_MTN_subset,by =c("Six_digit"))%>%
                                  dplyr::select(Product_group,Treatment,CustomsDuties_TE)

                                CustomsDuties_TE_MTN <- distinct(CustomsDuties_TE_MTN)

                                CustomsDuties_TE_MTN<-CustomsDuties_TE_MTN%>%
                                  dplyr::group_by(Product_group,Treatment)%>%
                                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))

                                CustomsDuties_TE_MTN$Product_group <- ifelse(is.na( CustomsDuties_TE_MTN$Product_group), "Other",  CustomsDuties_TE_MTN$Product_group)



                                # Factor the "Sections" column without ordering other strings
                                CustomsDuties_TE_MTN$Product_group <- factor( CustomsDuties_TE_MTN$Product_group)

                                #
                                # 2.7 TE's by Countries(Choropleth) -----------------------------------------------
                                
                                # Adding ISO 3 codes to names of countries from ggplot2
                                CustomsDuties_TE_agg_countries<-CustomsDuties_TE_agg_countries%>%
                                  #dplyr::mutate(CustomsDuties_TE=CustomsDuties_TE/1000000)
                                  dplyr::mutate(CustomsDuties_TE=CustomsDuties_TE)



                                # Extracting table for Plot
                                CustomsDuties_TE_agg_countries_tbl_agg<-CustomsDuties_TE_agg_countries%>%
                                  dplyr::arrange(desc(CustomsDuties_TE))




                               # CustomsDuties_TE_agg_countries_tbl<-left_join(CustomsDuties_TE_agg_countries_tbl,GeoDimension, by=c("iso3c"))%>%
                                CustomsDuties_TE_agg_countries_tbl<-left_join(CustomsDuties_TE_agg_countries_tbl_agg,GeoDimension, by=c("iso3c"))%>%
                                  select(iso3c,countries,CustomsDuties_TE)%>%
                                  dplyr::rename("Countries"="countries")


                                CustomsDuties_TE_agg_countries_tbl$CustomsDuties_TE<-round(CustomsDuties_TE_agg_countries_tbl$CustomsDuties_TE,3)

                                CustomsDuties_TE_agg_countries_tbl<-CustomsDuties_TE_agg_countries_tbl%>%
                                  dplyr::filter(CustomsDuties_TE>0)%>%
                                  na.omit()

                                mapdata3 <- left_join(mapdata_iso3c,CustomsDuties_TE_agg_countries,by = c("iso3"="iso3c"))

                                # Removing Antarctica and Nan values
                                mapdata3<-mapdata3[!(mapdata3$region=="Antarctica"),]


                                # 2.8 TE's by HS codes ---------------------------------------------------------
                                CustomsDuties_TE_agg_HS_subset<-CustomsDuties_base%>%
                                  dplyr::select(HS_code_s,Description,Treatment,CustomsDuties_TE)%>%
                                  dplyr::filter(Treatment=="NonPreferential")%>%
                                  dplyr::group_by(HS_code_s,Description,Treatment)%>%
                                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE))


                                CustomsDuties_TE_agg_HS_subset$CustomsDuties_TE<-round(CustomsDuties_TE_agg_HS_subset$CustomsDuties_TE,1)

                                CustomsDuties_TE_agg_HS_subset <- CustomsDuties_TE_agg_HS_subset %>%
                                  arrange(desc(CustomsDuties_TE))%>%
                                  dplyr::rename("HS_code"="HS_code_s")


                                CustomsDuties_TE_agg_HS_subset<-CustomsDuties_TE_agg_HS_subset %>%
                                  pivot_wider(names_from = Treatment, values_from = CustomsDuties_TE)

                                CustomsDuties_TE_agg_HS_subset$NonPreferential <- abs(round(CustomsDuties_TE_agg_HS_subset$NonPreferential, 0))


                                CustomsDuties_TE_agg_HS_subset<-CustomsDuties_TE_agg_HS_subset%>%
                                  dplyr::rename('TaxExpenditures'='NonPreferential' )


                                # # For treemap
                                # CustomsDuties_TE_agg_HS_treemap<-CustomsDuties_TE_agg_HS_subset
                                # CustomsDuties_TE_agg_HS_treemap$HS<-"HS codes"


                                # 2.9 TE's by CPA and Combined Nomenclature -------------------------------------------------
                                
                                # links https://circabc.europa.eu/ui/group/c1b49c83-24a7-4ff2-951c-621ac0a89fd8/library/d3056f31-a684-430a-a77d-12d1d0fdfffa?p=2&n=10&sort=modified_DESC
                                
                                
                                
                                # CustomsDuties_TE_CPA<-CustomsDuties_TE_agg_HS%>%
                                #   dplyr::select(Eight_digit,Treatment,CustomsDuties_TE)%>%
                                #   dplyr::filter(Treatment=="NonPreferential")%>%
                                #   dplyr::group_by(Eight_digit,Treatment)%>%
                                #   dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                                # 
                                # 
                                # CPA_CN_subset<-CPA_CN%>%
                                #   dplyr::filter(year==actual_year_simulation)
                                # 
                                # 
                                # 
                                # CustomsDuties_TE_CPA<-left_join(CustomsDuties_TE_CPA,CPA_CN_subset,by = c("Eight_digit"="CN_CODE"))%>%
                                #   dplyr::select(CPA_CODE,Treatment,CustomsDuties_TE)%>%
                                #   dplyr::group_by(CPA_CODE,Treatment)%>%
                                #   dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                                # 
                                # 
                                # CustomsDuties_TE_CPA <- distinct(CustomsDuties_TE_CPA)
                                # 
                                # 
                                # # Adding desegregation by CPA codes
                                # CustomsDuties_TE_CPA <- mutate(CustomsDuties_TE_CPA,
                                #                                CPA_CODE2 = substr(CPA_CODE, 1, 2))
                                # 
                                # 
                                # # Preparing subset for table with CPA and NACE section
                                # CustomsDuties_TE_CPA<-CustomsDuties_TE_CPA%>%
                                #   dplyr::group_by(CPA_CODE2,Treatment)%>%
                                #   dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                                # 
                                # 
                                # CPA_NACE_subset<-CPA_NACE%>%
                                #   dplyr::select(CPA_CODE2,CPA21_NAME,Heading_CPA,Sector)%>%
                                #   na.omit()
                                # 
                                # CPA_NACE_subset$CPA_CODE2<-as.double(CPA_NACE_subset$CPA_CODE2)
                                # CustomsDuties_TE_CPA$CPA_CODE2<-as.double(CustomsDuties_TE_CPA$CPA_CODE2)
                                # 
                                # CustomsDuties_TE_CPA<-left_join(CustomsDuties_TE_CPA,CPA_NACE_subset,by = c("CPA_CODE2"="CPA_CODE2"))
                                # 
                                # 
                                # CustomsDuties_TE_SECTORS<-CustomsDuties_TE_CPA%>%
                                #   dplyr::select(Sector,CPA21_NAME,CustomsDuties_TE,Treatment)%>%
                                #   dplyr::group_by(Sector,CPA21_NAME,Treatment)%>%
                                #   dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                                # 
                                # 
                                # CustomsDuties_TE_SECTORS$CPA21_NAME <- ifelse(is.na( CustomsDuties_TE_SECTORS$CPA21_NAME), "Other",  CustomsDuties_TE_SECTORS$CPA21_NAME)
                                # CustomsDuties_TE_SECTORS$Sector <- ifelse(is.na( CustomsDuties_TE_SECTORS$Sector), "Other",  CustomsDuties_TE_SECTORS$Sector)
                                # 
                                # 
                                # 
                                

          # 3. Main table with Simulation Results ---------------------------------------------------

                                CustomsDuties_base_non_preferential<-CustomsDuties_base%>%
                                  dplyr::filter(Treatment=="NonPreferential")
                                
                                MainResultsCustoms1<-MacroFiscalData%>%
                                  #dplyr::filter(Year==actual_year_simulation)%>%
                                  dplyr::filter(Year==base_year)%>%
                                  
                                  dplyr::mutate(
                                    # Total
                                    `Actual Total Import`=sum(CustomsDuties_base$Value,na.rm=TRUE)/1e+06, 
                                    `Actual Total Customs Revenues`=sum(CustomsDuties_base$CustomsRevenue,na.rm=TRUE)/1e+06, 
                                    `Actual Total Effective Customs Rate (%)`=sum(CustomsDuties_base$CustomsRevenue,na.rm=TRUE)/sum(CustomsDuties_base$Value,na.rm=TRUE)*100,
                                    # Estimated tax expenditures
                                    `Import(without FTA)`=sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)/1e+06, 
                                    `Actual Customs Revenues(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsRevenue,na.rm=TRUE)/1e+06, 
                                    `Effective Customs Rate (%) (without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsRevenue,na.rm=TRUE)/sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)*100,
                                    
                                    `Customs Revenue Benchmark(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsDuties_Benchmark,na.rm=TRUE)/1e+06,
                                    `Benchmark Effective Customs Rate(without FTA)`=`Customs Revenue Benchmark(without FTA)`/(sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)/1e+06)*100,
                                    `Tax Expenditures(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsDuties_TE,na.rm=TRUE)/1e+06,
                                    `Tax Expenditures(without FTA) as % of GDP`=(`Tax Expenditures(without FTA)`/GDP)*100,
                                    `Tax Expenditures(without FTA) as % of import FOB`=(`Tax Expenditures(without FTA)`/ImportsOfGoods_FOB)*100,
                                    `Tax Expenditures(without FTA) as % of Government Revenue`=(`Tax Expenditures(without FTA)`/GeneralGovernmentRevenue)*100,
                                    `Tax Expenditures (as % of Taxes On Products)`=(`Tax Expenditures(without FTA)`/TaxesOnProducts)*100,
                                    `Tax Expenditures (as % of CustomsRevenue)`=(`Tax Expenditures(without FTA)`/ImportDuties)*100)%>%
                                  dplyr::select(
                                    `Actual Total Import`,
                                    `Actual Total Customs Revenues`,
                                    `Actual Total Effective Customs Rate (%)`,
                                    `Import(without FTA)`,
                                    `Actual Customs Revenues(without FTA)`,
                                    `Effective Customs Rate (%) (without FTA)`,
                                    `Customs Revenue Benchmark(without FTA)`,
                                    `Benchmark Effective Customs Rate(without FTA)`,
                                    `Tax Expenditures(without FTA)`,
                                    `Tax Expenditures(without FTA) as % of GDP`,
                                    `Tax Expenditures(without FTA) as % of import FOB`,
                                    `Tax Expenditures(without FTA) as % of Government Revenue`,
                                    `Tax Expenditures (as % of Taxes On Products)`,
                                    `Tax Expenditures (as % of CustomsRevenue)`)
                                
                                
                                
                                MainResultsCustoms1<-melt(MainResultsCustoms1)
                                
                                MainResultsCustoms2<-MainResultsCustoms1
                                MainResultsCustoms1$value<-round(MainResultsCustoms1$value,2)
                                
                                
                                MainResultsFinal<-MainResultsCustoms1%>%
                                  dplyr::rename("Description"= "variable",
                                                "Value"="value")
                                
                                Estimation_TE <- data.frame(
                                  DataSet = character(),  # Replace with appropriate data type if not character
                                  Value = numeric(),
                                  ExciseRevenue=numeric()# Replace with appropriate data type if not numeric
                                )
                                
                                Excise_TE_Chapters<- data.frame(
                                  Chapter = character(),  # Replace with appropriate data type if not character
                                  Excise_TE = numeric(),
                                  ExciseRevenue=numeric()# Replace with appropriate data type if not numeric
                                )
                                
                                Excise_TE_MTN<- data.frame(
                                  Product_group = character(),  # Replace with appropriate data type if not character
                                  Excise_TE = numeric(),
                                  ExciseRevenue=numeric()# Replace with appropriate data type if not numeric
                                )
                                
                               
                                summary_TE_SIM<-MainResultsFinal
                                
          # 4. Projection of tax Expenditures  -----------------------------------------

                      
                      GDP_share_TE <- MainResultsCustoms2 %>%
                        filter(variable == "Tax Expenditures(without FTA) as % of GDP")
                      
                      ProjectionTE_customs<-MacroFiscalData%>%
                        #dplyr::filter(Year>=actual_year_simulation)%>%
                        dplyr::filter(Year>=base_year)%>%
                        select(Year,GDP)%>%
                        dplyr::mutate(tax_expenditures=GDP*GDP_share_TE$value)
                        

                      
# II. Excise ------------------------------------------------------------------
                      
        # Data prep ---------------------------------------------------------------

                      CustomsDuties_TE_agg_countries<-CustomsDuties_base%>%
                        dplyr::group_by(HS_code,HS_code_s,iso2c,iso3c,Treatment,countries)%>%
                        dplyr::summarise(Value=sum(Value,na.rm = TRUE),
                                         Quantity=sum(Quantity,na.rm = TRUE),
                                         Netweight=sum(Netweight,na.rm = TRUE),
                                         CustomsRevenue=sum(CustomsRevenue,na.rm = TRUE),
                                         ExciseRevenue=sum(ExciseRevenue,na.rm = TRUE),
                                         VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE),
                                         CustomsDuties_Benchmark=sum(CustomsDuties_Benchmark,na.rm = TRUE),
                                         CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                      
                      
                      CustomsDuties_TE_agg_countries$HS_code<-NULL
                      CustomsDuties_TE_agg_countries$HS_code_s<-NULL
                      
                      
                      CustomsDuties_TE_agg_countries <- CustomsDuties_TE_agg_countries %>%
                        dplyr::select(iso3c,CustomsDuties_TE)%>%
                        dplyr::group_by(iso3c) %>%
                        dplyr::summarise(across(where(is.numeric), sum))
                      
                      CustomsDuties_TE_agg_HS<-CustomsDuties_base%>%
                        dplyr::group_by(HS_code,Treatment,HS_code_s)%>%
                        dplyr::summarise(Value=sum(Value,na.rm = TRUE),
                                         Quantity=sum(Quantity,na.rm = TRUE),
                                         Netweight=sum(Netweight,na.rm = TRUE),
                                         CustomsRevenue=sum(CustomsRevenue,na.rm = TRUE),
                                         ExciseRevenue=sum(ExciseRevenue,na.rm = TRUE),
                                         VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE),
                                         CustomsDuties_Benchmark=sum(CustomsDuties_Benchmark,na.rm = TRUE),
                                         CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                      
                      
                      CustomsDuties_TE_agg_HS<-left_join(CustomsDuties_TE_agg_HS,taric_data,by =c("HS_code"))  
                      
                      
                      
                      # Adding desegregation by HS codes
                      CustomsDuties_TE_agg_HS <- mutate(CustomsDuties_TE_agg_HS,
                                                        Chapter = substr(HS_code_s, 1, 2),
                                                        Four_digit = substr(HS_code_s, 1, 4),
                                                        Six_digit = substr(HS_code_s, 1, 7),
                                                        Eight_digit = paste0(substr(HS_code_s, 1, 4),
                                                                             "",
                                                                             substr(HS_code_s, 5, 7),
                                                                             " ",
                                                                             substr(HS_code_s, 8, 9)))
                      
                      
                      
        # 1. Data Sub-setting-------------------------------------------------------
                      # 1.1 Fuels ------------------------------------------------------------------------
                      
                      # Subset Chapter 27
                      HS_Chapter_27<-CustomsDuties_TE_agg_HS %>%
                        filter(Chapter == '27')
                      
                      
                      HS_Chapter_27<-HS_Chapter_27%>%
                        dplyr::filter(ExciseRevenue>0)
                      
                      HS_Chapter_27$Subdataset<-c(" ") 
                      
                      
                      # Subset 1
                      HS_Chapter_27 <- HS_Chapter_27 %>%
                        dplyr::mutate(Subdataset = case_when(
                          Eight_digit == "2710 19 43" ~ 'EURO DIESEL',
                          Eight_digit == "2710 12 45" ~ 'EUROSUPER BC 95',
                          Eight_digit == "2710 12 49" ~ 'EUROSUPER BS 100',
                          Eight_digit == "2711 12 94" ~ 'LPG',
                          Eight_digit == "2710 19 81" ~ 'LUBRICATING OILS', # MOTOR OIL
                          Eight_digit == "2711 13 97" ~ 'LPG',
                          Eight_digit == "2710 19 99" ~ 'LUBRICATING OILS', # Other lubricating oils
                          Eight_digit == "2710 19 67" ~ 'HEAVY OILS', # MAZUT M-1
                          Eight_digit == "2710 19 83" ~ 'LUBRICATING OILS', #Hydraulic oils
                          Eight_digit == "2710 19 87" ~ 'LUBRICATING OILS', #MOTOR GEAR OIL
                          Eight_digit == "2711 12 97" ~ 'LPG',
                          Eight_digit == "2711 12 11" ~ 'LPG',
                          Eight_digit == "2711 13 91" ~ 'LPG',
                          Eight_digit == "2710 19 93" ~ 'LUBRICATING OILS',#Electrical insulating oils
                          Eight_digit == "2711 12 19" ~ 'LPG',
                          Eight_digit == "2710 19 47" ~ 'OTHER',
                          Eight_digit == "2707 30 00" ~ 'LUBRICATING OILS',#Oils and other products of the distillation
                          Eight_digit == "2710 12 31" ~ 'AVIATION GASOLINE',
                          Eight_digit == "2710 12 90" ~ 'AVIATION GASOLINE',
                          TRUE ~ 'OTHER'
                        ))
                      
                      # Subset 2 - Cyclic and Acyclic hydrocarbons- ORGANIC CHEMICALS
                      CyclicAcyclicHydrocarbons <- CustomsDuties_TE_agg_HS %>%
                        dplyr::filter(Four_digit %in% c('2901','2902'))
                      
                      CyclicAcyclicHydrocarbons$Subdataset<-c("CHEMICAL PRODUCTS")
                      
                      # Subset 3  -Other which are used for the same purposes as mineral oils-MISCELLANEOUS CHEMICAL PRODUCTS
                      # Organic composite solvents and solvents
                      # Alkylbenzenes and mixed Alkylnaphthalenes
                      
                      Other_Ch_38 <- CustomsDuties_TE_agg_HS %>%
                        dplyr::filter(Four_digit %in% c('3811','3814','3817'))
                      
                      Other_Ch_38$Subdataset<-c("CHEMICAL PRODUCTS")
                      
                      #  Merging subsets
                      Fuel_tbl <- bind_rows(HS_Chapter_27,CyclicAcyclicHydrocarbons,Other_Ch_38)
                      
                      # Initialize Effective_Excise_rate if missing
                      if (!"Effective_Excise_rate" %in% names(Fuel_tbl)) {
                        Fuel_tbl$Effective_Excise_rate <- NA  # Initialize with NA or a default value
                      }
                      
                      
                      #  Removing Nan from effective tax rates
                      Fuel_tbl$Effective_Excise_rate<-ifelse(Fuel_tbl$Quantity==0,0,Fuel_tbl$Effective_Excise_rate)
                      
                      
                    
                      
                      
                      # Filter dataset with excise
                      Fuel_tbl<-Fuel_tbl%>%
                        dplyr::filter(ExciseRevenue>0)
                      
                      Fuel_tbl$DataSet<-c("Fuels")
                      
                      
                      Fuel_tbl$Quantity_HL<-as.numeric(0)
                      Fuel_tbl$PotentialExcise<-as.numeric(0)
                      
                      
                      
                     
                      
                      
                      # Fuel_tbl_subset<-Fuel_tbl%>%
                      #   select(Eight_digit,Subdataset,Effective_Excise_rate)
                      #  View(Fuel_tbl_subset)
                      
                      # View(Fuel_tbl)
                      
                      # sum(Fuel_tbl$ExciseRevenue)
                      # [1] 135639599
                      

                      
                        # 1.1a Adding emission factors -------------------------------------------------
                      
                      # Create a data frame
                      EmmisionFactors <- data.frame(Subdataset =  c("AVIATION GASOLINE", "EURO DIESEL", "EUROSUPER BC 95", "EUROSUPER BS 100", "HEAVY OILS", "LPG BUTANE", "LPG PROPANE"),
                                                    CO2_per_1000_units = c(2.54, 2.68, 2.29, 2.29, 2.83, 2.98, 2.98),
                                                    StatutoryExcisePerLiter = c(0.385, 0.360, 0.385, 0.385, 0.250, 0.100, 0.100)
                                                    
                      )
                      
                      
                      
                      Fuel_tbl<-left_join(Fuel_tbl,EmmisionFactors, by=c("Subdataset"="Subdataset"))
                      
                      
                      # Filter rows where CO2_per_1000_units is not NaN
                      Fuel_tbl <- Fuel_tbl[complete.cases(Fuel_tbl$CO2_per_1000_units), ]
                      
                      
                      # # Adding calculation of TE with legal approach
                      # Adding calculation of TE with legal approach
                      Fuel_tbl<-Fuel_tbl%>%
                        dplyr::mutate(PotentialExcise=StatutoryExcisePerLiter*Quantity,
                                      TE=PotentialExcise-ExciseRevenue)
                                      
                      
                      # Potential
                      
                      Fuel_tbl_Excise<-sum(Fuel_tbl$PotentialExcise,na.rm = TRUE)/1E06
                      Fuel_tbl_TE<-sum(Fuel_tbl$TE,na.rm = TRUE)/1E06
                      
                      # View(Fuel_tbl)
                      
                      
                      # sum(Fuel_tbl$PotentialExcise)
                      # 
                      # sum(Fuel_tbl$ExciseRevenue)
                      
                      
                      
                      # write.csv(Fuel_tbl,"Fuel_tbl2.csv")
                      
                      # 1.2 Tobacco ---------------------------------------------------------------
                      
                      ## 2401-Unmanufactured tobacco; tobacco refuse:
                      ## 2402-Cigars, cheroots, cigarillos and cigarettes, of tobacco or of tobacco substitutes
                      ## 2403-Other manufactured tobacco and manufactured tobacco substitutes; "homogenized" or "reconstituted" tobacco; tobacco extracts and essences:
                      ## 2404-Products containing tobacco, reconstituted tobacco, nicotine, or tobacco or nicotine substitutes, intended for inhalation without combustion; other nicotine containing products intended for the intake of nicotine into the human body
                      
                      
                      # Subset Chapter 24
                      HS_Chapter_24<-CustomsDuties_TE_agg_HS %>%
                        filter(Chapter == '24')
                      
                      HS_Chapter_24$Subdataset<-c("Tobacco") #<---- Ova ke treba detealno da se definira po tip na proizvodi ili po tarifen broj 
                      
                      # Check if the column exists, and initialize it if missing
                      if (!"Effective_Excise_rate" %in% names(HS_Chapter_24)) {
                        HS_Chapter_24$Effective_Excise_rate <- NA  # Initialize with NA
                      }
                      
                      
                      # Removing Nan from effective tax rates
                      HS_Chapter_24$Effective_Excise_rate<-ifelse(HS_Chapter_24$Quantity==0,0,HS_Chapter_24$Effective_Excise_rate)
                      
                      Tobacco_tbl<-HS_Chapter_24%>%
                        dplyr::filter(ExciseRevenue>0)
                      
                      Tobacco_tbl$DataSet<-c("Tobacco")
                      
                      Tobacco_tbl$Quantity_HL<-as.numeric(0)
                      Tobacco_tbl$PotentialExcise<-as.numeric(0)
                      
                      
                      Tobacco_tbl <- Tobacco_tbl %>%
                        dplyr::mutate(Subdataset = case_when(
                          Eight_digit == "2402 10 00" ~ 'CIGARS AND CIGARILLOS',
                          Eight_digit == "2402 20 10" ~ 'CIGARETTES',
                          Eight_digit == "2402 20 90" ~ 'CIGARETTES',
                          TRUE ~ 'TOBACCO'
                        ))
                      
                      # View(Tobacco_tbl)
                      
                      # Calculation of base for calculation
                      # Conversion factors, (equivalent of grams tobacco):
                      
                      # . 1 stick cigarette = 0.7g
                      # . 1 cigarillo = 3 g
                      # . 1 cigar = 20 g
                      # . 1 ml (E-cig) = 1 g # Electronic cigarettes and similar personal electric vaporizing devices-Contains sets together with cartridges for smoking
                      # . Fine cut and HNB are with the same weight as is declared converted in grams
                      
                      
                      Tobacco_tbl$ExciseRate<-as.numeric(35)
                      
                      Tobacco_tbl <- Tobacco_tbl %>%
                        mutate(ExciseRate = ifelse(Eight_digit == "2402 10 00", 1.1, ExciseRate))
                      
                      
                      Tobacco_tbl <- Tobacco_tbl %>%
                        mutate(ExciseRate = ifelse(Eight_digit == "2402 20 90", 51, ExciseRate))
                      
                      
                      Tobacco_tbl <- Tobacco_tbl %>%
                        dplyr::mutate(
                          ConventionalUnit = ExciseRevenue / ExciseRate, # For Cigars, cigarillos and cigarettes is 1000 sticks, for fine-cut is 1 KG
                          ExciseEstimation = ConventionalUnit * ExciseRate,
                          VolumeBySticks = case_when(
                            Eight_digit == "2402 10 00" ~ Quantity * 1000,
                            Eight_digit == "2402 20 90" ~ ConventionalUnit * 1000,
                            TRUE ~ Quantity * 1000
                          ),
                          # Base for calculation of TE's
                          Base_EquivalentOfGramsTobacco = case_when(
                            Eight_digit == "2402 10 00" ~ VolumeBySticks * 11.5, 
                            Eight_digit == "2402 20 90" ~ VolumeBySticks * 0.7,
                            TRUE ~ VolumeBySticks
                          ),
                          # Equivalent of tobacco in grams
                          ExciseByEquivalentGramsTobacco=ExciseEstimation/Base_EquivalentOfGramsTobacco,
                          ExciseByEquivalentGramsTobacco_1000=ExciseByEquivalentGramsTobacco*1000,
                          CrossCheckExise=Base_EquivalentOfGramsTobacco*ExciseByEquivalentGramsTobacco
                          # TE_TEST=Base_EquivalentOfGramsTobacco/1000*72.857143,
                          # TE=TE_TEST-CrossCheckExise
                        )
                      
                      
                      Tobacco_tbl <- Tobacco_tbl %>%
                        dplyr::filter(Subdataset %in% c("CIGARETTES", "TOBACCO"))
                      
                      
                      
                      # View(Tobacco_tbl)
                      
                      # Tobacco_tbl_subset<-Tobacco_tbl%>%
                      #   select(Subdataset,Quantity,ExciseRevenue,ExciseRate,ConventionalUnit,VolumeBySticks,Base_EquivalentOfGramsTobacco,
                      #          ExciseByEquivalentGramsTobacco,ExciseByEquivalentGramsTobacco_1000,CrossCheckExise
                      #          )
                      # 
                      # 
                      # write.csv(Tobacco_tbl_subset,"Tobacco_tbl_subset.csv")
                      # 
                      # 1.3 Alcohol -----------------------------------------------------------          
                            # 1.3.1 Beer --------------------------------------------------------------------
                            
                            BeerQuantity <- CustomsDuties_TE_agg_HS %>%
                              filter(Four_digit == 2203)
                            
                            
                            # Adding columns
                            BeerQuantity$Subdataset<-c("BEER")
                            # Adding excis rate
                            BeerQuantity$ExciseRate<-as.numeric(800)
                            
                            
                            test<-BeerQuantity%>%
                              dplyr::mutate(Quantity_HL=Quantity/100, # Conversion in HL
                                            PotentialExcise=Quantity_HL*ExciseRate,
                                            Alc_Content=ExciseRevenue/PotentialExcise,
                                            Pure_Alc=Quantity_HL*Alc_Content
                                            # test=ExciseRate*Pure_Alc
                              )
                            
                            sum(BeerQuantity$ExciseRevenue)/sum(BeerQuantity$Quantity)
                            
                            
                            # 1.3.2 Sugar-sweetened beverages(SSB) ------------------------------------------------------------------
                            # Do tuka !!! Ova da se vnese 
                            # # # Non Alcoholic Beer is included here
                            SSB_Quantity <- CustomsDuties_TE_agg_HS %>%
                              filter(Four_digit == 2202)
                            
                            SSB_Quantity$Subdataset<-c("SSB")
                            
                            
                            # 1.3.3 Wine of fresh grapes ----------------------------------------------------
                            
                            # Wine of fresh grapes, including fortified wines; grape must other than that of heading 2009)                    
                            WineQuantity <- CustomsDuties_TE_agg_HS %>%
                              filter(Four_digit == 2204)
                            
                            WineQuantity$Subdataset<-c("WINE")
                            # Adding excis rate
                            WineQuantity$ExciseRate<-as.numeric(500)
                            
                            # View(WineQuantity)
                            # sum(WineQuantity$Quantity)/1e06
                            
                            
                            
                            # 1.3.4 Vermouth ---------------------------------------------------
                            
                            # Vermouth and other wine of fresh grapes flavoured with plants or aromatic substances
                            VermouthQuantity <- CustomsDuties_TE_agg_HS %>%
                              filter(Four_digit == 2205)
                            
                            
                            # View(VermouthQuantity)
                            # sum(VermouthQuantity$Quantity)/1e06
                            VermouthQuantity$Subdataset<-c("VERMOUTH")
                            
                            # Adding excis rate
                            VermouthQuantity$ExciseRate<-as.numeric(500)
                            
                            
                            # 1.3.5 Other fermented beverages --------------------------------------------------
                      
                      # Other fermented beverages (for example, cider, perry, mead); mixtures of fermented beverages and mixtures of fermented beverages and non-alcoholic beverages, not elsewhere specified or included
                      
                      OtherFermentedBeveragesQuantity  <- CustomsDuties_TE_agg_HS %>%
                        filter(Four_digit == 2206)
                      
                      OtherFermentedBeveragesQuantity$Subdataset<-c("OTHER FERMENTED BEVERAGES")
                      
                      # Adding excis rate
                      OtherFermentedBeveragesQuantity$ExciseRate<-as.numeric(500)
                      
                      
                            # 1.3.5a Undenatured ethyl alcohol of an alcoholic strength by volume of 80 % vol or higher; ethyl alcohol and other spirits, denatured, of any strength:-----------------------------------------
                      
                      # Not included ! With 80 strength by volume of 80 % vol or higher is not for driniking
                      # 
                      # UndenaturedethylAlcoholQuantity  <- CustomsDuties_TE_agg_HS %>%
                      #   filter(Four_digit == 2207)
                      # 
                      # UndenaturedethylAlcoholQuantity$Subdataset<-c("UndenaturedethylAlcoholQuantity")
                      # 
                      
                      
                            # 1.3.6 Undenatured ethyl alcohol ---------------------------------------------
                      
                      # Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80 % vol; spirits, liqueurs and other spirituous beverages
                      UndenaturedEthylAlcoholQuantity  <- CustomsDuties_TE_agg_HS %>%
                        dplyr::filter(Four_digit == 2208)
                      
                      
                      # View(UndenaturedEthylAlcoholQuantity)
                      #  sum(UndenaturedEthylAlcoholQuantity$Quantity)/1e06
                      UndenaturedEthylAlcoholQuantity$Subdataset<-c("ALCHOLIC BEVERAGE")
                      
                      # Adding excis rate
                      UndenaturedEthylAlcoholQuantity$ExciseRate<-as.numeric(800)
                      
                            # 1.3.7 Merging data --------------------------------------------------
                      
                      # SSB is not included in the dataset !!!
                      
                      # AlcoholQuantity <- bind_rows(BeerQuantity,WineQuantity,VermouthQuantity,OtherFermentedBeveragesQuantity,UndenaturedEthylAlcoholQuantity)
                      
                      AlcoholQuantity <- bind_rows(BeerQuantity,WineQuantity,VermouthQuantity,OtherFermentedBeveragesQuantity,UndenaturedEthylAlcoholQuantity,SSB_Quantity)
                      
                      
                      # Check if the column exists, and initialize it if missing
                      if (!"Effective_Excise_rate" %in% names(AlcoholQuantity)) {
                        AlcoholQuantity$Effective_Excise_rate <- NA  # Initialize as NA
                      }
                      
                      
                      # Removing Nan from effective tax rates
                      AlcoholQuantity$Effective_Excise_rate<-ifelse(AlcoholQuantity$Quantity==0,0,AlcoholQuantity$Effective_Excise_rate)
                      
                      
                      
                      Alcohol_tbl<-AlcoholQuantity%>%
                        dplyr::filter(ExciseRevenue>0)                    
                      
                      Alcohol_tbl$DataSet<-c("Alcohol")
                      
                      # Calculation of abs. alc 100%
                      
                      Alcohol_tbl<-Alcohol_tbl%>%
                        dplyr::mutate(Quantity_HL=Quantity/100, # Conversion in HL
                                      PotentialExcise=Quantity_HL*ExciseRate,
                                      Alc_Content=ExciseRevenue/PotentialExcise,
                                      Pure_Alc=Quantity_HL*Alc_Content
                                      # test=ExciseRate*Pure_Alc
                        )
                      
                      Alcohol_tbl$Quantity_HL<-NULL
                      Alcohol_tbl$PotentialExcise<-NULL
                      
                      # Cross check
                      # sum(Alcohol_tbl$ExciseRevenue)
                      # sum(Alcohol_tbl$test)
                      
                      Alcohol_tbl$Alc_Content<-round(Alcohol_tbl$Alc_Content,2)
                      
                      
                      # Assuming your data frame is named Alcohol_tbl
                      Alcohol_tbl <- Alcohol_tbl %>%
                        mutate(DataSet  = case_when(
                          grepl("SSB", Subdataset, ignore.case = TRUE) ~ "SSB",
                          TRUE ~ DataSet  # If not containing 'SSB', keep the original value
                        ))
                      
                      
                      #write.csv(Alcohol_tbl,"Alcohol_tbl.csv")
                      
                      #View(Alcohol_tbl)
                      
                      Alcohol_tbl_subset_export<-Alcohol_tbl%>%
                        select(Subdataset,ExciseRate,Alc_Content,Pure_Alc,Quantity,ExciseRevenue)%>%
                        group_by(Subdataset,ExciseRate,Alc_Content)%>%
                        summarise(Pure_Alc=sum(Pure_Alc),
                                  Quantity=sum(Quantity),
                                  ExciseRevenue=sum(ExciseRevenue)
                                  
                        )
                      
                      
                      
                      #View(Alcohol_tbl_subset_export)
                      
                      # sum(Alcohol_tbl_subset_export$ExciseRevenue)
                      # [1] 469772.5
                       
                      # write.csv(Alcohol_tbl_subset_export,"Alcohol_tbl_subset_export.csv")
                      
                      
                      
                      # 1.4 Cars ------------------------------------------------------------------
                      
                      CarsSet <- CustomsDuties_TE_agg_HS %>%
                        dplyr::filter(Four_digit %in% c('8703'))
                      
                      
                      Cars_tbl<-left_join(CarsSet,Cars_ExciseRates, by=c("HS_code_s"="HS"))
                      
                      
                      
                      
                      Cars_tbl$Subdataset<-c("Cars")
                      Cars_tbl$DataSet<-c("Cars")
                      
                      
                      Cars_tbl$Quantity_HL<-as.numeric(0)
                      Cars_tbl$PotentialExcise<-as.numeric(0)
                      
                      
                      Cars_tbl<-Cars_tbl%>%
                        dplyr::mutate(PotentialExcise=Quantity*ExciseRate.y,
                                      TE=PotentialExcise-ExciseRevenue
                                      )
                      
                      
                      #View(Cars_tbl)
                      
                      
                      sum(Cars_tbl$PotentialExcise,na.rm = TRUE)
                        
                      TE_Cars_ExciseTotal<-sum(Cars_tbl$PotentialExcise,na.rm = TRUE)/1e+06
                      TE_Cars_total<-(sum(Cars_tbl$PotentialExcise,na.rm = TRUE)-sum(Cars_tbl$ExciseRevenue,na.rm = TRUE))/1e+06
                      
                      # Cars_tbl<-CarsSet%>%
                      #   dplyr::filter(ExciseRevenue>0)
                      #
                      
                      # 
                      # sum(Cars_tbl$ExciseRevenue)
                      # [1] 535240
                      # 
                      # View(Cars_tbl)
                      
                      
                      
          # 2.Merging Data Sets and estimation of TE'S -------------------------------------------------------
                      
                      #ExciseFinal_tbl <- bind_rows(Fuel_tbl, Tobacco_tbl,Alcohol_tbl,Cars_tbl)
                      ExciseFinal_tbl <- bind_rows(Fuel_tbl, Tobacco_tbl,Alcohol_tbl,Cars_tbl)
                      ExciseFinal_tbl$ExciseRate[is.nan(ExciseFinal_tbl$ExciseRate)]<-0
                      ExciseFinal_tbl$Alc_Content[is.nan(ExciseFinal_tbl$Alc_Content)]<-0
                      ExciseFinal_tbl$Pure_Alc[is.nan( ExciseFinal_tbl$Pure_Alc)]<-0
                      # Benchmark_ExciseFuels<-1
                      
                      
                      Benchmark_AVIATION_GASOLINE<-0.43
                      Benchmark_EURO_DIESEL<-0.43
                      Benchmark_EUROSUPER_BC_95<-0.45
                      Benchmark_EUROSUPER_BS_100<- 0.39
                      Benchmark_HEAVY_OILS<-0.48
                      Benchmark_LPG_BUTANE<-0.50
                      Benchmark_LPG_PROPANE<-0.50
                      Benchmark_ExciseTobacco<-72.8
                      Benchmark_ExciseAlcohol<-800
                      
                      
                      
                      
                      
                      calculate_benchmark_fun <- function(data) {
                        data %>%
                          mutate(
                            result = case_when(
                              #DataSet == "Fuels" ~ Quantity * Benchmark_ExciseFuels,
                              Subdataset == "AVIATION GASOLINE" ~ Quantity * Benchmark_AVIATION_GASOLINE,
                              Subdataset == "EURO DIESEL" ~ Quantity * Benchmark_EURO_DIESEL,
                              Subdataset == "EUROSUPER BC 95" ~ Quantity * Benchmark_EUROSUPER_BC_95,
                              Subdataset == "EUROSUPER BS 100" ~ Quantity * Benchmark_EUROSUPER_BS_100,
                              Subdataset == "HEAVY OILS" ~ Quantity * Benchmark_HEAVY_OILS,
                              Subdataset == "LPG BUTANE" ~ Quantity * Benchmark_LPG_BUTANE,
                              Subdataset == "LPG PROPANE" ~ Quantity * Benchmark_LPG_PROPANE,
                              DataSet == "Tobacco" ~ (Base_EquivalentOfGramsTobacco / 1000) * Benchmark_ExciseTobacco,
                              DataSet == "Alcohol" ~ Pure_Alc * Benchmark_ExciseAlcohol,
                              DataSet == "Cars" ~ Quantity * ExciseRate
                            )
                          )
                      }
                      
                      
                      # Apply the function 
                      Estimation_TE <- calculate_benchmark_fun(ExciseFinal_tbl)%>%
                        dplyr::rename("Excise_BenchmarkRevenue"="result")%>%
                        dplyr::mutate(Excise_TE=Excise_BenchmarkRevenue-ExciseRevenue)
                      
                      # Remove NaN OLD
                      #Estimation_TE[is.na(Estimation_TE)] <- 0
                      
                      Estimation_TE <- Estimation_TE %>%
                        mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .)))
                      
                      
                      # Define a function to round up two columns
                      round_up_columns <- function(data, columns) {
                        data %>%
                          mutate(across(all_of(columns), ~ceiling(.)))
                      }
                      
                      # Apply the function to round up 
                      Estimation_TE <- round_up_columns(Estimation_TE, c("Excise_BenchmarkRevenue", "Excise_TE"))
                      
                      # View(Estimation_TE)
                      
                      #write.csv(Estimation_TE,"Estimation_TE.csv")
                      
                      
                      

                      
        # 3. Main table with Simulation Results ----------------------------------------------------------------
                      # # 1.Main table ------------------------------------------------------------
                      # 
                      # # Emissions approach
                      # TE_MineralOils_total <- Estimation_TE %>% select(DataSet,Excise_TE)%>%filter(DataSet == "Fuels")%>%group_by(DataSet)%>% summarise(Excise_TE = sum(Excise_TE, na.rm = TRUE) / 1e+06)%>%select(Excise_TE)
                      # 
                      # # Exemption approach
                      # TE_MineralOils_total_exemption <- Estimation_TE %>% 
                      #   ungroup()%>%
                      #   select(DataSet,ExciseRevenue,PotentialExcise)%>%
                      #   filter(DataSet == "Fuels")%>%
                      #   group_by(DataSet)%>% 
                      #   mutate(PotentialExcise = sum(PotentialExcise, na.rm = TRUE),
                      #          ExciseRevenue = sum(ExciseRevenue, na.rm = TRUE),
                      #          Excise_TE=(PotentialExcise-ExciseRevenue)/ 1e+06)%>%
                      #   distinct()%>%
                      #   select(Excise_TE)
                      # 
                      # TE_MineralOils_total_exemption<-TE_MineralOils_total_exemption$Excise_TE
                      # 
                      # #summarise(Excise_TE = sum(PotentialExcise, na.rm = TRUE)-summarise(ExciseRevenue = sum(ExciseRevenue, na.rm = TRUE) / 1e+06))%>%select(Excise_TE)
                      # 
                      # TE_TobaccoProducts_total <- Estimation_TE %>% select(DataSet,Excise_TE)%>%filter(DataSet == "Tobacco")%>%group_by(DataSet)%>% summarise(Excise_TE = sum(Excise_TE, na.rm = TRUE) / 1e+06)%>%select(Excise_TE)
                      # TE_Alcohol_total <- Estimation_TE %>% select(DataSet,Excise_TE)%>%filter(DataSet == "Alcohol")%>%group_by(DataSet)%>% summarise(Excise_TE = sum(Excise_TE, na.rm = TRUE) / 1e+06)%>%select(Excise_TE)
                      # #TE_Cars_total <- Estimation_TE %>% select(DataSet,Excise_TE)%>%filter(DataSet == "Cars")%>%group_by(DataSet)%>% summarise(Excise_TE = sum(Excise_TE, na.rm = TRUE) / 1e+06)%>%select(Excise_TE)
                      # 
                      # 
                      # 
                      # MainResultsExcise<-MacroFiscalData%>%
                      #   dplyr::filter(Year==actual_year_simulation)%>%
                      #   dplyr::mutate(
                      #     # Total
                      #     `Actual Total Import`=sum(Estimation_TE$Value,na.rm=TRUE)/1e+06,
                      #     `Actual Total Excise Revenues`=sum(Estimation_TE$ExciseRevenue,na.rm=TRUE)/1e+06,
                      #     `Excise Revenue Benchmark`=sum(Estimation_TE$Excise_BenchmarkRevenue,na.rm=TRUE)/1e+06,
                      #     
                      #     `Tax Expenditures`=sum(Estimation_TE$Excise_TE,na.rm=TRUE)/1e+06,
                      #     `Tax Expenditures as % of GDP`=(`Tax Expenditures`/GDP)*100,
                      #     `Tax Expenditures as % of Government Revenue`=(`Tax Expenditures`/GeneralGovernmentRevenue)*100,
                      #     `Tax Expenditures (as % of Taxes On Products)`=(`Tax Expenditures`/TaxesOnProducts)*100,
                      #     `Tax Expenditures (as % of ExciseRevenue)`=(`Tax Expenditures`/TaxesOnProducts)*100,
                      #     # TE's by type of products
                      #     `Tax Expenditures by Mineral Oils`= TE_MineralOils_total$Excise_TE,
                      #     `Tax Expenditures by Tobacco Products`= TE_TobaccoProducts_total$Excise_TE,
                      #     `Tax Expenditures by Alcohol Products`= TE_Alcohol_total$Excise_TE,
                      #     `Tax Expenditures by Cars`= 0 #TE_Cars_total$Excise_TE
                      #   )%>%
                      #   dplyr::select(
                      #     `Actual Total Import`,
                      #     `Actual Total Excise Revenues`,
                      #     `Excise Revenue Benchmark`,
                      #     `Tax Expenditures`,
                      #     `Tax Expenditures as % of GDP`,
                      #     `Tax Expenditures as % of Government Revenue`,
                      #     `Tax Expenditures (as % of Taxes On Products)`,
                      #     `Tax Expenditures (as % of ExciseRevenue)`,
                      #     `Tax Expenditures by Mineral Oils`,
                      #     `Tax Expenditures by Tobacco Products`,
                      #     `Tax Expenditures by Alcohol Products`,
                      #     `Tax Expenditures by Cars`
                      #   )
                      # 
                      # 
                      # 
                      # MainResultsExcise<-melt(MainResultsExcise)
                      # MainResultsExcise$value<-round(MainResultsExcise$value,2)
                      # 
                      # MainResultsExcise<-MainResultsExcise%>%
                      #   dplyr::rename("Description"= "variable",
                      #                 #"Value"="value")
                      #                 "Normative_Approach"="value")
                      

# 2   Second table-LEGAL APPROACH ------------------------------------------

                     # actual_year_simulation=2023
                      
                      actual_year_simulation=base_year

                      
                      Legal_Fuels_Cars<-rbind(Fuel_tbl,Cars_tbl)
                      
                     # View(Legal_Fuels_Cars)
                      
                      sum(Legal_Fuels_Cars$TE,na.rm = TRUE)/1E06
                      
                      
                      Legal_Fuels_Cars_Products<-Legal_Fuels_Cars%>%
                        dplyr::select(Subdataset,TE)%>%
                        dplyr::group_by(Subdataset)%>%
                        dplyr::summarise(TE = pmax(sum(TE, na.rm = TRUE), 0))%>%
                        dplyr::filter(TE>0)
                      
                      
                      
                      Fuel_tbl_TE <- Legal_Fuels_Cars_Products %>%
                        dplyr::filter(Subdataset != "Cars") %>%
                        dplyr::select(-c("Subdataset"))%>%
                        dplyr::summarise(TE = sum(TE, na.rm = TRUE))
                      
                      
                      Cars_tbl_TE <- Legal_Fuels_Cars_Products %>%
                        dplyr::filter(Subdataset == "Cars") %>%
                        dplyr::select(-c("Subdataset"))%>%
                        dplyr::summarise(TE = sum(TE, na.rm = TRUE))
                      
                      
                      
                      
                      MainResultsExcise_1<-MacroFiscalData%>%
                        dplyr::filter(Year==base_year)%>%
                        dplyr::mutate(
                          # Total
                          `Actual Total Import`=sum(CustomsDuties_TE_agg_HS$Value,na.rm=TRUE)/1e+06,
                          `Actual Total Excise Revenues`=sum(CustomsDuties_TE_agg_HS$ExciseRevenue,na.rm=TRUE)/1e+06,
                          `Excise Revenue Benchmark`=  TE_Cars_ExciseTotal+Fuel_tbl_Excise,
                            #(sum(Estimation_TE$ExciseRevenue)/1e+06) + TE_MineralOils_total_exemption + TE_TobaccoProducts_total$Excise_TE +TE_Alcohol_total$Excise_TE,
                          
                          #sum(Estimation_TE$Excise_BenchmarkRevenue,na.rm=TRUE)/1e+06,
                          
                          #`Tax Expenditures`= (TE_MineralOils_total_exemption + TE_TobaccoProducts_total$Excise_TE +TE_Alcohol_total$Excise_TE),
                          `Tax Expenditures`=  sum(Legal_Fuels_Cars$TE,na.rm = TRUE)/1E06, #TE_Cars_total$Excise_TE),
                          
                          #sum(Estimation_TE$Excise_TE,na.rm=TRUE)/1e+06,
                          `Tax Expenditures as % of GDP`=(`Tax Expenditures`/GDP)*100,
                          `Tax Expenditures as % of Government Revenue`=(`Tax Expenditures`/GeneralGovernmentRevenue)*100,
                          `Tax Expenditures (as % of Taxes On Products)`=(`Tax Expenditures`/TaxesOnProducts)*100,
                          `Tax Expenditures (as % of ExciseRevenue)`=(`Tax Expenditures`/TaxesOnProducts)*100,
                          # TE's by type of products
                          `Tax Expenditures by Mineral Oils`= (Fuel_tbl_TE$TE)/1e06,
                          `Tax Expenditures by Tobacco Products`= 0, #TE_TobaccoProducts_total$Excise_TE,
                          `Tax Expenditures by Alcohol Products`= 0, #TE_Alcohol_total$Excise_TE,
                          `Tax Expenditures by Cars`= (Cars_tbl_TE$TE)/1e06 #$Excise_TE
                        )%>%
                        dplyr::select(
                          `Actual Total Import`,
                          `Actual Total Excise Revenues`,
                          `Excise Revenue Benchmark`,
                          `Tax Expenditures`,
                          `Tax Expenditures as % of GDP`,
                          `Tax Expenditures as % of Government Revenue`,
                          `Tax Expenditures (as % of Taxes On Products)`,
                          `Tax Expenditures (as % of ExciseRevenue)`,
                          `Tax Expenditures by Mineral Oils`,
                          `Tax Expenditures by Tobacco Products`,
                          `Tax Expenditures by Alcohol Products`,
                          `Tax Expenditures by Cars`
                        )
                      
                      
                      MainResultsExcise_1<-melt(MainResultsExcise_1)
                      MainResultsExcise_2<-MainResultsExcise_1
                      MainResultsExcise_2$value<-round(MainResultsExcise_2$value,2)
                      
                     # MainResultsExcise_2 <- MainResultsExcise_2[, -2]
                      
                      
                      
                      MainResultsExcise_2<-MainResultsExcise_2%>%
                        dplyr::rename("Description"= "variable"
                                      #"Value"="value")%>%
                                      )
                      
                      # #MainResultsExciseFinal
                      # 
                      # 
                      # 
                      # MainResultsExciseFinal<-cbind(MainResultsExcise,MainResultsExcise_1)
                      # 
                      # 
                      # MainResultsExciseFinal$Normative_Approach<-NULL
                      
                      # MainResultsExciseFinal<-MainResultsExciseFinal%>%
                      #   dplyr::rename("Value"="Legal_Approach")
                     
                      summary_TE_SIM_Excise<-MainResultsExcise_2
                     
                      
                    
                      #MainResultsExcise_1 <- MainResultsExcise_1[, -2]
                      
                      GDP_share_TE <- MainResultsExcise_1 %>%
                        filter(variable == "Tax Expenditures as % of GDP")
                      
                      ProjectionTE_Excise<-MacroFiscalData%>%
                        dplyr::filter(Year>=base_year)%>%
                        select(Year,GDP)%>%
                        dplyr::mutate(tax_expenditures=GDP*GDP_share_TE$value)
                      
                      

                    # Timing the process
                    end.time <- proc.time()
                    save.time <- end.time - start.time
                    cat("\n Number of minutes running-Tax Expenditures:", save.time[3] / 60, "\n \n")                  
                    
