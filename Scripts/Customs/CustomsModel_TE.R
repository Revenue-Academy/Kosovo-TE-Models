' Data preparation,estimation of tax expenditures and preparation of data for charts
'
options(warn = -1)

suppressMessages({
  
# I. Estimation of tax expenditures for customs duties ----------------------------------------------------------
        # 1.Processing of data ------------------------------------------------------

                    # Change the column names EN
                    colnames(Import_raw_monthly)[1:11] <- c("Year","Month","TradePolicy","Countries","Code_Description","Quantity","Value","Netweight","CustomsRevenue","ExciseRevenue","VAT_Revenue")
                    
                    # Split the Code_Description column into two parts using "-"
                    split_columns_hs <- strsplit(Import_raw_monthly$Code_Description, "-")
                    
                    # Split the Code_Description column into two parts using "-"
                    # Create new columns with the split parts
                    Import_raw_monthly$HS_code <- sapply(split_columns_hs, `[`, 1)
                    
                    
                    # Extract the second part of the split using "[, 2]"
                    Import_raw_monthly$Description <- sapply(split_columns_hs, `[`, 2)
                    
                    # Split the Code for countries column into two parts using "-"
                    split_columns_countries <- strsplit(Import_raw_monthly$Countries, "-")
                    Import_raw_monthly$iso2c <- sapply(split_columns_countries, `[`, 1)
                    
                    Import_raw_monthly<-Import_raw_monthly%>%
                      dplyr::mutate(HS_code = trimws(HS_code, which = "both"))
                    
                   # Import_raw_monthly$Code_Description<-NULL
                    Import_raw_monthly$TradePolicy<-NULL
                    Import_raw_monthly$Countries<-NULL
            
                    
                    Import_raw_monthly<-Import_raw_monthly%>%
                      dplyr::select("HS_code","Description",
                                    "iso2c","Month","Year","Quantity","Value","Netweight","CustomsRevenue","ExciseRevenue","VAT_Revenue")
                   
                    # Trim data
                    Import_raw_monthly<-Import_raw_monthly%>%
                      dplyr::mutate(iso2c = trimws(iso2c, which = "both"))
            
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
                   
                    rm(split_columns_hs,split_columns_countries)
            
                    

        # 2. Adding FreeTrade agreements --------------------------------------------

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
            

        # 3.Estimation of Tax Expenditures for Customs duties -----------------------
            # 3.1 Countries -----------------------------------------------------------

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
                      

            # 3.2 Harmonized System-HS  --------------------------------------------------------------------

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

                    
# II.Preparing data for charts and for the main table --------------------------------------------

      # 1.Preparing data for Charts ---------------------------------------
            # 1.1 Regular Import------------------------------------------
                    
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
                    
                    
            # 1.2 Pie chart  --------------------------------------------------------------
                    
                    CustomsDuties_base_pie<-CustomsDuties_base%>%
                      dplyr::select(Treatment,Value)%>%
                      dplyr::group_by(Treatment)%>%
                      dplyr::summarise(Value=sum(Value,na.rm = TRUE))
                    
                    CustomsDuties_base_pie<-melt(CustomsDuties_base_pie)

            # 1.3 TE's by HS Type of products -----------------------------------------
  
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
                  
          
            # 1.4 TE's by HS Sections --------------------------------------------------------------------
              
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
              
              
            # 1.5 TE's by Chapters ---------------------------------------------------------
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
             
            # 1.6 TE's by WTO classification (MTN) Categories -----------------------------------------
      
              # Multilateral Trade Negotiations (MTN) Categories, HS 2017
              # Multilateral Trade Negotiations (MTN) categories were first defined in the Tokyo Round and adapted for the Harmonized System in the Uruguay Round.
              # The product group breakdown in this publication deviates slightly from the previous definition, which was based on the HS 1992 nomenclature.
              
              CustomsDuties_TE_MTN<-CustomsDuties_TE_agg_HS%>%
                dplyr::select(Six_digit,Treatment,CustomsDuties_TE)%>%
                dplyr::group_by(Six_digit,Treatment)%>%
                dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))


              CustomsDuties_TE_MTN<-left_join(CustomsDuties_TE_MTN,WTO_MTN,by =c("Six_digit"))%>%
                dplyr::select(Product_group,Treatment,CustomsDuties_TE)

              CustomsDuties_TE_MTN <- distinct(CustomsDuties_TE_MTN)

              CustomsDuties_TE_MTN<-CustomsDuties_TE_MTN%>%
              dplyr::group_by(Product_group,Treatment)%>%
                dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))

              CustomsDuties_TE_MTN$Product_group <- ifelse(is.na( CustomsDuties_TE_MTN$Product_group), "Other",  CustomsDuties_TE_MTN$Product_group)

              
              
              # Factor the "Sections" column without ordering other strings
              CustomsDuties_TE_MTN$Product_group <- factor( CustomsDuties_TE_MTN$Product_group)
              
              
            # 1.7 TE's by Countries(Choropleth) -----------------------------------------------
            
              # Adding ISO 3 codes to names of countries from ggplot2
              CustomsDuties_TE_agg_countries<-CustomsDuties_TE_agg_countries%>%
                dplyr::mutate(CustomsDuties_TE=CustomsDuties_TE/1000000)
              
              
              
              # Extracting table for GUI
              CustomsDuties_TE_agg_countries_tbl<-CustomsDuties_TE_agg_countries%>%
              dplyr::arrange(desc(CustomsDuties_TE))
              
              
              CustomsDuties_TE_agg_countries_tbl<-left_join(CustomsDuties_TE_agg_countries_tbl,GeoDimension, by=c("iso3c"))%>%
              select(iso3c,countries,CustomsDuties_TE)%>%
              dplyr::rename("Countries"="countries")
              
              
              CustomsDuties_TE_agg_countries_tbl$CustomsDuties_TE<-round(CustomsDuties_TE_agg_countries_tbl$CustomsDuties_TE,3)
              
              CustomsDuties_TE_agg_countries_tbl<-CustomsDuties_TE_agg_countries_tbl%>%
                dplyr::filter(CustomsDuties_TE>0)%>%
                na.omit()

              mapdata3 <- left_join(mapdata_iso3c,CustomsDuties_TE_agg_countries,by = c("iso3"="iso3c"))

              # Removing Antarctica and Nan values
              mapdata3<-mapdata3[!(mapdata3$region=="Antarctica"),]

               
            # 1.8 TE's by HS codes ---------------------------------------------------------
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
              
              
            # 1.9 TE's by CPA and Combined Nomenclature -------------------------------------------------

        # links https://circabc.europa.eu/ui/group/c1b49c83-24a7-4ff2-951c-621ac0a89fd8/library/d3056f31-a684-430a-a77d-12d1d0fdfffa?p=2&n=10&sort=modified_DESC
              
              actual_year_simulation <- unique(Import_raw_monthly$Year)

               CustomsDuties_TE_CPA<-CustomsDuties_TE_agg_HS%>%
                       dplyr::select(Eight_digit,Treatment,CustomsDuties_TE)%>%
                       dplyr::filter(Treatment=="NonPreferential")%>%
                       dplyr::group_by(Eight_digit,Treatment)%>%
                       dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))


              CPA_CN_subset<-CPA_CN%>%
                       dplyr::filter(year==actual_year_simulation)


              
               CustomsDuties_TE_CPA<-left_join(CustomsDuties_TE_CPA,CPA_CN_subset,by = c("Eight_digit"="CN_CODE"))%>%
                 dplyr::select(CPA_CODE,Treatment,CustomsDuties_TE)%>%
                 dplyr::group_by(CPA_CODE,Treatment)%>%
                 dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))


               CustomsDuties_TE_CPA <- distinct(CustomsDuties_TE_CPA)

               
               # Adding desegregation by CPA codes
               CustomsDuties_TE_CPA <- mutate(CustomsDuties_TE_CPA,
                                              CPA_CODE2 = substr(CPA_CODE, 1, 2))
               
               
               # Preparing subset for table with CPA and NACE section
               CustomsDuties_TE_CPA<-CustomsDuties_TE_CPA%>%
                 dplyr::group_by(CPA_CODE2,Treatment)%>%
                 dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))


               CPA_NACE_subset<-CPA_NACE%>%
                 dplyr::select(CPA_CODE2,CPA21_NAME,Heading_CPA,Sector)%>%
                   na.omit()
               
               CPA_NACE_subset$CPA_CODE2<-as.double(CPA_NACE_subset$CPA_CODE2)
               CustomsDuties_TE_CPA$CPA_CODE2<-as.double(CustomsDuties_TE_CPA$CPA_CODE2)
               
               CustomsDuties_TE_CPA<-left_join(CustomsDuties_TE_CPA,CPA_NACE_subset,by = c("CPA_CODE2"="CPA_CODE2"))
               
              
               CustomsDuties_TE_SECTORS<-CustomsDuties_TE_CPA%>%
                 dplyr::select(Sector,CPA21_NAME,CustomsDuties_TE,Treatment)%>%
                 dplyr::group_by(Sector,CPA21_NAME,Treatment)%>%
                 dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
               
               
                CustomsDuties_TE_SECTORS$CPA21_NAME <- ifelse(is.na( CustomsDuties_TE_SECTORS$CPA21_NAME), "Other",  CustomsDuties_TE_SECTORS$CPA21_NAME)
                CustomsDuties_TE_SECTORS$Sector <- ifelse(is.na( CustomsDuties_TE_SECTORS$Sector), "Other",  CustomsDuties_TE_SECTORS$Sector)


            

      # 2.Main Simulation Results ----------------------------------------------

      
                CustomsDuties_base_non_preferential<-CustomsDuties_base%>%
                  dplyr::filter(Treatment=="NonPreferential")
                
                MainResultsCustoms1<-MacroFiscalData%>%
                  dplyr::filter(Year==actual_year_simulation)%>%
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
                MainResultsCustoms1$value<-round(MainResultsCustoms1$value,2)
                
                MainResultsFinal<-MainResultsCustoms1%>%
                  dplyr::rename("Description"= "variable",
                                "Value"="value")
               
})
        
        