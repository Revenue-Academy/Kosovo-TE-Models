
#path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/Models/Kosovo-TE-Models"
revenue_vat_total_bu <- read_excel("Data/VAT/export_data.xlsx",sheet="Est_Rev_BU")
est_rev_bu_te<- read_excel("Data/VAT/export_data.xlsx",sheet="Est_Rev_BU_TE")
result_simulation <- read_excel("Data/VAT/export_data.xlsx",sheet="Result_Simulation")
revenue_vat_total <- read_excel("Data/VAT/export_data.xlsx",sheet="Revenue_VAT_TOTAL")
simulation_vat_total<-read_excel("Data/VAT/export_data.xlsx",sheet="Est_Rev1")


te_agg <- read_excel("Data/VAT/export_data.xlsx",sheet="Results_1")
main_results<-read_excel("Data/VAT/export_data.xlsx",sheet="Main_Results")

MacroFiscalData<-read_excel(paste0(path1, "/Data/ImportData/MacroFiscalData/MacroFiscalData.xlsx"),
                            #"Data/ImportData/MacroFiscalData/MacroFiscalData.xlsx", 
                            sheet = "MacroFiscalData")





# I.Define chart parameters -------------------------------------------------
            
            
            # Define fonts
            t <- list(
              family = "Arial",
              size = 12
            )
            
            
            t_8 <- list(
              family = "Arial",
              size = 8,
              face="bold"
            )
            
            t_10 <- list(
              family = "Arial",
              size = 10,
              face="bold"
            )
            
            
            t_11 <- list(
              family = "Arial",
              size = 11,
              face="bold"
            )
            
            t_16 <- list(
              family = "Arial",
              size = 16,
              face="bold"
            )
            
            # Original colors
            colr <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#17becf', '#57a9e2',
                      '#ffb574', '#5fd35f', '#7f7f7f', '#e77c7c', '#c6aedc', '#bcbd22',
                      '#bc8b81', '#f4cce8', '#b2b2b2', '#9467bd', '#e2e362', '#e377c2',
                      '#5fe0ed', '#8c564b', '#103d5d', '#a74e00')
            
            # Generate additional unique colors
            additional_colors <- colorRampPalette(c("#00FF00", "#FF0000"))(102 - length(colr))
            
            # Combine the original and additional colors
            colr <- c(colr, additional_colors)        



            
            Benchmark_VAT<-main_results[1,2] 
            Uncalibrated_VAT<-main_results[2,2] 
            Calibrated_VAT<-main_results[3,2] 
            Total_VAT_Gap <-main_results[4,2] 
            Policy_Gap  <-main_results[6,2] 
            Compliance_Gap   <-main_results[8,2] 
            
            
            Total_VAT_Gap_Pct   <-main_results[5,2] 
            Policy_Gap_Pct   <-main_results[7,2] 
            Compliance_Gap_Pct <-main_results[9,2] 
            VAT_Benchmark_Rate<-main_results[10,2] 
            C_Efficiency <-main_results[11,2] 
           
            
            
            
            
# 1.Nominal GDP and share of VAT revenues in GDP -----------------------------------------------------
        
        MacroFiscalData <- MacroFiscalData[!is.na(MacroFiscalData$VAT), ]
        
        df_plt <- MacroFiscalData %>%
          dplyr::select(Year, GDP, VAT_PctOfGDP) %>%
          mutate(Excise_PctOfGDP = round(VAT_PctOfGDP, 1))
        
        
        # For value box !!!
        
        VAT_PctOfGDP_variable<-df_plt[8,3]
        
        
        # Extract last year
        last_year <- MacroFiscalData$Year[nrow(MacroFiscalData)]                            

        # Plot
          VAT_PctOfGDP <- plot_ly(df_plt)
          VAT_PctOfGDP <- VAT_PctOfGDP %>% add_trace(x = ~Year, y = ~GDP, type = 'bar', name = 'GDP')
          VAT_PctOfGDP <- VAT_PctOfGDP %>% add_trace(x = ~Year, y = ~VAT_PctOfGDP, type = 'scatter', mode = 'lines+markers', name = 'Share of VAT revenue in GDP ',
                                                     yaxis = 'y2', line = list(dash = 'dot', color = "#FFA500", width = 6))
          
          VAT_PctOfGDP <- VAT_PctOfGDP %>% layout(title = paste("Nominal GDP and share of VAT revenues in GDP,2015-", last_year),font = t_11,
                                                  xaxis = list(title = ""),
                                                  yaxis = list(side = 'left', title = 'In million LCU', showgrid = FALSE, zeroline = FALSE),
                                                  yaxis2 = list(side = 'right', overlaying = "y", title = 'Percentage', showgrid = FALSE, zeroline = FALSE, font = list(size = 8)))
                                  VAT_PctOfGDP <- VAT_PctOfGDP %>%layout(legend = list(orientation = 'h'))


# 2. VAT Revenues -------------------------------------------
            
            year_df <- MacroFiscalData%>%
              dplyr::select(Year, "VAT")
            
            year_df$Year<-as.factor(year_df$Year)
            year_df<-melt(year_df)
            
            VAT_Revenues_Nominal <- plot_ly(data = year_df, x = ~Year, y = ~value, 
                                            type = 'scatter', mode = 'lines', 
                                            name = ~variable, line = list(width = 4)) %>%
                                          layout(
                                            title = paste("VAT Revenues, 2015-", last_year),
                                            font = t_11,
                                            xaxis = list(title = ' '),
                                            yaxis = list(title = 'In million LCU'),
                                            font = t_11,
                                            annotations = list()
                                          )
            

# 3. Structure of tax revenues --------------------------------------------

           
            year_df <- MacroFiscalData%>%
              dplyr::select(Year, "VAT", "ImportDuties", "Taxes on imports excluding VAT and duties",
                            "Taxes on products, except VAT and import taxes",
                            "Other taxes on production", "Property income",
                            "Current taxes on income, wealth, etc.")
            
            year_df$Year<-as.factor(year_df$Year)
            year_df<-melt(year_df)
            year_df$color <- factor(year_df$variable, labels =c( "orange","brown","forestgreen","red", "cyan","royalblue","blue"))
            
            
            StructureOfTaxRevenues_Nominal <- plot_ly(year_df, x = ~Year, y = ~value,
                                                      type = 'bar',
                                                      marker = list(color = ~color), name = ~variable) %>%
                                                layout( title = paste("Structure of tax revenues, 2015-", last_year),font = t_11,
                                                        xaxis = list(title = ''),
                                                        yaxis = list(title = 'In million LCU'),
                                                        annotations =
                                                          list( 
                                                          ),
                                                        barmode = 'stack')



# 3. Structure of revenues in percentage----------------------------------------
                        
                        #year_df<-group_by(year_df, Year) %>% mutate(Pct = value/sum(value))
            year_df<-year_df%>%
            dplyr::group_by(Year) %>% 
            dplyr::mutate(Pct = value/sum(value))
            
                        VAT_StructureOfTaxRevenuesPct <- plot_ly(year_df, x = ~Year, y = ~Pct*100,
                                                                 type = 'bar',
                                                                 marker = list(color = ~color), name = ~variable) %>%
                                                            layout(title = paste("Structure of revenues in percentage, 2015-", last_year),font = t_11,
                                                                   xaxis = list(title = ''),
                                                                   yaxis = list(title = 'In percentage '),
                                                                  # annotations =list(),
                                                                   barmode = 'stack')
                        

                        
# 4.VAT Structure -----------------------------------------------------
    # 4.1 Industries -----------------------------------------------------------------
            # 4.1.1 Value box - VAT revenues from Industries-----------------------------------------------------------------
            
            VAT_Industries<-revenue_vat_total_bu%>%
                  dplyr::select(PRODUCT_INDUSTRY_CODE,Total_Revenues_from_Intermediate_Inputs)
            
                                    
            
            
            # 4.1.2 Treemap-VAT revenues from industries --------------------------
            
                        VAT_revenues_industries<-revenue_vat_total_bu%>%
                                                    dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_Revenues_from_Intermediate_Inputs)
                      
                        VAT_revenues_industries<-melt(VAT_revenues_industries)
                      
                        VAT_revenues_industries<-VAT_revenues_industries%>%
                                                    dplyr::mutate(value=round(value),0)%>%
                                                    dplyr::arrange(desc(value))
                      
                        VAT_revenues_industries$SUT_division<-"CPA division"
                      
                      
                        VAT_revenues_industries_treemap<-plot_ly(data = VAT_revenues_industries, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                                                       parents = ~SUT_division, name = " ",
                                                       text = ~PRODUCT_INDUSTRY_NAME,
                                                       marker = list(colors = setNames(colr, unique(VAT_revenues_industries$PRODUCT_INDUSTRY_CODE))), 
                                                       textinfo="label+value+percent parent")  %>%
                                                       layout(title="")
            
            
    # 4.2 NPISH ----------------------------------------------------------------------
          # 4.2.1 Value box - VAT revenues from NPISH--------------------------------------
                        VAT_NPISH<-revenue_vat_total_bu%>%
                                  dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_NPISH)
                        
          # 4.2.2 Treemap-VAT revenues from NPISH -------------------------------------------------
    
                VAT_revenues_NPISH<-revenue_vat_total_bu%>%
                                          dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_NPISH)
                
                VAT_revenues_NPISH<-melt(VAT_revenues_NPISH)
                
                VAT_revenues_NPISH<-VAT_revenues_NPISH%>%
                                            dplyr::mutate(value=round(value),0)%>%
                                            dplyr::arrange(desc(value))
                
                VAT_revenues_NPISH$SUT_division<-"CPA Divisions"
                
                VAT_revenues_NPISH_treemap<-plot_ly(data = VAT_revenues_NPISH, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                                            parents = ~SUT_division, name = " ",
                                            text = ~PRODUCT_INDUSTRY_NAME,
                                            marker = list(colors = setNames(colr, unique(VAT_revenues_NPISH$PRODUCT_INDUSTRY_CODE))), 
                                            textinfo="label+value+percent parent")  %>%
                                            layout(title="")    
    
    
    # 4.3 Government  ------------------------------------------------------------
          # 4.3.1 Value box VAT revenues from Government-----------------------------------------
                VAT_Government<-revenue_vat_total_bu%>%
                      dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_Government)
    
          # 4.3.2 Treemap-VAT revenues from Government --------------------------------------------
    
                VAT_revenues_Government<-revenue_vat_total_bu%>%
                                       dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_Government)
                
                VAT_revenues_Government<-melt(VAT_revenues_Government)
                
                VAT_revenues_Government<-VAT_revenues_Government%>%
                                      dplyr::mutate(value=round(value),0)%>%
                                      dplyr::arrange(desc(value))
                
                VAT_revenues_Government$SUT_division<-"CPA Divisions"
                
                
                VAT_revenues_Government_treemap<-plot_ly(data = VAT_revenues_Government, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                                            parents = ~SUT_division, name = " ",
                                            text = ~PRODUCT_INDUSTRY_NAME,
                                            marker = list(colors = setNames(colr, unique(VAT_revenues_Government$PRODUCT_INDUSTRY_CODE))), 
                                            textinfo="label+value+percent parent")  %>%
                                            layout(title="") 
    
    
    
    # 4.4 Households----------------------------------------------------------
          # 4.4.1 Value box VAT revenues from Households-----------------------------------------
                      VAT_Households<-revenue_vat_total_bu%>%
                        dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_HH)
          
          # 4.4.2 Treemap-VAT revenues from Households---------------------------------------------------
    
                VAT_revenues_Households<-revenue_vat_total_bu%>%
                                                 dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_HH)
                
                VAT_revenues_Households<-melt(VAT_revenues_Households)
                
                VAT_revenues_Households<-VAT_revenues_Households%>%
                                                dplyr::mutate(value=round(value),0)%>%
                                                dplyr::arrange(desc(value))
                
                VAT_revenues_Households$SUT_division<-"CPA Divisions"
                
                VAT_revenues_Households_treemap<-plot_ly(data = VAT_revenues_Households, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                                                  parents = ~SUT_division, name = " ",
                                                  text = ~PRODUCT_INDUSTRY_NAME,
                                                  marker = list(colors = setNames(colr, unique(VAT_revenues_Households$PRODUCT_INDUSTRY_CODE))), 
                                                  textinfo="label+value+percent parent")  %>%
                                                   layout(title="")    
                
                
                
    
# 5. Tax Expenditures -------------------------------------------------
      # 5.1 Value Box -----------------------------------------------------------

                te_agg_value_box<-sum(te_agg$Simulated_Change_in_Revenues.M_of_LCU)/1000
                
      # 5.2 Bar Chart (Actual VS Standard) -----------------------------------------------------------

                        dat_r<-revenue_vat_total_bu%>%
                                        dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_Total)%>%
                                        data.table()
                        
                        dat_te<-est_rev_bu_te%>%
                                        dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_Total)%>%
                                        data.table()
                        
                        
                        dat<-left_join(dat_r,dat_te,by = c("PRODUCT_INDUSTRY_CODE"))%>%
                                        select(PRODUCT_INDUSTRY_NAME.x,PRODUCT_INDUSTRY_CODE,Final_Demand_Total.y,Final_Demand_Total.x)

                                        colnames(dat) <- c('PRODUCT_INDUSTRY_NAME', 'PRODUCT_INDUSTRY_CODE','vat_revenue_after', 'vat_revenue_before')

                        Plot_Standard_Actual_Rates <- plot_ly(dat) %>% 
                                        add_trace(x = ~PRODUCT_INDUSTRY_CODE,  y = ~vat_revenue_before, type = 'bar', name = 'With actual VAT rates (standard and preferential)',hoverinfo = 'text', text = ~PRODUCT_INDUSTRY_NAME,
                                                  textposition = "none", hovertemplate = "%{text}") %>% 
                                        add_trace(x = ~PRODUCT_INDUSTRY_CODE,  y = ~vat_revenue_after, type = 'bar', name = 'With Benchmark VAT rates', hoverinfo = 'text', text = ~PRODUCT_INDUSTRY_NAME,
                                                  textposition = "none", hovertemplate = "%{text}") %>% 
                                                layout(
                                                  xaxis = list(title = ''), font = t_8,
                                                  yaxis = list(title = ''),font = t_8,
                                                  barmode = 'group',
                                                  title = 'Comparison of VAT Revenue Before and After Benchmarking across CPA divisions (in LCU)', font = t_11
                                                ) %>%
                                                layout(legend = list(orientation = 'h'))  # Set legend orientation to horizontal



  # 5.3 Value Box-------------------------------------------------

                        te_agg_value_pct<-sum(te_agg$Simulated_Change_in_Revenues.Prc)

    # 5.4 Bar Chart (Diff)-----------------------------------------------------

                      dat$diff<-dat$vat_revenue_after-dat$vat_revenue_before
                      
                      dat$diff<-round(dat$diff,1)

              BarChart_TE <- plot_ly(dat) %>% 
                                add_trace(x = ~PRODUCT_INDUSTRY_CODE,  y = ~diff, type = 'bar', hoverinfo = 'text', text = ~PRODUCT_INDUSTRY_NAME,
                                          textposition = "none") %>% 
                                layout(
                                  xaxis = list(title = ''), font = t_8,
                                  yaxis = list(title = ''), font = t_8,
                                  barmode = 'group',
                                  title = 'Tax Expenditures across CPA divisions (in LCU)',font = t_11
                                ) %>%
                                layout(legend = list(orientation = 'h'))  


              
# 6.Value box -------------------------------------------------------

        filtered_results <- main_results %>%
                        filter(variable == "Calibrated_VAT (in LCU Millions)") %>%
                        select(variable, `Difference(Simulation-Actual)`, `Pct_Change(Simulation/Actual)`)

              filtered_results_value_box <-sum(filtered_results $`Difference(Simulation-Actual)`)


# 6.1 Bar chart ----------------------------------------------------------------


              dat_r<-simulation_vat_total%>%
                      dplyr::select(PRODUCT_INDUSTRY_NAME,PRODUCT_INDUSTRY_CODE,Final_Demand_Total)%>%
                      data.table()
              
              dat_bu<-revenue_vat_total_bu%>%
                      dplyr::select(PRODUCT_INDUSTRY_NAME,PRODUCT_INDUSTRY_CODE,Final_Demand_Total)%>%
                      data.table()
              
              dat_rev<-left_join(dat_r,dat_bu,by = c("PRODUCT_INDUSTRY_CODE"))%>%
                select(PRODUCT_INDUSTRY_NAME.x,PRODUCT_INDUSTRY_CODE,Final_Demand_Total.y,Final_Demand_Total.x)
              
              
              colnames(dat_rev) <- c('PRODUCT_INDUSTRY_NAME', 'PRODUCT_INDUSTRY_CODE','vat_revenue_after', 'vat_revenue_before')
              
              BarChart_Simulation <- plot_ly(dat_rev) %>% 
                        add_trace(x = ~PRODUCT_INDUSTRY_CODE,  y = ~vat_revenue_before, type = 'bar', name = 'Before reform',hoverinfo = 'text', text = ~PRODUCT_INDUSTRY_NAME,
                                  textposition = "none", hovertemplate = "%{text}") %>% 
                        add_trace(x = ~PRODUCT_INDUSTRY_CODE,  y = ~vat_revenue_after, type = 'bar', name = 'After reform', hoverinfo = 'text', text = ~PRODUCT_INDUSTRY_NAME,
                                  textposition = "none", hovertemplate = "%{text}") %>% 
                        layout(
                          xaxis = list(title = ''), font = t_8,
                          yaxis = list(title = ''), font = t_8,
                          barmode = 'group',
                          title = 'VAT revenues (before and after reform)',font = t_11
                        ) %>%
                        layout(legend = list(orientation = 'h'))  # Set legend orientation to horizontal