" Tax Expenditures Customs Dashboard "

# I.Function for Dashboard ------------------------------------------------------------------
#CustomsDuties_TE_Sections,CustomsDuties_TE_MTN,CustomsDuties_TE_agg_countries_tbl,

TE_Charts_Customs_fun <- function(ProjectionTE_customs,CustomsDuties_TE_agg_countries_tbl_agg,
                                  CustomsDuties_TE_Chapters,CustomsDuties_TE_MTN,
                                     forecast_horizon,SimulationYear) {
  # Check if forecast_horizon is provided and is valid
  if (missing(forecast_horizon) || length(forecast_horizon) != 2) {
    stop("Please provide a valid 'forecast_horizon' with minimum and maximum values.")
  }
  # Chart 1. Tax expenditures by Group of products by HS -----------------------------------------------------------------
                    
 
          # te_agg_plt <- plot_ly(
          #   #ProjectionTE_customs,
          #   ProjectionTE_customs%>%filter(tax_expenditures>0),
          #             x = ~Year,
          #             y = ~round(tax_expenditures*1e06,1),
          #             name = "Baseline",
          #             type = 'scatter',
          #             mode = 'lines+markers+text',
          #             text = ~round(tax_expenditures,1),
          #             textposition = 'top middle',
          #             hoverinfo = 'text+y',
          #             line = list(width = 4, dash = "solid")
          #           ) %>%
          #             layout(
          #               #title = paste("Total Tax Expenditures Forecast (EUR MIL),", min(forecast_horizon), "-", max(forecast_horizon)),
          #               title = paste("Total Tax Expenditures Forecast (EUR MIL),", min(forecast_horizon), "-", 2025),
          #               xaxis = list(title = '', tickformat = 'd'),
          #               yaxis = list(title = ' ', rangemode = 'tozero'),
          #               annotations = list(
          #                 x = -0.02,
          #                 y = -0.1,
          #                 text = "Source: WB staff estimation",
          #                 showarrow = FALSE,
          #                 xref = 'paper',
          #                 yref = 'paper',
          #                 align = 'left'
          #               )
          #             )
          
  ProjectionTE_customs_plot <- ProjectionTE_customs %>%
    filter(tax_expenditures > 0) %>%
    arrange(Year)
  
  te_agg_plt <- plot_ly(
    data = ProjectionTE_customs_plot,
    x = ~Year,
    y = ~round(tax_expenditures * 1e06, 1),
    name = "Baseline",
    type = "scatter",
    mode = "lines+markers+text",
    text = ~round(tax_expenditures, 1),
    textposition = "top middle",
    hovertemplate = paste(
      "Year: %{x}<br>",
      "Tax expenditures: %{text} EUR mil<extra></extra>"
    ),
    line = list(width = 4, dash = "solid"),
    marker = list(size = 8)
  ) %>%
    layout(
      title = paste(
        "Total Tax Expenditures (EUR MIL),",
        min(forecast_horizon),
        "-",
        2025
      ),
      xaxis = list(
        title = "",
        tickmode = "linear",
        tick0 = min(ProjectionTE_customs_plot$Year, na.rm = TRUE),
        dtick = 1
      ),
      yaxis = list(
        title = " ",
        rangemode = "tozero"
      ),
      annotations = list(
        x = -0.02,
        y = -0.1,
        text = "Source: WB staff estimation",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        align = "left"
      )
    )
    

  
  # Chart 2. Tax expenditures by Top 15 Importing Countries  -----------------------------------------------------------------
            
          CustomsDuties_TE_agg_countries_tbl_agg <- CustomsDuties_TE_agg_countries_tbl %>%
            arrange(desc(CustomsDuties_TE)) %>%  
            filter(Year==SimulationYear)%>%
            slice(1:15)  
          
              
          custom_colors <- c('#1f77b4', '#ff7f0e')  
          
          Sections_HS <- plot_ly(CustomsDuties_TE_agg_countries_tbl_agg, 
                                       x = ~reorder(iso3c, -CustomsDuties_TE),  # Reverse order for highest on left
                                       y = ~CustomsDuties_TE,               
                                       colors = custom_colors,   
                                       hoverinfo = 'text', 
                                 hovertext = ~Countries ,
                                       type = 'bar', 
                                       barmode = 'group') %>%
                                  layout(
                                    title = paste("Distribution of Tax Expenditures by the Top 15 Countries,", SimulationYear),
                                    xaxis = list(title = " ", tickmode = 'linear'),  
                                    yaxis = list(title = " "),
                                    annotations = list(
                                      list(
                                        x = -0.02,
                                        y = -0.1,
                                        text = "Source: WB staff estimation",
                                        showarrow = FALSE,
                                        xref = 'paper',
                                        yref = 'paper',
                                        align = 'left',
                                        font = list(size = 10)  
                                      )
                                    ))
      
              
  
  # Chart 3.Tax expenditures by HS Sections -------------------------------------------------------------------------
                
          CustomsDuties_TE_Chapters$Treatment<-as.character(c("HS Chapters"))
          
          CustomsDuties_TE_Chapters<-CustomsDuties_TE_Chapters%>%filter(Year==SimulationYear)
          
            Chapters_HS <- plot_ly(data = CustomsDuties_TE_Chapters,
                                 type = "treemap",
                                 values = ~round(CustomsDuties_TE/1e06,1),
                                 labels = ~Chapter,
                                 parents = ~Treatment ,
                                 name = " ",
                                 text = ~Chapters_description ,
                                 textinfo = "label+value+percent parent") %>%
                            layout(
                                title = list(
                                  text = paste("Distribution of Tax Expenditures accross HS Chapters,", SimulationYear),  
                                  font = list(size = 14)  
                                ),
                                annotations = list(
                                  x = 0.05,
                                  y = -0.01,
                                  text = "Source: WB staff estimation",
                                  showarrow = FALSE,
                                  xref = 'paper',
                                  yref = 'paper',
                                  xanchor = 'center',
                                  yanchor = 'top',
                                  font = list(size = 10)
                                )
                              )
                          
  
  # Chart 4. Tax expenditures by MTN Categories ---------------------------------------------------------------
  
            
          ProductGroups_MTN <- plot_ly(
                                      CustomsDuties_TE_MTN%>%filter(Year==SimulationYear),
                                      
                                      y = ~reorder(Product_group, CustomsDuties_TE),
                                      x = ~CustomsDuties_TE,
                                      type = 'bar',
                                      text = ' ',
                                      hoverinfo = 'x+text',
                                      hovertext = ~Product_group,
                                      marker = list(color = '#ff7f0e')
                                    )
          
          ProductGroups_MTN <- ProductGroups_MTN %>%
                          layout(
                            title = paste("Tax expenditures by MTN Categories,", SimulationYear),
                            font = list(size = 11),
                            yaxis = list(
                              title = '', 
                              tickangle = 0,  # Keep the labels horizontal
                              automargin = TRUE,  # Ensure enough margin for longer labels
                              tickmode = 'linear',
                              ticks = "outside",
                              standoff = 10  # Add space between the axis and labels
                            ),
                            xaxis = list(title = ''),
                            annotations = list(
                              x = -0.02,
                              y = -0.1,
                              text = "Source: WB staff estimation",
                              showarrow = FALSE,
                              xref = 'paper',
                              yref = 'paper',
                              align = 'left'
                            )
                          )
                        
              
              
              
             
  
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    te_agg_plt=te_agg_plt,
    Sections_HS=Sections_HS,
    Chapters_HS=Chapters_HS,
    ProductGroups_MTN=ProductGroups_MTN
    
  )
}


