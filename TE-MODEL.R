library(shinydashboard)
library(DT)
library(readxl)
library(openxlsx)
library(shinyjs)
library(plotly)
library(ggplot2)
library(data.table)
library(fontawesome)
library(shiny)
library(ggplot2)
library(tidyverse)
library(kableExtra)
library(stringr)
library(reshape2)
library(base64enc)
library(countrycode)

#Day 1 training
#Besart testing

options(scipen=999)
options(shiny.maxRequestSize = 30*1024^2) # defining a maximum file size

# I. Define the UI ---------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(
    title = "TE Model",
    # Add a navbarPage for the tabPanel titles in the header
    tags$li(
      class = "dropdown",
      navbarPage(
        id = "vertical-tabs",
        title = "",
        tabPanel("CustomsDuties", ""),
        tabPanel("Excise", "")
      )
    )
  ),
  dashboardSidebar(
    id = " ",
    conditionalPanel(
      condition = 'input["vertical-tabs"] == "CustomsDuties"',
      sidebarMenu(
                    menuItem("Input", tabName = "CustomsDuties-input",icon = icon("database")),
                    menuItem("Simulation Parameters", tabName = "CustomsDuties-simulationParameters",icon = icon("edit")),
                    menuItem("Run simulation", tabName = "CustomsDuties-simulation",icon = icon("calculator")),
                    menuItem("Results",  icon = icon("gauge"),
                             menuSubItem("TaxExpenditures_HS", tabName = "HS_CODES"),
                             menuSubItem("TE_Countries", tabName = "TE_agg_countries"),
                             menuSubItem("MainResults", tabName = "MainResults")
                    ),
                    menuItem("Charts", tabName = "CustomsDuties-charts",icon = icon("chart-line"),
                                menuSubItem("CustomsRevenue", tabName = "CustomsRevenue"), 
                                menuSubItem("TaxExpenditures", tabName = "TaxExpenditures")),
                    menuItem("Summary", tabName = "CustomsDuties-summary",icon = icon("info"))
      
      )
    ),
    # Excise
    conditionalPanel(
      condition = 'input["vertical-tabs"] == "Excise"',
      sidebarMenu(
        menuItem("Input", tabName = "Excise-input"),
        menuItem("Simulation Parameters", tabName = "Excise-simulationParameters"),
        menuItem("Simulation", tabName = "Excise-simulation"),
        menuItem("Charts", tabName = "Excise-charts"),
        menuItem("Summary", tabName = "Excise-summary")
      )
    )
  ),
  # Customs duties
  dashboardBody(
    useShinyjs(),
    tabsetPanel(
      id = "mainTabset", # Add an id to the tabsetPanel
               tabItems(
                 tabItem(
                   "CustomsDuties-input", 
                   fluidRow(
                     column(6,
                            selectInput("inputType", "Data Source",
                                        choices = c("Excel File"),
                                        selected = "Excel File"),
                            conditionalPanel(
                              condition = "input.inputType == 'Excel File'",
                              fileInput("fileInput", "Upload Excel File", accept = c(".xlsx")),
                              checkboxInput("hasHeader", "Header", TRUE)
                            ),
                            actionButton("resetInput", "Reset")
                     )
                   )
                 ),
                tabItem("CustomsDuties-simulation", 
                         fluidRow(
                           column(6,
                                  sliderInput("simulationSlider", "Customs benchmark rate",
                                              min = 0, max = 1, step = 0.01, value = 0.10),
                                  actionButton("runSimulation", "Run Simulation")
                           ),
                           column(6,
                                  # Table to display the slider values
                                  tableOutput("sliderValueTable")
                           )
                         )
                 ),
                tabItem("CustomsDuties-simulationParameters",
                         fluidRow(
                           column(
                             12,
                             DTOutput("excelDataTable"),
                             br(),
                             actionButton("updateGlobalData", "Update"),
                             actionButton("resetGlobalData", "Reset Imported Data")
                           )
                         )
                 ),
              
                 tabItem(
                   tabName = "MainResults",
                   fluidRow(
                     column(12,
                            DTOutput("TableOutputId")
                     )
                   )
                 ),
                # New
            tabItem(
              tabName = "HS_CODES",
              fluidRow(
                column(12,
                       DTOutput("HS_CODE_TE")
                )
              )
            ),
            tabItem(
              tabName = "TE_agg_countries",
              fluidRow(
                column(12,
                       DTOutput("TE_agg_countries")
                )
              )
            ),
          # Charts tab
                tabItem(
                  tabName = "CustomsRevenue",
                  fluidRow(
                    column(6,
                           selectInput("chartSelectCustomsRevenue", "Select Chart",
                                       choices = c(
                                         "ImportDuties_PctOfGDP","StructureOfTaxRevenues_Nominal","StructureOfTaxRevenues_Percentage","RegularImport","ImportStructure" 
                                       ),
                                       selected = "ImportDuties_PctOfGDP")
                    )
                  ),
                  fluidRow(
                    column(12,
                           plotlyOutput("chartOutputCustomsRevenue", height = "700px")
                    )
                  )
                ),
                 tabItem(
                   tabName = "TaxExpenditures",
                   fluidRow(
                     column(6,
                            selectInput("chartSelectTaxExpenditures", "Select Chart",
                                        choices = c(
                                          "GroupOfProducts_HS","Sections_HS","Chapters_HS","ProductGroups_MTN","Sectors_CPA", 
                                          "ChoroplethMap" 
                                        ),
                                        selected = "GroupOfProducts_HS")
                     )
                   ),
                   fluidRow(
                     column(12,
                            plotlyOutput("chartOutputTaxExpenditures", height = "700px")
                     )
                   )
                 ),
                
                tabItem("CustomsDuties-summary",
                 fluidRow(
                   uiOutput("infoBoxUI"),
                   column(12,
                          plotlyOutput("treemap_final_plt", height = "700px")))),
    # Excise duties   
                # tabItem("Excise-input", "This is the Excise Input content"),
    
    tabItem(
      "Excise-input",
      fluidRow(
        column(6,
               selectInput("inputType", "Data Source",
                           choices = c("Excel File"),
                           selected = "Excel File"),
               conditionalPanel(
                 condition = "input.inputType == 'Excel File'",
                 #fileInput("fileInput", "Upload Excel File", accept = c(".xlsx")),
                 fileInput("ExcelFileExcise", "Upload Excel File", accept = c(".xlsx")),
                 checkboxInput("hasHeader", "Header", TRUE)
               ),
               actionButton("resetInput", "Reset")
        ))),
    tabItem("Excise-simulationParameters",
            fluidRow(
              column(
                12,
                DTOutput("excelDataTableExcise"),# New name
                br(),
                actionButton("updateGlobalData", "Update"),
                actionButton("resetGlobalData", "Reset Imported Data")
              )
            )
          ),
    # tabItem("Excise-simulation",
    #         fluidRow(
    #           column(6,
    #                  sliderInput("simulationSlider", "Petroleum gases and other gaseous hydrocarbons",
    #                              min = 0, max = 1, step = 0.01, value = 0.10),
    #                  actionButton("runSimulation", "Run Simulation")
    #           ),
    #           column(6,
    #                  # Table to display the slider values
    #                  tableOutput("sliderValueTableExcise")
    #           )
    #         )
    # ),
  # This part 
    tabItem("Excise-simulation", 
            fluidRow(
              column(2,
                     h4("Fuels"),
                     sliderInput("simulationSlider01", "Excise benchmark",
                                 min = 0, max = 0.1, step = 0.0001, value = 0.0385, width = "100%"),
                     actionButton("ExciseSimulation", "Run Simulation")
              ),
              column(2,
                     h4("Tobacco"),
                     sliderInput("simulationSlider02", "Excise benchmark",
                                 min = 0, max = 100, step = 1, value = 53, width = "100%"),
                    
              ),
              column(2,
                     h4("Alcohol"),
                     sliderInput("simulationSlider03", "Excise benchmark",
                                 min = 0, max = 0.7, step = 0.0001, value = 0.385, width = "100%")
              ),
              # Sugar-sweetened beverages

              column(2,
                     h4("Vehicles"),
                    sliderInput("simulationSlider04", "Excise benchmark",
                                 min = 0, max = 4000, step = 100, value = 1800, width = "100%")
                   
            ),
           
              column(7,
                     # Table to display the slider values
                     tableOutput("sliderValueTableExcise")
              )
            )
    ),
                 tabItem("Excise-charts", "This is the Excise Charts content"),
                 tabItem("Excise-summary", "This is the Excise Summary content")
              
    
     )
    )
  )
)


        

# II. Define the server logic ----------------------------------------------

server <- function(input, output, session) {

# 1. Import Excel file for Customs duties model - create a reactive data frame to store the editable Excel data -------------


  originalExcelData <- reactiveVal(NULL)
  editedExcelData <- reactiveVal(NULL)
  
  observe({
    if (!is.null(input$fileInput)) {
      excel_data <- read_excel(input$fileInput$datapath)
      originalExcelData(excel_data)
      editedExcelData(excel_data)
    }
  })
  
  # Define a DataTable using the reactive data frame
  output$excelDataTable <- renderDT({
    data <- editedExcelData()
    if (!is.null(data)) {
      datatable(
        data,
        caption = "Regular Import by Customs Code in LCU",
        editable = TRUE,
        options = list(
          pageLength = 20,
          columnDefs = list(
            list(
              targets = c(0:4),
              className = "not-editable"
            )
          )
        )
      )
    }
  })
  
  # Observer to update the reactive data frame when the DataTable is edited
  observeEvent(input$excelDataTable_cell_edit, {
    info <- input$excelDataTable_cell_edit
    
    if (!is.null(editedExcelData())) {
      modifiedData <- editedExcelData()
      modifiedData[info$row, info$col] <- info$value
      
      # Update the reactive data frame
      editedExcelData(modifiedData)
    }
  })
  
  # Button click event to update the Global Environment
  observeEvent(input$updateGlobalData, {
    global_data <- editedExcelData()
    
    # Store the updated data in the global environment
    assign("Import_raw_monthly", global_data, envir = .GlobalEnv)
  })
  
  # Button click event to reset the imported data
  observeEvent(input$resetGlobalData, {
    editedExcelData(originalExcelData())
  })
  

# 2. # Import Excel file for Excise duties model - create a reactive data frame to store the editable Excel data -------------------------

  originalExcelData <- reactiveVal(NULL)
  editedExcelData <- reactiveVal(NULL)
  
  observe({
    if (!is.null(input$ExcelFileExcise)) {
      excel_data <- read_excel(input$ExcelFileExcise$datapath)
      originalExcelData(excel_data)
      editedExcelData(excel_data)
    }
  })
  
  # Define a DataTable using the reactive data frame
  output$excelDataTableExcise <- renderDT({
    data <- editedExcelData()
    if (!is.null(data)) {
      datatable(
        data,
        caption = "Regular Import by Customs Code in LCU",
        editable = TRUE,
        options = list(
          pageLength = 20,
          columnDefs = list(
            list(
              targets = c(0:4),
              className = "not-editable"
            )
          )
        )
      )
    }
  })
  
  # Observer to update the reactive data frame when the DataTable is edited
  observeEvent(input$excelDataTable_cell_edit, {
    info <- input$excelDataTable_cell_edit
    
    if (!is.null(editedExcelData())) {
      modifiedData <- editedExcelData()
      modifiedData[info$row, info$col] <- info$value
      
      # Update the reactive data frame
      editedExcelData(modifiedData)
    }
  })
  
  # Button click event to update the Global Environment
  observeEvent(input$updateGlobalData, {
    global_data <- editedExcelData()
    
    # Store the updated data in the global environment
    assign("Import_Excise_Data", global_data, envir = .GlobalEnv)
  })
  
  # Button click event to reset the imported data
  observeEvent(input$resetGlobalData, {
    editedExcelData(originalExcelData())
  })
  
  

# 3.Simulation with scripts -----------------------------------------------

# 3.1 Customs duties ------------------------------------------------------

  observeEvent(input$runSimulation, {
    
    setwd(path1)
    getwd()
    
    # Assign rentInput value to VAT_Input in the global environment
    Benchmark_Customs_Rate <- input$simulationSlider
    assign("Benchmark_Customs_Rate", Benchmark_Customs_Rate, envir = .GlobalEnv)
  
    # Execute the scripts OLD
    source("./Scripts/Customs/ChartsParametars-Module.R")
    source("./Scripts/Customs/CustomsModel_TE.R")
    #source("./Scripts/Customs/ExciseModel_TE.R")
    source("./Scripts/Customs/Export-Module.R")
    
    # Check if Import_raw_monthly exists in the global environment
    # if (exists("Import_raw_monthly", envir = .GlobalEnv)) {
    #   # Execute scripts excluding ExciseModel_TE.R
    #   source("./Scripts/Customs/ChartsParametars-Module.R")
    #   source("./Scripts/Customs/CustomsModel_TE.R")
    #   source("./Scripts/Customs/Export-Module.R")
    # } else {
    #   # Execute scripts excluding CustomsModel_TE.R
    #   source("./Scripts/Customs/ChartsParametars-Module.R")
    #   source("./Scripts/Excise/ExciseModel_TE.R")
    #   source("./Scripts/Customs/Export-Module.R")
    # }
    
    
  
    # 1. Custins duties Tables  -------------------------------------------------------------
    
    # Historic data import
    setwd(path)
    getwd()

    # Table 1
    output$HS_CODE_TE<-renderDT({
      datatable(CustomsDuties_TE_agg_HS_subset,
                caption = tags$caption(paste("Tax expenditures by HS codes in LCU,", actual_year_simulation), class = "table-caption-bold"),
                extensions='Buttons',
                options = list(
                  pageLength = 15,
                  dom = 'Blfrtip',
                  buttons=c('copy','csv','excel','print','pdf'),
                  lengthMenu = list(c(10,25,50,-1),c(10,25,50,"All"))))
    })

    # Table 2
    output$TE_agg_countries <-renderDT({
      datatable(CustomsDuties_TE_agg_countries_tbl,
                caption = tags$caption(paste("Tax expenditures in LCU (Millions),", actual_year_simulation), class = "table-caption-bold"),
                extensions='Buttons',
                options = list(
                  pageLength = 15,
                  dom = 'Blfrtip',
                  buttons=c('copy','csv','excel','print','pdf'),
                  lengthMenu = list(c(10,25,50,-1),c(10,25,50,"All"))))
    })
    
    
    # Table 3
    output$TableOutputId <-renderDT({
      datatable(MainResultsFinal,
                caption = tags$caption(paste("Main results from simulation in LCU (Millions),", actual_year_simulation), class = "table-caption-bold"),
                extensions='Buttons',
                options = list(
                  pageLength = 15,
                  dom = 'Blfrtip',
                  buttons=c('copy','csv','excel','print','pdf'),
                  lengthMenu = list(c(10,25,50,-1),c(10,25,50,"All"))))
    })
  

# 2.Excise Duties Tables --------------------------------------------------

    # Historic data import
    setwd(path)
    getwd()
    
    # # Table 1
    # output$HS_CODE_TE<-renderDT({
    #   datatable(CustomsDuties_TE_agg_HS_subset,
    #             caption = tags$caption(paste("Tax expenditures by HS codes in LCU,", actual_year_simulation), class = "table-caption-bold"),
    #             extensions='Buttons',
    #             options = list(
    #               pageLength = 15,
    #               dom = 'Blfrtip',
    #               buttons=c('copy','csv','excel','print','pdf'),
    #               lengthMenu = list(c(10,25,50,-1),c(10,25,50,"All"))))
    # })
    # 
    # # Table 2
    # output$TE_agg_countries <-renderDT({
    #   datatable(CustomsDuties_TE_agg_countries_tbl,
    #             caption = tags$caption(paste("Tax expenditures in LCU (Millions),", actual_year_simulation), class = "table-caption-bold"),
    #             extensions='Buttons',
    #             options = list(
    #               pageLength = 15,
    #               dom = 'Blfrtip',
    #               buttons=c('copy','csv','excel','print','pdf'),
    #               lengthMenu = list(c(10,25,50,-1),c(10,25,50,"All"))))
    # })
    # 
    # 
    # Table 3
    output$TableOutputId <-renderDT({
      datatable(MainResultsExciseFinal,
                caption = tags$caption(paste("Main results from simulation in LCU (Millions),", actual_year_simulation), class = "table-caption-bold"),
                extensions='Buttons',
                options = list(
                  pageLength = 15,
                  dom = 'Blfrtip',
                  buttons=c('copy','csv','excel','print','pdf'),
                  lengthMenu = list(c(10,25,50,-1),c(10,25,50,"All"))))
    })
    
    
    
    
    
    # 2. Charts ------------------------------------------------------------------

    # CustomsRevenue Tab
    
            # Charts 1 
    
           MacroFiscalData$ImportDuties_PctOfGDP<-round(MacroFiscalData$ImportDuties_PctOfGDP,2)
      
            df_plt <- MacroFiscalData %>%
              dplyr::select(Year, GDP, ImportDuties_PctOfGDP)
    
  
                                    ImportDuties_PctOfGDP <- plot_ly(df_plt)
                                    ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% add_trace(x = ~Year, y = ~GDP, type = 'bar', name = 'GDP')
                                    ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% add_trace(x = ~Year, y = ~ImportDuties_PctOfGDP, type = 'scatter', mode = 'lines+markers', name = 'Share of customs revenue in GDP ',
                                                                                                 yaxis = 'y2', line = list(dash = 'dot', color = "#FFA500", width = 6))
                                    
                                    ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% layout(title = 'Nominal GDP and share of customs revenues in GDP,2015-2022',
                                                                                                        xaxis = list(title = ""),
                                                                                                        yaxis = list(side = 'left', title = 'In million LCU', showgrid = FALSE, zeroline = FALSE),
                                                                                                        yaxis2 = list(side = 'right', overlaying = "y", title = 'Percentage', showgrid = FALSE, zeroline = FALSE, font = list(size = 11)), 
                                                                                    annotations = list(
                                                                                                        x = -0.1, y = -0.056,
                                                                                                        text = "Source: National authorities",
                                                                                                        showarrow = FALSE,
                                                                                                        xref = 'paper',
                                                                                                        yref = 'paper',
                                                                                                        align = 'left'))
                                                            
                                    df_plt <- MacroFiscalData %>%
                                      dplyr::select(Year, GDP, ImportDuties_PctOfGDP)
                                                    ImportDuties_PctOfGDP <- plot_ly(df_plt)
                                                    ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% add_trace(x = ~Year, y = ~GDP, type = 'bar', name = 'GDP')
                                                    ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% add_trace(
                                                      x = ~Year, y = ~ImportDuties_PctOfGDP, type = 'scatter', mode = 'lines+markers', name = 'Share of customs revenue in GDP ',
                                                      yaxis = 'y2', line = list(dash = 'dot', color = "#FFA500", width = 4)  # Set width to 4 for a bold dotted line
                                                    )
                                    
                                    ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% layout(
                                                                                                title = 'Nominal GDP and share of customs revenues in GDP,2015-2022',font = t_11,
                                                                                                xaxis = list(title = ""),
                                                                                                yaxis = list(side = 'left', title = 'In million LCU', showgrid = FALSE, zeroline = FALSE),
                                                                                                yaxis2 = list(side = 'right', overlaying = "y", title = 'Percentage', showgrid = FALSE, zeroline = FALSE, font = list(size = 8)), 
                                                                                                annotations = list(
                                                                                                  x = 0, y = -0.05,
                                                                                                  text = "Source: National authorities",font = t_8,
                                                                                                  showarrow = FALSE,
                                                                                                  xref = 'paper',
                                                                                                  yref = 'paper',
                                                                                                  align = 'left'
                                                                                                )
                                                                                              )
            
              
                                    ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>%
                                      layout(
                                        images = list(
                                          source = base64_image,  # Use the base64-encoded image data
                                          xref = "paper",
                                          yref = "paper",
                                          x = 0,
                                          y = 1,
                                          sizex = 0.05,  
                                          sizey = 0.05,  
                                          opacity = 0.8
                                        )
                                      )
                            
            # Chart 2
            
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
                                                                                  layout(title = 'Structure of tax revenues',font = t_11,
                                                                                         xaxis = list(title = ''), 
                                                                                         yaxis = list(title = ' '),
                                                                                         annotations =
                                                                                           list( x = 0, y = -0.05, 
                                                                                                text = "Source:National authorities",font = t_8,
                                                                                                showarrow = F,
                                                                                                xref='paper',
                                                                                                yref='paper',align='left'),barmode = 'stack')
                                              
    
              # Chart 3
              year_df<-group_by(year_df, Year) %>% mutate(Pct = value/sum(value))
              
              
              StructureOfTaxRevenues_Percentage <- plot_ly(year_df, x = ~Year, y = ~Pct*100,
                                                    type = 'bar',
                                                    marker = list(color = ~color), name = ~variable) %>%
                                              layout(title = 'Structure of revenues in percentage, 2015-2022',font = t_11,
                                                     xaxis = list(title = ''), 
                                                     yaxis = list(title = 'In percentage '),
                                                     annotations =
                                                       list( x = 0, y = -0.05, 
                                                            text = "Source:National authorities",font = t_8,
                                                            showarrow = F,
                                                            xref='paper',
                                                            yref='paper',align='left'),barmode = 'stack')
              
             
             # Chart 4
             
             RegularImport<-plot_ly(data =CustomsValue, type = "treemap", values = ~round(Value/1e06,1), labels = ~Chapter,
                                    parents = ~HS,
                                    name = " ",
                                    text = ~Chapters_description   ,
                                    textinfo="label+value+percent parent")%>%
               layout(title=paste("Structure of Total Regular Import by HS Chapters in LCU(Millions),", actual_year_simulation),font =t_11)
             

             # Chart 5
             
             ImportStructure <- CustomsDuties_base_pie %>% 
               plot_ly(labels = ~Treatment, values = ~value)
             
                     ImportStructure <- ImportStructure %>% add_pie(hole = 0.6)
                     ImportStructure <- ImportStructure %>% layout(
                       title = paste("Structure of Import by Treatment of Goods (preferential vs non-preferential)", actual_year_simulation),
                       font = t_11, 
                       showlegend = TRUE,
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       annotations = list(
                         x = 0, y = -0.1,
                         title = "Additional Information:",  # New subtitle
                         text = "Preferential treatment includes goods covered by Free Trade Agreements, while non-preferential treatment includes only imports subject to customs tariffs.",
                         showarrow = FALSE,
                         xref = 'paper',
                         yref = 'paper',
                         align = 'left'
                       ),
                       font = t_8
                     )
             
             
             
    # TaxExpenditures Tab 
    
              # Chart 1
            
             GroupOfProducts_HS <- plot_ly(CustomsDuties_TE_type_products, x = ~type_products , y = ~value, type = 'bar', text = ' ', hoverinfo = 'y+text', color = ~type_products, colors = colors) %>% 
                                 layout(
                                   title = paste("Tax expenditures by Group of products by HS,", actual_year_simulation),
                                   font = list(size = 11),
                                   xaxis = list(title = ''),
                                   yaxis = list(title = 'In LCU'),
                                   #barmode = 'stack',  # Use 'stack' for multiple colors within a single bar
                                   annotations = list(
                                     x = 0, y = -0.056,
                                     text = "Source: Calculations by WB staff based on data from National authorities",
                                     showarrow = FALSE,
                                     xref = 'paper',
                                     yref = 'paper',
                                     align = 'left'
                                   ),
                                   legend = list(orientation = 'v', x = 1.02, y = 0.5)
                                 )
             
    
              # Charts 2
            
             Sections_HS <- plot_ly(
                             CustomsDuties_TE_Sections, 
                             x = ~reorder(Sections, -CustomsDuties_TE), 
                             y = ~CustomsDuties_TE, 
                             type = 'bar', 
                             text = ' ', 
                             hoverinfo = 'y+text', 
                             hovertext = ~Section_description,
                             marker = list(color = '#ff7f0e')  # Set the color of all bars to orange
                           ) %>%
                           layout(
                             title = paste("Tax expenditures by HS Sections,", actual_year_simulation),
                             font = t_11,
                             font = list(size = 11),
                             xaxis = list(title = ''),
                             yaxis = list(title = 'In LCU'),
                             annotations = list(
                               x = 0, y = -0.056,
                               text = "Source: Calculations by WB staff based on data from National authorities",
                               showarrow = FALSE,
                               xref = 'paper',
                               yref = 'paper',
                               align = 'left'
                             ),
                             legend = list(orientation = 'v', x = 1.02, y = 0.5)
                           )
                       
             
                  # Charts 3
                  
             
                   Chapters_HS <- plot_ly(CustomsDuties_TE_Chapters, x = ~reorder(Chapter, -CustomsDuties_TE), y = ~CustomsDuties_TE, type = 'bar', text = ' ', hoverinfo = 'y+text',#color = ~Treatment, colors = colors,
                                                        hovertext = ~Chapters_description) %>%
                                   layout(
                                     title = paste("Tax expenditures by HS Chapters,", actual_year_simulation),font = t_11,
                                     font = list(size = 11),
                                     xaxis = list(title = ''),
                                     yaxis = list(title = 'In LCU'),
                                     # barmode = 'stack',
                                     annotations = list(
                                       x = 0, y = -0.056,
                                       text = "Source: Calculations by WB staff based on data from National authorities",
                                       showarrow = FALSE,
                                       xref = 'paper',
                                       yref = 'paper',
                                       align = 'left'
                                     ),
                                     #legend = list(orientation = 'h')
                                     legend = list(orientation = 'v', x = 1.02, y = 0.5)
                                   )
                                

                            
                            # Chart 4
                         
                   ProductGroups_MTN <- plot_ly(
                     CustomsDuties_TE_MTN, 
                     y = ~reorder(Product_group, CustomsDuties_TE), 
                     x = ~CustomsDuties_TE, 
                     type = 'bar', 
                     text = ' ', 
                     hoverinfo = 'x+text',
                     hovertext = ~Product_group,
                     marker = list(color = '#d62728')
                   ) 
                   
                   # Add the layout step separately
                   ProductGroups_MTN <- ProductGroups_MTN %>% 
                     layout(
                       title = paste("Tax expenditures by Multilateral Trade Negotiations Categories,", actual_year_simulation),
                       font = t_11,
                       font = list(size = 11),
                       yaxis = list(title = ''),
                       xaxis = list(title = 'In LCU'),
                       annotations = list(
                         x = -0.1, y = -0.056,
                         text = "Source: Calculations by WB staff based on data from National authorities",
                         showarrow = FALSE,
                         xref = 'paper',
                         yref = 'paper',
                         align = 'left'
                       )
                     )
                  
                            #  Chart 5
              
                          Sectors_CPA <- plot_ly(CustomsDuties_TE_SECTORS, y = ~reorder(CPA21_NAME , CustomsDuties_TE), x = ~CustomsDuties_TE, type = 'bar', text = ' ', hoverinfo = 'x+text',#color = ~Treatment, colors = colors,
                                                        hovertext = ~CPA21_NAME) %>%
                                         layout(
                                           title = paste("Tax expenditures by CPA Sectors", actual_year_simulation),font = t_11,
                                           font = list(size = 11),
                                           yaxis = list(title = ''),
                                           xaxis = list(title = 'In LCU'),
                                           barmode = 'stack',
                                           annotations = list(
                                             x = -0.1, y = -0.056,
                                             text = "Source: Calculations by WB staff based on data from National authorities",
                                             showarrow = FALSE,
                                             xref = 'paper',
                                             yref = 'paper',
                                             align = 'left'
                                           )
                                         )
                            
                            
                                    
                        #  Chart 5
                                    
                                    mapdata3 <- left_join(mapdata_iso3c,CustomsDuties_TE_agg_countries,by = c("iso3"="iso3c"))
                                    
                                    # Removing Antarctica
                                    mapdata3<-mapdata3[!(mapdata3$region=="Antarctica"),]
                                    
                                    map1 <- ggplot(mapdata3, aes(x= long , y=lat,group=group)) +
                                      geom_polygon(aes(fill=CustomsDuties_TE), color="black")
                                    

                                    
                                  ChoroplethMap <- map1 + scale_fill_gradient(name= "In LCU million",
                                                                       low = "lightgreen",
                                                                       high = "darkgreen",
                                                                       na.value="grey90") +
                                                                                        theme_minimal()+
                                                                                        theme(plot.title = element_text(face = "bold"),
                                                                                              axis.title.x=element_blank(),
                                                                                              axis.text.x=element_blank(),
                                                                                              axis.ticks.x=element_blank(),
                                                                                              axis.title.y=element_blank(),
                                                                                              axis.text.y=element_blank(),
                                                                                              axis.ticks.y=element_blank(),
                                                                                              legend.position="bottom")+
                                    labs(title = paste("                                                                                      Geographic distribution of Kosovo tax expenditures,", actual_year_simulation))

                          
    # 3.Drop down menu for Charts ---------------------------------------------------
                      
      # 3.1 CustomsRevenue ---------------------------------------
      output$chartOutputCustomsRevenue <- renderPlotly({
                        switch(input$chartSelectCustomsRevenue,
                               "ImportDuties_PctOfGDP" = ImportDuties_PctOfGDP,
                               "StructureOfTaxRevenues_Nominal" = StructureOfTaxRevenues_Nominal,
                               "StructureOfTaxRevenues_Percentage" = StructureOfTaxRevenues_Percentage,
                               "RegularImport"=RegularImport,
                               "ImportStructure"=ImportStructure
                               
                               )
                      })
                      
                      
      # 3.2 TaxExpenditures ---------------------------------------
                      
    output$chartOutputTaxExpenditures <- renderPlotly({
      switch(input$chartSelectTaxExpenditures,
             "GroupOfProducts_HS"=GroupOfProducts_HS,
             "Sections_HS" = Sections_HS,
             "Chapters_HS" = Chapters_HS,
             "ProductGroups_MTN" = ProductGroups_MTN,
             "Sectors_CPA"=Sectors_CPA,
             "ChoroplethMap" = ChoroplethMap
      )
    })

    
    # 4. Info Boxes -------------------------------------------------------------

                        value1 <- MainResultsFinal %>%
                        filter(Description == "Tax Expenditures(without FTA)") %>%
                        select(Value)

                        value2 <- MainResultsFinal %>%
                          filter(Description == "Tax Expenditures(without FTA) as % of GDP") %>%
                          select(Value)
  
                        value3 <- MainResultsFinal %>%
                          filter(Description == "Tax Expenditures (as % of CustomsRevenue)") %>%
                          select(Value)
        
                        
                        value4 <-CustomsDuties_TE_agg_HS_subset$HS_code[1]
          
                        first_chapter <- CustomsDuties_TE_Chapters %>%
                          arrange(desc(CustomsDuties_TE)) 
                        value5 <- first_chapter$Chapters_description[1]
                        
                        
                        first_mtn <- CustomsDuties_TE_MTN %>%
                          arrange(desc(CustomsDuties_TE)) 
                        value6 <- first_mtn$Product_group[1]
                        
                        
                        
    
    # Update the content of the infoboxes
    output$infoBoxUI <- renderUI({
      info_boxes <- list(

        infoBox("Tax Expenditures (in million of LCU )", value1, icon = icon("hand-holding-dollar"), width = 4, color = "red"),
        infoBox("Tax Expenditures (as % of GDP)", value2, icon = icon("chart-pie"), width = 4,color = "green"),
        infoBox("Tax Expenditures (as % of Customs Revenue)", value3,  icon = icon("gauge"), width = 4,color = "blue"),
        infoBox("HS code with highest tax expenditures", value4,  icon = icon("chart-pie"), width = 4,color = "orange"),
        infoBox("HS Chapter with highest tax expenditures", value5,  icon = icon("chart-column"), width = 4,color = "navy"),
        infoBox("MTN group with highest tax expenditures", value6,  icon = icon("business-time"), width = 4,color = "maroon")
      )
      #  Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
      do.call(tagList, info_boxes)
    })

    
    # 5. Show notification -------------------------------------------------------
    # Show a centered modal notification when the simulation is done
    showModal(
      modalDialog(
        title = "Simulation complete !",
        footer = NULL,
        easyClose = TRUE,
        size = "s",
        draggable = TRUE
      )
    )
  }) 

# II. Excise Simulation  ------------------------------------------------------------------
  observeEvent(input$ExciseSimulation, {
    setwd(path1)
    getwd()
    
    # Assign rentInput value to VAT_Input in the global environment
    Benchmark_ExciseFuels <- input$simulationSlider01
    Benchmark_ExciseTobacco <- input$simulationSlider02
    Benchmark_ExciseAlcohol <- input$simulationSlider03
    Benchmark_ExciseCars <- input$simulationSlider04

    assign("Benchmark_ExciseFuels", Benchmark_ExciseFuels, envir = .GlobalEnv)
    assign("Benchmark_ExciseTobacco", Benchmark_ExciseTobacco, envir = .GlobalEnv)
    assign("Benchmark_ExciseAlcohol", Benchmark_ExciseAlcohol, envir = .GlobalEnv)
    assign("Benchmark_ExciseCars", Benchmark_ExciseCars, envir = .GlobalEnv)

    
    # Execute the scripts OLD
    source("./Scripts/Customs/ChartsParametars-Module.R")
    source("./Scripts/Excise/ExciseModel_TE.R")
    source("./Scripts/Customs/Export-Module.R")
  
  
      })
}

shinyApp(ui, server)
