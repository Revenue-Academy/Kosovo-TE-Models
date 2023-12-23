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
        menuItem("Results",  icon = icon("gauge"),
                # menuSubItem("TaxExpenditures_HS", tabName = "HS_CODES"),
                 #menuSubItem("TE_Countries", tabName = "TE_agg_countries"),
                 menuSubItem("MainResults", tabName = "MainResultsExciseFinal"
                             )
        ),
        #menuItem("Charts", tabName = "Excise-charts"),
        menuItem("Charts", tabName = "Excise-charts",icon = icon("chart-line"),
                 menuSubItem("ExciseRevenue", tabName = "ExciseRevenue"), 
                 menuSubItem("ExciseTaxExpenditures", tabName = "ExciseTaxExpenditures")
                 ),
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
                            h4("Customs Data"),
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
     tabItem(
      "Excise-input",
      fluidRow(
        column(6,
               h4("Excise Data"),
               selectInput("inputTypeExcise", "Data Source",
                           choices = c("Excel File"),
                           selected = "Excel File"
               ),
               conditionalPanel(
                 condition = "input.inputTypeExcise == 'Excel File'",
                 fileInput("fileInputExcise", "Upload Excel File", accept = c(".xlsx")),
                 checkboxInput("hasHeaderExcise", "Header", TRUE)
               ),
               actionButton("resetInputExcise", "Reset")
        )
      )
    ),
    tabItem(
      "Excise-simulationParameters",
      fluidRow(
        column(
          12,
          DTOutput("excelDataTableExcise"),  # Updated name for Excise
          br(),
          actionButton("updateGlobalDataExcise", "Update"),  # Updated name for Excise
          actionButton("resetGlobalDataExcise", "Reset Imported Data")  # Updated name for Excise
        )
      )
    ),
    tabItem("Excise-simulation", 
            fluidRow(
              column(2,
                     h4("Fuels"),
                     sliderInput("simulationSlider01", "Excise benchmark",
                                 min = 0, max = 0.8, step = 0.001, value = 0.3850, width = "100%"),
                     actionButton("ExciseSimulation", "Run Simulation")
              ),
              column(2,
                     h4("Tobacco"),
                     sliderInput("simulationSlider02", "Excise benchmark",
                                 min = 0, max = 100, step = 1, value = 73, width = "100%"),
              ),
              column(2,
                     h4("Alcohol"),
                     sliderInput("simulationSlider03", "EUR/HLTR abs.alc 100% vol",
                                 min = 0, max = 1000, step = 100, value = 800, width = "100%")
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
      tabItem(
        tabName = "MainResultsExciseFinal",
        fluidRow(
          column(12,
                 #DTOutput("TableOutputId")
                 DTOutput("TableOutput_Excise")
          )
        )
      ),
  # Charts tab
  tabItem(
    tabName = "ExciseRevenue",
    fluidRow(
      column(6,
             selectInput("chartSelectExciseRevenue", "Select Chart",
                         choices = c(
                           "Excise_PctOfGDP1",
                           "StructureOfTaxRevenues_Nominal1", 
                           "StructureOfTaxRevenues_Percentage1", 
                           "ExciseGoodRegulaImport_plt1",
                           "ImportStructureExcise1",
                           "ExciseProductCategories1",
                           "Structure_Excise_MineralOils",
                           "Structure_Excise_TobaccoProducts",
                           "Structure_Excise_AlcoholProducts"
                           
                         ),
                         selected = "Excise_PctOfGDP1")
      )
    ),
    fluidRow(
      column(12,
             plotlyOutput("chartOutputExciseRevenue", height = "700px")
      )
    )
  ),
  tabItem(
    tabName = "ExciseTaxExpenditures",
    fluidRow(
      column(6,
             selectInput("chartSelectTaxExpendituresExcise", "Select Chart",
                         choices = c(
                           "Chapters_HS1","ProductGroups_MTN1","ExciseStructure_MineralOils","ExciseStructure_TobaccoProducts","ExciseStructure_AlcoholProducts"
                         ),
                         selected = "Chapters_HS1")
      )
    ),
    fluidRow(
      column(12,
             plotlyOutput("chartOutputExciseTaxExpenditures", height = "700px")
      )
    )
  ),
  tabItem("Excise-summary",
          fluidRow(
            uiOutput("exciseInfoBox"),
            column(12,
                   plotlyOutput("Excise_PctOfGDP1", height = "700px"))
          )
        )
     )
    )
  )
)

# II. Define the server logic ----------------------------------------------

server <- function(input, output, session) {

# 1. Import Excel file for Customs duties model  -------------
   originalExcelData <- reactiveVal(NULL)
   editedExcelData <- reactiveVal(NULL)

   observe({
    if (!is.null(input$fileInput)) {
      excel_data <- read_excel(input$fileInput$datapath)
      originalExcelData(excel_data)
      editedExcelData(excel_data)
    }
  })
#   Define a DataTable using the reactive data frame
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

#   Observer to update the reactive data frame when the DataTable is edited
  observeEvent(input$excelDataTable_cell_edit, {
    info <- input$excelDataTable_cell_edit

    if (!is.null(editedExcelData())) {
      modifiedData <- editedExcelData()
      modifiedData[info$row, info$col] <- info$value

      # Update the reactive data frame
      editedExcelData(modifiedData)
    }
  })

#   Button click event to update the Global Environment
  observeEvent(input$updateGlobalData, {
    global_data <- editedExcelData()

    # Store the updated data in the global environment
    assign("Import_raw_monthly", global_data, envir = .GlobalEnv)
  })

  # Button click event to reset the imported data
  observeEvent(input$resetGlobalData, {
    editedExcelData(originalExcelData())
  })

# 2. Import Excel file for Excise duties model  -------------------------

  originalExcelDataExcise <- reactiveVal(NULL)
  editedExcelDataExcise <- reactiveVal(NULL)
  
  observe({
    if (!is.null(input$fileInputExcise)) {
      excel_data <- read_excel(input$fileInputExcise$datapath)
      originalExcelDataExcise(excel_data)
      editedExcelDataExcise(excel_data)
    }
  })
  
  output$excelDataTableExcise <- renderDT({
    data <- editedExcelDataExcise()
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
  
  observeEvent(input$excelDataTableExcise_cell_edit, {
    info <- input$excelDataTableExcise_cell_edit
    
    if (!is.null(editedExcelDataExcise())) {
      modifiedData <- editedExcelDataExcise()
      modifiedData[info$row, info$col] <- info$value
      
      editedExcelDataExcise(modifiedData)
    }
  })
  
  observeEvent(input$updateGlobalDataExcise, {
    global_data_excise <- editedExcelDataExcise()
    assign("Import_Excise_Data", global_data_excise, envir = .GlobalEnv)
  })
  
  observeEvent(input$resetGlobalDataExcise, {
    editedExcelDataExcise(originalExcelDataExcise())
  })
  
# 3. Customs Duties model -----------------------------------------------
# 3.1 Execution of scripts for the Customs Duties model ------------------------------------------------------
  observeEvent(input$runSimulation, {
    
    setwd(path1)
    getwd()
    
    # Assign input value to  in the global environment
    Benchmark_Customs_Rate <- input$simulationSlider
    assign("Benchmark_Customs_Rate", Benchmark_Customs_Rate, envir = .GlobalEnv)
  
    #  Check if Import_raw_monthly exists in the global environment
    if (exists("Import_raw_monthly", envir = .GlobalEnv)) {
      # Execute scripts excluding ExciseModel_TE.R
      source("./Scripts/Customs/ChartsParametars-Module.R")
      source("./Scripts/Customs/CustomsModel_TE.R")
      source("./Scripts/Customs/Export-Module.R")
    } else {

    }

    # 3.2 Customs duties Tables ------------------------------------------------------

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
  
    # 3.3 Drop down menu for Customs Charts ---------------------------------------------------
    # 3.4 Chart selection --------------------------------------------------------
        # 3.4.1 Historic Data Charts -----------------------------------------------------
           output$chartOutputCustomsRevenue <- renderPlotly({
                              switch(input$chartSelectCustomsRevenue,
                                     "ImportDuties_PctOfGDP" = ImportDuties_PctOfGDP,
                                     "StructureOfTaxRevenues_Nominal" = StructureOfTaxRevenues_Nominal,
                                     "StructureOfTaxRevenues_Percentage" = StructureOfTaxRevenues_Percentage,
                                     "RegularImport" = RegularImport,
                                     "ImportStructure" = ImportStructure
                                     )
                                    })
        # 3.4.2 Tax Expenditures Charts ---------------------------------------
                      
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

                                  
    # 3.5. Info Boxes -------------------------------------------------------------
        
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
    # 3.6 Show notification -------------------------------------------------------
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

# 4. Excise Simulation  ------------------------------------------------------------------

#  4.1 Execution of scripts for the Excise model----------------------------

  observeEvent(input$ExciseSimulation, {
    setwd(path1)
    getwd()
    
    # Assign input values to  in the global environment
    Benchmark_ExciseFuels <- input$simulationSlider01
    Benchmark_ExciseTobacco <- input$simulationSlider02
    Benchmark_ExciseAlcohol <- input$simulationSlider03
    Benchmark_ExciseCars <- input$simulationSlider04

    assign("Benchmark_ExciseFuels", Benchmark_ExciseFuels, envir = .GlobalEnv)
    assign("Benchmark_ExciseTobacco", Benchmark_ExciseTobacco, envir = .GlobalEnv)
    assign("Benchmark_ExciseAlcohol", Benchmark_ExciseAlcohol, envir = .GlobalEnv)
    assign("Benchmark_ExciseCars", Benchmark_ExciseCars, envir = .GlobalEnv)
 
    # # Check if Import_raw_monthly exists in the global environment
    if (exists("Import_Excise_Data", envir = .GlobalEnv)) {
      # Execute scripts excluding ExciseModel_TE.R
      source("./Scripts/Customs/ChartsParametars-Module.R")
      source("./Scripts/Excise/ExciseModel_TE.R")
     # source("./Scripts/Customs/Export-Module.R")
    } else {

    }
    
    # 4.2 Excise Duties Tables --------------------------------------------------
    
    # Historic data import
    setwd(path)
    getwd()
    
    
    # Show Excise tables
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
    output$TableOutput_Excise <-renderDT({
      datatable(MainResultsExciseFinal,
                caption = tags$caption(paste("Main results from simulation in LCU (Millions),", actual_year_simulation), class = "table-caption-bold"),
                extensions='Buttons',
                options = list(
                  pageLength = 15,
                  dom = 'Blfrtip',
                  buttons=c('copy','csv','excel','print','pdf'),
                  lengthMenu = list(c(10,25,50,-1),c(10,25,50,"All"))))
    })
    
    
    # 4.3 Drop down menu for Excise Charts -----------------------------------------------------

    # 4.3.1 Historic Data -----------------------------------------------------
    
        output$chartOutputExciseRevenue <- renderPlotly({
          switch(input$chartSelectExciseRevenue,
                 "Excise_PctOfGDP1" = Excise_PctOfGDP1,
                 "StructureOfTaxRevenues_Nominal1" = StructureOfTaxRevenues_Nominal1,
                 "StructureOfTaxRevenues_Percentage1" = StructureOfTaxRevenues_Percentage1,
                 "ExciseGoodRegulaImport_plt1" = ExciseGoodRegulaImport_plt1,
                 "ExciseProductCategories1"= ExciseProductCategories1,
                 "ImportStructureExcise1" = ImportStructureExcise1,
                 "Structure_Excise_MineralOils"=Structure_Excise_MineralOils,
                 "Structure_Excise_TobaccoProducts"=Structure_Excise_TobaccoProducts,
                 "Structure_Excise_AlcoholProducts"=Structure_Excise_AlcoholProducts

          )
        })
        
    
    # 4.3.2 Tax Expenditures Charts -------------------------------------------

    output$chartOutputExciseTaxExpenditures <- renderPlotly({
      switch(input$chartSelectTaxExpendituresExcise,
             "Chapters_HS1" = Chapters_HS1,
             "ProductGroups_MTN1" = ProductGroups_MTN1,
             "ExciseStructure_MineralOils"=ExciseStructure_MineralOils,
             "ExciseStructure_TobaccoProducts"=ExciseStructure_TobaccoProducts,
             "ExciseStructure_AlcoholProducts"=ExciseStructure_AlcoholProducts
             
      )
    })
    
    
    # 4.4   Info Boxes ---------------------------------------------
    
    value1 <- MainResultsExciseFinal %>%
      filter(Description == "Actual Total Excise Revenues") %>%
      select(Value)
    
    value2 <- MainResultsExciseFinal %>%
      filter(Description == "Tax Expenditures") %>%
      select(Value)
    
    value3 <- MainResultsExciseFinal %>%
      filter(Description == "Tax Expenditures as % of GDP") %>%
      select(Value)
    
    value4 <- MainResultsExciseFinal %>%
      filter(Description == "Tax Expenditures as % of Government Revenue") %>%
      select(Value)
    
    # Update the content of the info boxes
    output$exciseInfoBox <- renderUI({
      info_boxes <- list(
        infoBox("Actual Total Excise Revenues", value1, icon = icon("hand-holding-dollar"), width = 4, color = "red"),
        infoBox("Tax Expenditures", value2, icon = icon("chart-pie"), width = 4, color = "green"),
        infoBox("Tax Expenditures as % of GDP", value3,  icon = icon("gauge"), width = 4,color = "blue"),
        infoBox("Tax Expenditures as % of Government Revenue", value4,  icon = icon("chart-pie"), width = 4,color = "orange")
        
        # Add more info boxes if needed
      )
      do.call(tagList, info_boxes)
    })

    # 4.5 Show notification -------------------------------------------------------
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
  
  
}

shinyApp(ui, server)
