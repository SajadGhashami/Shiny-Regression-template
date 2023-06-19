#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(waiter)
library(shinycssloaders)
library(DT)
library(DBI)
library(tidyverse)

# Global option
options(shiny.maxRequestSize=3000*1024^2)


con <- DBI::dbConnect(odbc::odbc(),
                      #Snowflake
                      #SnowflakeDSIIDriver
                      Driver       = "SnowflakeDSIIDriver",
                      Server       = "ed87949.us-east-1.snowflakecomputing.com",                      
                      UID          = rstudioapi::askForPassword("Database user"),
                      PWD          = rstudioapi::askForPassword("Database password"),
                      Database     = "EDW",
                      Warehouse    = "COMPUTE_LARGE",
                      Schema       = "dim"
                      #,
                      #authenticator = "externalbrowser"
)
mywh <- DBI::dbSendQuery(con, 'use role analyst_role')
#mywh <- DBI::dbSendQuery(con, 'use role developer_role')
mywh <- DBI::dbSendQuery(con, 'use warehouse COMPUTE_LARGE')

# Random seed
set.seed(27)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  #md = TRUE,
  # skin = "midnight",
  #skin = "blue",
  #skin = "blue",
  dashboardHeader(title = "Regression App", titleWidth = 200),
  dashboardSidebar( width = 200,
    sidebarMenu(
      menuItem("Import data", tabName = "import", icon = icon("table")),
      menuItem("Visualize data", tabName = "Visual", icon = icon("pie-chart")),
      menuItem("Regression", tabName = "model", icon = icon("line-chart")),
      menuItem("Dashboard", tabName = "result", icon = icon("dashboard"))
    )
  ),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  dashboardBody(
    
    tabItems(
      # First tab content
      
      tabItem(tabName = "import",
              
              box(
                title = "Import you data", status = "primary", solidHeader = FALSE, width= 12,
                "First lets import the data", br(), "Choose the method:",
                wellPanel( 
                  
                         offset=3,
                         radioButtons("importmethod",
                                      NULL,
                                      c( "I want to write a query"="querytext" ,
                                         "I have a CSV file"="csvfile"  ),
                                      inline = TRUE)
                  ),
                htmlOutput("chosenimport"),
                htmlOutput("importdetails"),
                br(),
                fluidRow( 
                  column(12,
                  shinydashboard::valueBoxOutput("tablerow"),
                  shinydashboard::valueBoxOutput("tablecol"),
                  ),
                  column(12,
                  shinydashboard::valueBoxOutput("numbcolnum"),
                  shinydashboard::valueBoxOutput("factcolnum"),
                  shinydashboard::valueBoxOutput("datecolnum")
                  )
                ),
                br(),
                column(12,
                       withSpinner(dataTableOutput("querydata")
                ))
              )
              )
              
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$chosenimport <- renderUI({
    switch(input$importmethod, 
           "querytext" =  textAreaInput("query",
                                        "Write your Query here(Dont use commented lines \"--\")",
                                        "SELECT *\nFROM analytics.sandbox.initial_teacher_cluster_data\nLIMIT 200",
                                        width = validateCssUnit("100%"),
                                        height = validateCssUnit('200px'),
                                        #placeholder = "SELECT"
           ),
           "csvfile" = fileInput("file1",
                                 "Choose CSV File",
                                 accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
           )
    )
  })
  
  output$importdetails <- renderUI({
    switch(input$importmethod, 
           "querytext" =  actionButton("go", "Run and Update Data", class = "btn-warning", width = "100%"),
           "csvfile" = checkboxInput("header", "Does your data have Header?", TRUE)
    )
  })
  
  importvalues <- reactiveValues(statusqueryerror= NULL, successmessage= NULL, iserrorquery= NULL)
  
  textquery <- eventReactive(input$go, {
    id <- showNotification("Running the Query(The Peformance depends on SNOWFLAKE server)", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    
    trythequery <- tryCatch(DBI::dbGetQuery(con, input$query),
                            error=function(e) NULL)
    
    defaultresult <- tryCatch(DBI::dbGetQuery(con,"SELECT 'There is something wrong in the query' AS ERROR"),
                              error=function(e) NULL)
    
    
    importvalues$iserrorquery <- if (is.null(tryCatch(trythequery, error=function(e) NULL))) {
      1
    } else {
      0  }
    
    result <- if (is.null(trythequery)) { 
      defaultresult } else {
        trythequery  }
    
    errormessage <- if (is.null(trythequery)) {
      paste("Ooops... There is a problem with the query. Make sure it is not commented.
             It only reads the data so you can not change the warehouse or etc.",
            "Maybe you should try your query in Snowflake first",
            sep="\n") } else {
              ""  }
    
    successmessage <- if (!is.null(trythequery)) {
      paste("Well done your query worked",
            sep="\n") } else {
              ""  }
    
    
    
    importvalues$statusqueryerror <- errormessage
    
    importvalues$statusquerysuccess <- successmessage
    
    return(result)
    
  })
  
  output$queryerror <- renderUI({
    switch(input$importmethod, 
           "querytext" =  renderText(importvalues$statusqueryerror),
           "csvfile" = ""
    )
  })
  
  output$querysucess <- renderUI({
    switch(input$importmethod, 
           "querytext" =  renderText(importvalues$statusquerysuccess),
           "csvfile" = ""
    )
  })
  
  warningnumeric <- reactive({ 
    if (is.null(importvalues$iserrorquery)) {
      NULL
    } else {
      
      if (importvalues$iserrorquery==1) {
        NULL
      } else {
        if (numberofNumerical()==0) {
          paste("Notice that You can not perform Clustering Modeling as there are 0 numerical variables ",
                sep="\n")
        } else {
          if (numberofNumerical()==1) { 
            "It is recommended to have at least one more numerical variable. (You have only 1 now)"
          } else {
            NULL
          }
        }
      }
    }
  })
  
  output$numericwarning <- renderUI({
    switch(input$importmethod, 
           "querytext" =  renderText(warningnumeric()),
           "csvfile" = renderText(warningnumeric())
    )
  })
  
  
  
  #  output$queryerror <- renderText(importvalues$statusquery)
  
  filedata <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read_delim(inFile$datapath, col_names = input$header)
  })
  
  # based on the button the users clicks(Update or input file)
  values <- reactiveValues(df_data = NULL, datebins= NULL)
  
  observeEvent(input$file1, {
    values$df_data <- filedata()
    names(values$df_data) <- make.names(names(values$df_data), unique=TRUE)
  })
  
  observeEvent(input$go, {
    temp <- textquery()
    values$df_data <- temp
    names(values$df_data) <- make.names(names(values$df_data), unique=TRUE)
    
  })
  
  # Number of rows
  #rownumb
  output$tablerow <- renderValueBox({
    shinydashboard::valueBox( #VB_style( paste0( '$',format(tmp_ex_tot,big.mark=','), " m"), "font-size: 60%;"  ),
      
      tryCatch(if(is.null(values$df_data)) {
        0 
      } else {
        dim(values$df_data)[1]
      },
      error=function(e) 0 ),       
      
      "Rows",
      color = "blue"
      # icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
      #color = "blue"
    )
  })
  
  output$tablecol <- renderValueBox({
    shinydashboard::valueBox( #VB_style( paste0( '$',format(tmp_ex_tot,big.mark=','), " m"), "font-size: 60%;"  ),
      
      tryCatch(if(is.null(values$df_data)) {
        0 
      } else {
        dim(values$df_data)[2]
      },
      error=function(e)0),
      "Columns",
      #icon("sign-in"),
      #icon = icon('export', lib = 'glyphicon'), 
      color = "blue"
    )
  })
  
  output$numbcolnum <- renderValueBox({
    shinydashboard::valueBox( #VB_style( paste0( '$',format(tmp_ex_tot,big.mark=','), " m"), "font-size: 60%;"  ),
      
      tryCatch(numberofNumerical(), error=function(e)0),
      "Numerical Columns",
      icon("sign-in"),
      #icon = icon('export', lib = 'glyphicon'), 
      color = "blue"
    )
  })
  
  output$factcolnum <- renderValueBox({
    shinydashboard::valueBox( #VB_style( paste0( '$',format(tmp_ex_tot,big.mark=','), " m"), "font-size: 60%;"  ),
      
      tryCatch(ifelse(is.null(numberoffactors()),0, numberoffactors()), error=function(e)0),
      "Categorical Columns",
      #icon = icon('export', lib = 'glyphicon'),
      icon("sign-in"),
      color = "blue"
    )
  })
  
  output$datecolnum <- renderValueBox({
    shinydashboard::valueBox( #VB_style( paste0( '$',format(tmp_ex_tot,big.mark=','), " m"), "font-size: 60%;"  ),
      
      tryCatch(numberofdates(), error=function(e)0),
      "Date Columns",
      #icon = icon('export', lib = 'glyphicon'),
      # icon("sign-in"),
      color = "blue"
    )
  })
  
  output$querydata <- renderDataTable({
    id <- showNotification("Reading the data", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    datatable(textquery(),filter = list(position = 'top', clear = FALSE), options = list(autoWidth = TRUE, bAutoWidth = FALSE,  scrollX = TRUE) )
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
