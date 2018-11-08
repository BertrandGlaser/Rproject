library(shinydashboard)
library(readxl)
library(ggvis)
library(dplyr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(shinyjs)
library(leaflet)
library(htmltools)
library(arules)
library(arulesViz)

logs1 <- {}
survey1 <- {}

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Enter file", tabName="file",
               icon=icon("file", lib='glyphicon')),
      menuItem("All users", tabName="all_users",
               icon=icon("globe", lib='glyphicon')),
      menuItem("Single user", tabName="single_user",
               icon=icon("user", lib='glyphicon'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "file",
        titlePanel("Uploading Files"),
        sidebarLayout(
          sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/coma-separated-values, text/plain",
                        ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
          ),
          mainPanel(
            tableOutput("contents")
          ),
          "left"
        ),
        sidebarLayout(
          sidebarPanel(
            fileInput("file2", "Choose XLSX File",
                      multiple = FALSE,
                      accept = c(
                        "text/xlsx",
                        ".xlsx")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
          ),
          mainPanel(
            tableOutput("contents1")
          ),
          "left"
        )
        
        
      ),
      tabItem(tabName = "all_users",
              
              fluidPage(
                h2("All users content"),
                column(4,
                       selectInput('family_status', 'Select a specific family status', c(" ", "Single", "Married")),
                       selectInput('gender', 'Select a specific gender', c(" ", "Male", "Female")),
                       sliderInput("displayAge", "Age of population:", min=0, max=100, value=c(0,100),
                                   step=1),
                       sliderInput("displayWeight", "Weigh of population:", min=0, max=150, value=c(0,150),
                                   step=1),
                       sliderInput("displayHeight", "Height of population:", min=0, max=250, value=c(0,250),
                                   step=1),
                       selectInput('selectedColumn', 'Select a specific column of the data frame', "")
                       
                ),
                column(8,
                       plotOutput("plt1"),
                       plotOutput("plt2"),
                       plotOutput("plt3"),
                       plotOutput("plt4"),
                       plotOutput("plt5"),
                       plotOutput("plt6")
               )
              )
      ),
      tabItem(tabName = "single_user",
              fluidPage(
                h2("Single user content"),
                
                column(4, 
                       selectInput('selectedUser', 'Select a specific user', ""),
                       selectInput('displayTime', 'Select unit time', c("year", "month", "day", "hour"))
                ),
                column(8,
                       useShinyjs(),
                       tableOutput("tbl"),
                       plotOutput("plot1"),
                       plotOutput("plot2"),
                       plotOutput("plot3"),
                       plotOutput("plot4"),
                       plotOutput("plot5"),
                       plotOutput("plot6"),
                       plotOutput("plot7"),
                       plotOutput("plot8"),
                       leafletOutput("mymap")
                )
              )
              
              
              
              
      )
    )
  ))

server <- function(input, output, session)
{
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    req(input$file1)
    tryCatch(
      {
        logs1 <<- read.csv(input$file1$datapath, header = TRUE, sep = ";", fileEncoding = "MACROMAN" )
        logs1$Time <<- dmy_hm(logs1$Time)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(!is.null(input$header) & input$header == TRUE) {
      return(head(logs1))
    }
    else {
      return(NULL)
    }
    
  })
  output$contents1 <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    req(input$file2)
    tryCatch(
      {
        survey1 <<- read_excel(input$file2$datapath, 1)
        filter_survey <<- survey1
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(!is.null(input$header) & input$header == TRUE) {
      return(head(survey1))
    }
    else {
      return(NULL)
    }
    
  })
  
  observe({
    req(input$file1)
    tryCatch(
      {
        updateSelectInput(session, "selectedUser", choices = logs1$User)
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  observe({
    req(input$file2)
    tryCatch(
      {
        val <- input$displayAge
        updateSliderInput(session, "displayAge", value = val,
                          min = min(survey1$Age), max = max(survey1$Age), step = 1)
        
        val <- input$displayWheight
        updateSliderInput(session, "displayWeight", value = val,
                          min = min(survey1$`How much do you weigh? (kg)`), max = max(survey1$`How much do you weigh? (kg)`), step = 1)
        
        val <- input$displayHeight
        updateSliderInput(session, "displayHeight", value = val,
                          min = min(survey1$`What is your height? (cm)`), max = max(survey1$`What is your height? (cm)`), step = 1)
        updateSelectInput(session, "selectedColumn", choices = colnames(survey1))
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  observeEvent(input$selectedUser,{
    show("plot1")
    show("plot2")
    show("plot3")
    show("plot4")
    show("plot5")
    show("plot6")
    show("plot7")
    show("plot8")
    show("tbl")
   })
  
  sur <- reactiveValues()
  
  filterSurvey <- function(){
    sur <- survey1
    if(input$family_status != " "){
      sur <- filter(survey1, `Family status` == input$family_status)
    }
    if(input$gender != " "){
      sur <- filter(sur, Gender == input$gender)
    }
    sur <- filter(sur, Age >= input$displayAge[1])
    sur <- filter(sur, Age <= input$displayAge[2])
    sur <- filter(sur, `How much do you weigh? (kg)` >= input$displayWeight[1])
    sur <- filter(sur, `How much do you weigh? (kg)` <= input$displayWeight[2])
    sur <- filter(sur, `What is your height? (cm)` >= input$displayHeight[1])
    sur <- filter(sur, `What is your height? (cm)` <= input$displayHeight[2])
    return(sur)
  }
  
  output$plt1 <- renderPlot({
    req(input$file2)
    sur <- filterSurvey()
    if(nrow(sur) > 0){
      barplot(table(sur$Age), main="Age of population")
    }
    
  })
  
  output$plt2 <- renderPlot({
    req(input$file2)
    sur <- filterSurvey()
    if(nrow(sur) > 0){
      barplot(table(sur$`How much do you weigh? (kg)`), main="Weight of population")
    }
  })
  
  output$plt3 <- renderPlot({
    req(input$file2)
    sur <- filterSurvey()
    if(nrow(sur) > 0){
      col <- input$selectedColumn
      barplot(table(sur[col]), main="Personnalized plot")
    }
  })
  
  output$plt4 <- renderPlot({
    req(input$file2)
    sur <- filterSurvey()
    if(nrow(sur)>0){
      df <- select(sur, `User #`, Name, Education, `How many cigarettes do you smoke per day`)
      trans <- data.frame(sapply(df,as.factor))
      print(trans)
      rules <- apriori(trans, parameter = list(support = 0.1,conf = 0.4,target = "rules"))
      rules <- sort(rules, by='confidence', deacreasing= TRUE)
      #inspect(rules)
      plot(rules, method = "graph")
    }
  })
  
  output$plt5 <- renderPlot({
    req(input$file2)
    sur <- filterSurvey()
    if(nrow(sur)>0){
      df <- select(sur, `User #`, Name, `Have you had any changes in your alcohol consumption since your beginning of the ibriquet program?`, `Progress expectations`)
      trans <- data.frame(sapply(df,as.factor))
      rules <- apriori(trans, parameter = list(support = 0.1,conf = 0.4,target = "rules"))
      rules <- sort(rules, by='confidence', deacreasing= TRUE)
      #inspect(rules)
      plot(rules, method = "paracoord")
    }
  })
  
  output$plt6 <- renderPlot({
    req(input$file2)
    sur <- filterSurvey()
    if(nrow(sur)>0){
      df <- select(sur, `User #`, Name, `Smoking is relaxing for me`, `How soon after you wake up do you smoke your first cigarette?`)
      trans <- data.frame(sapply(df,as.factor))
      rules <- apriori(trans, parameter = list(support = 0.1,conf = 0.4,target = "rules"))
      rules <- sort(rules, by='confidence', deacreasing= TRUE)
      #inspect(rules)
      plot(rules, method = "matrix")
    }
  })
  
  concat <- function(x) {
    return(list(as.character(x)))
    
  }
  
  values_moment <- reactiveValues()
  
  getTimelog <- function(log, moment){
    switch(moment,
           year= { values_moment <- year(log$Time) },
           month = { values_moment <- month(log$Time) },
           day = { values_moment <- day(log$Time) },
           hour = { values_moment <- hour(log$Time) })
  }
  
  output$plot1 <- renderPlot({
    req(input$file1)
    user <- filter(logs1, User == input$selectedUser)
    if (nrow(user)>0){
      show("plot1")
      barplot(table(user$Type), main="Number of different mode used")
    }
    else{
      hide("plot1")
    }
    
  })
  
  output$plot2 <- renderPlot({
    req(input$file1)
    user <- filter(logs1, User == input$selectedUser)
    
    Auto_skipped <- filter(user, Type == "Auto skipped")
    if(nrow(Auto_skipped) > 0){
      show("plot2")
      values_moment <- getTimelog(Auto_skipped, input$displayTime)
      barplot(table(values_moment), main=paste(c('Use of Auto Skipped mode by ', input$displayTime), collapse=" "))
    }
    else{
      hide("plot2")
    }
    
  })
  
  output$plot3 <- renderPlot({
    req(input$file1)
    user <- filter(logs1, User == input$selectedUser)
    
    Behaviour <- filter(user, Type == "Behaviour")
    if(nrow(Behaviour) > 0){
      show("plot3")
      values_moment <- getTimelog(Behaviour, input$displayTime)
      barplot(table(values_moment), main=paste(c('Use of Behaviour mode by ', input$displayTime), collapse=" "))
    }
    else{
      hide("plot3")
    }
  })
  
  output$plot4 <- renderPlot({
    req(input$file1)
    user <- filter(logs1, User == input$selectedUser)
    Cheated <- filter(user, Type == "Cheated")
    if(nrow(Cheated)>0){
      show("plot4")
      
      values_moment <- getTimelog(Cheated, input$displayTime)
      barplot(table(values_moment), main=paste(c('Use of Cheated mode by ', input$displayTime), collapse=" "))
    }
    else{
      hide("plot4")
    }
  })
  
  output$plot5 <- renderPlot({
    req(input$file1)
    user <- filter(logs1, User == input$selectedUser)
    
    Friend <- filter(user, Type == "Friend")
    if(nrow(Friend)>0){
      show("plot5")
      values_moment <- getTimelog(Friend, input$displayTime)
      barplot(table(values_moment), main=paste(c('Use of Friend mode by ', input$displayTime), collapse=" "))
    }
    else{
      hide("plot5")
    }
    
  })
  
  output$plot6 <- renderPlot({
    req(input$file1)
    user <- filter(logs1, User == input$selectedUser)
    On_time <- filter(user, Type == "On time")
    if(nrow(On_time)>0){
      show("plot6")
      values_moment <- getTimelog(On_time, input$displayTime)
      barplot(table(values_moment), main=paste(c('Use of On time mode by ', input$displayTime), collapse=" "))
    }
    else{
      hide("plot6")
    }
  })
  
  output$plot7 <- renderPlot({
    req(input$file1)
    user <- filter(logs1, User == input$selectedUser)
    Skipped <- filter(user, Type == "Skipped")
    if(nrow(Skipped)>0){
      show("plot7")
      values_moment <- getTimelog(Skipped, input$displayTime)
      barplot(table(values_moment), main=paste(c('Use of Skipped mode by ', input$displayTime), collapse=" "))
    }
    else{
      hide("plot7")
    }
  })
  
  output$plot8 <- renderPlot({
    req(input$file1)
    user <- filter(logs1, User == input$selectedUser)
    Snoozed <- filter(user, Type == "Snoozed")
    if(nrow(Snoozed)>0){
      show("plot8")
      values_moment <- getTimelog(Snoozed, input$displayTime)
      barplot(table(values_moment), main=paste(c('Use of Snoozed mode by ', input$displayTime), collapse=" "))
    }
    else{
      hide("plot8")
    }
    
  })
  
  output$tbl <- renderTable({
    req(input$file1)
    req(input$file2)
    user <- filter(survey1, Name == input$selectedUser)
    if(nrow(user)>0){
      show("tbl")
      info <- select(user, Gender, Age, `Family status`, `How much do you weigh? (kg)`, `What is your height? (cm)`, `How many cigarettes do you smoke per day`)
      return(head(info))
    }
    else{
      hide("tbl")
      return(NULL)
    }
    
  })
  
  
  output$mymap <- renderLeaflet({
    req(input$file1)
    user <- filter(logs1, User == input$selectedUser)
    user <- na.omit(user)
    leaflet(data = user) %>% addTiles() %>%
      addMarkers(lat = ~Latitude, lng = ~Longitude, popup=~htmlEscape(paste(paste(Type, "mode uses at", sep = " "), Time, sep = " ")))
  })
  
  
}

shinyApp(ui, server)