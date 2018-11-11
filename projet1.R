library(shiny)
library(shinydashboard)
library(lubridate)
library(DT)
library(dplyr)
library(readxl)
library(plyr)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(id = "tab",
      menuItem("Enter file", tabName="file",
               icon=icon("file", lib='glyphicon')),
      menuItem("All users", tabName="all_users",
               icon=icon("globe", lib='glyphicon')),
      menuItem("Single user", tabName="single_user",
               icon=icon("user", lib='glyphicon'))
    ),
    uiOutput("selectInput")
    
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "file",
        titlePanel("Uploading Files"),
        fluidRow(
          sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
          ),
          mainPanel(
            DT::dataTableOutput("contents")
          )
            
        ),
        fluidRow(
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
            DT::dataTableOutput("contents1")
          )
          
        )
      ),
      tabItem(
        tabName = "single_user",
        fluidPage(
          fluidRow(
            # Dynamic infoBoxes
            valueBoxOutput("numberSmoked"),
            valueBoxOutput("numberSmokedPerDay"),
            valueBoxOutput("smokedPerDayFromSurvey")
          ),
          fluidRow(
            plotOutput("barplotPeriod")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  logs <- {}
  survey <- {}
  
  #table output in file tab relative to logs.csv
  output$contents <- DT::renderDataTable(
    {
      req(input$file1)
      logs <<- inputLogs()
      datatable(logs, options = list(pageLength = 5)) %>% formatStyle(
        'Type',
        target = 'row',
        backgroundColor = 'orange'
      )
    })
  
  #table output in file tab relative to surveyece.xlsx
  output$contents1 <- DT::renderDataTable(
    {
      survey <<- inputSurvey()
      datatable(survey, options = list(pageLength = 5)) %>% formatStyle(
        'Age',
        target = 'row',
        backgroundColor = 'cyan'
      )
    })
  
  #load logs.csv
  #preprocess data
  inputLogs <- reactive(
  {
    req(input$file1)
    infile <- input$file1
    if(is.null(infile))
    {
      return(NULL)
    }
    dataset <- read.csv(infile$datapath, header = TRUE, sep = ";", fileEncoding = "macroman")
    #convert column Time in lubridate Time
    dataset$TimeConvert <- dmy_hm(dataset$Time) 
    #add column weekDay which correspond to the day of the week
    dataset$weekDay <- weekdays(dataset$TimeConvert) 
    #add column periodDay which correspond to the period of day (morning evening night)
    ct <- strftime(dataset$TimeConvert, format="%H:%M",tz="UTC")
    dataset$periodDay <- cut(strptime(ct, format = "%H:%M",tz="UTC"), 
                          breaks = strptime(c("00:00","04:30", "12:00", "21:00", "23:59"), format = "%H:%M", tz="UTC"), 
                          labels = c("night","morning","evening","night"))
    
    return(dataset)
  })
  
  #load surveyece.xlsx
  inputSurvey <- reactive({
    req(input$file2)
    infile <- input$file2
    if(is.null(infile))
    {
      return(NULL)
    }
    survey <- read_excel(input$file2$datapath, 1)
    return(survey)
  })
  
  #create selectInput dependant on which tab we are
  output$selectInput <- renderUI({
    req(input$file1)
    dyn_ui <- NULL
    if (input$tab == "single_user") {
      
      dyn_ui <- list(selectInput('selectedUser', 'Select a specific user', choices = unique(logs$User)))
      
    } 
    if (input$tab == "2") {
      
      dyn_ui <- list(selectInput("s1", label = "Select", choices = letters[1:3]),
                     selectInput("s2", label = "Select2", choices = letters[4:6]))
    }
    return(dyn_ui)
  })
  
  #Output the number of cigarette smoked by the user
  output$numberSmoked <- renderValueBox({
    req(input$file1)
    #Filter data
    #By Name of the user
    #By Type of action because for instance when using Friend mode this is not the user who smoke
    data <- filter(logs, User == input$selectedUser & Type %in% c("Behaviour", "Ontime", "Cheat"))
    valueBox(
      value = nrow(data), 
      subtitle = tags$p("smoked cigarette using iBriquet", style = "font-weight: bold"), 
      icon = icon("list")
    )
  })
  
  #convert dd/mm/yyyy date to yyyy/mm/dd
  #If no convertion function difftime bugs
  convertDateYMD <- function(date){
    if(length(date) == 1){
      #date format is dd/mm/yyyy
      start <- as.character(date)
      
      days <- substr(start, 1, 2)
      months <- substr(start, 4, 5)
      years <- substr(start, 7, 10)
      
      #new format is yyyy/mm/dd
      new <- paste(years, months, days, collapse = "/")
      return(ymd(new))
    }
    else{
      #declare vector of strings
      vector <- character()
      for(d in date){
        #date format is dd/mm/yyyy
        start <- as.character(d)
        
        days <- substr(start, 1, 2)
        months <- substr(start, 4, 5)
        years <- substr(start, 7, 10)
        
        #new format is yyyy/mm/dd
        new <- paste(years, months, days, collapse = "/")
        vector <- c(vector, new)
      }
      #return and convert vector
      return(ymd(vector))
    }
    return(NULL)
  }
  
  #Output the number of cigarette smoked by the user
  output$numberSmokedPerDay <- renderValueBox({
    req(input$file1)
    #Filter data
    #By Name of the user
    #By Type of action because for instance when using Friend mode this is not the user who smoke
    data <- filter(logs, User == input$selectedUser & Type %in% c("Behaviour", "Ontime", "Cheat"))
    
    #Convert format dd/mm/yyyy to yyyy/mm/dd
    #Because function difftime work with format yyyy/mm/dd
    data$Time <- convertDateYMD(data$Time)
    #Order data by Time in order to get min date and max date
    data <- data[order(data$Time),]
    
    start <- data$Time[1]
    end <- data$Time[nrow(data)]
    number_day <- as.numeric(difftime(end, start, units = "days"))
    
    #number of cigarettes smoke per day using iBriquet
    if(number_day == 0) val <- nrow(data)
    else val <- round(nrow(data) / number_day, digits = 2)
    #val <- number_day != 0? round(nrow(data) / number_day, digits = 2) : nrow(data)
    valueBox(
      value = val, 
      subtitle = tags$p("Per days smoked cigarette using iBriquet", style = "font-weight: bold"), 
      icon = icon("list")
    )
  })
  
  #Retrieve user in surveyece and output how many cigarette smoked per day
  output$smokedPerDayFromSurvey <- renderValueBox({
    req(input$file1)
    req(input$file2)
    
    user <- filter(survey, Name == input$selectedUser)
    if(nrow(user)>0){
      info <- select(user, Gender, Age, `Family status`, `How much do you weigh? (kg)`, `What is your height? (cm)`, `How many cigarettes do you smoke per day`)
      valueBox(
        value = info$`How many cigarettes do you smoke per day`, 
        subtitle = tags$p("Per days smoked cigarette from survey", style = "font-weight: bold"), 
        icon = icon("list")
      )
    }
    else{
      valueBox(
        value = 0,
        subtitle = tags$p("This user didn't fulfill the survey", style = "font-weight: bold"), 
        icon = icon("list")
      )
    }
  })
  
  output$barplotPeriod <- renderPlot({
    data <- filter(logs, User == input$selectedUser)
    data <- select(data, Type, periodDay)
    data <- count(data, c("Type", "periodDay"))
    #print(data)
    ggplot(data, aes(x=Type, fill=periodDay, y = freq)) + geom_bar(stat="identity")
  })
  
}

shinyApp(ui, server)
