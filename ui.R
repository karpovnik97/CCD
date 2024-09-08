#libraries

library(shiny)
library(shinydashboard)

source("data.R")

### Building user interface 

current_year <- as.numeric(format(Sys.Date(), "%Y"))

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Climate Change Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("th", lib = "glyphicon")),
      menuItem("Threatened species", icon = icon("otter"),
               menuSubItem("EU", tabName = "EU-TS", icon = icon("earth-europe")),
               menuSubItem("USA", tabName = "USA-TS", icon = icon("flag-usa"))
      ),
      menuItem("Change in Sea Levels", tabName = "CSL", icon = icon("water")),
      menuItem("Disasters", tabName = "D", icon = icon("fire")),
      menuItem("Air Pollution", tabName = "AP", icon = icon("smog")),
      menuItem("Forest Area", tabName = "FA", icon = icon("tree")),
      menuItem("Temperature Change", tabName = "TC", icon = icon("sun")),
      menuItem("CO2 Concentrations", tabName = "CC", icon = icon("cloud")),
      uiOutput("sidebarinput"),  # This item will change depending on the tab selected
      sliderInput("AllDate", sep = "", label = "Date Range", min = 1960, max = current_year, step = 5, value = c(1960, current_year))
    )
  ),
  dashboardBody(
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
      ),
      tabItems(
        tabItem(tabName = "home",  # Homepage will show information about the dashboard
                fluidRow(
                  valueBox(
                    format(info2, big.mark = ","), "Variables Analyzed",
                    icon = icon("rectangle-list"), color = "blue"
                  ),
                  valueBox(
                    format(info1, big.mark = ","), "Observations Analyzed",
                    icon = icon("magnifying-glass"), color = "green"
                  ),
                  valueBox(
                    format(info3, big.mark = ","), "Values Analyzed",
                    icon = icon("table-cells"), color = "yellow"
                  ),
                  valueBox(9, "Datasets Used", color = "black", icon = icon("database")),
                  valueBox(21, "Charts Plotted", icon = icon("chart-column"))
                ),
                fluidRow(
                  # Research Topic
                  box(
                    title = "Research Topic", width = 12, status = "success", solidHeader = TRUE, background = "black",
                    h3("The dashboard topic is “Climate Change”. It aims to analyze data about different 
                 ecological and environmental factors, including list of threatened species, mean sea 
                 levels, frequency of natural disasters, forest covers, temperature change, etc. The 
                 data will be graphically depicted in order to identify different trends, contrast 
                 their significance in different countries / regions and conclude what overall trends 
                 can we observe around us – Is climate change real? And if so, how severe is it?")
                  ),
                  # Research Question  
                  box(
                    title = "The list of Research Questions:", background = "black", status = "success", solidHeader = TRUE, width = 12,
                    h4(icon("otter"), "What factors endanger species and their habitats? Is climate change a main factor? How many species are Endangered?"),
                    h4(icon("water"), "Has mean sea levels increased over time?", "To what extent are sea levels rising in different regions?"),
                    h4(icon("fire"), "Are climate related disasters more common nowadays?", "How disasters are distributed in different countries?", "Which countries have most climate-related catastrophes?", "What climate-related disasters are most common?"),
                    h4(icon("smog"), "Is air pollution getting worse?", "IN which countries is pollution the most severe?"),
                    h4(icon("tree"), "Has forest cover decreased on all continents? What’s the overall trend?"),
                    h4(icon("sun"), "How has global/local average temperature changed over time?"),
                    h4(icon("cloud"), "Is CO2 concentration increasing globally?", "What is trend in superpowers?")
                  )
                )
        ),
        tabItem(tabName = "EU-TS",
                fluidRow(
                  tabBox(title = "Threatened species", width = 12,
                         tabPanel("EU Data",
                                  plotlyOutput(outputId = "Species")
                         ),
                         tabPanel("Countries",
                                  plotlyOutput(outputId = "Species2", height = "600px")
                         )
                  )
                )
        ),
        tabItem(tabName = "USA-TS",
                fluidRow(
                  tabBox(title = "Species in USA", width = 12,
                         tabPanel("USA Data",
                                  plotlyOutput(outputId = "USA_TS1", height = "500px")
                         ),
                         tabPanel("States",
                                  selectInput(
                                    inputId = "USATS",
                                    label = "Select State:",
                                    choices = unique(Species_USA$`State Name`),
                                    selected = c("Georgia", "California", "Kentucky", "Ohio", "Virginia", "Idaho"),
                                    multiple = TRUE
                                  ),
                                  plotlyOutput("USA_TS2")
                         ),
                         tabPanel("Date",
                                  radioButtons("USA_TS_Date", "Choose Option to fill:",
                                               choices = list(
                                                 "Species Group" = 1,
                                                 "ESA Listing Status" = 2,
                                                 "State" = 3
                                               ),
                                               selected = 1
                                  ),
                                  plotlyOutput("USA_TS3")
                         )
                  )
                )
        ),
        tabItem(tabName = "CSL",
                fluidRow(
                  tabBox(title = "Change in Sea Levels", width = 12,
                         tabPanel("Main",
                                  selectInput(
                                    inputId = "Sea_Inp",
                                    label = "Select Region:",
                                    choices = unique(Sea_Levels$Measure),
                                    multiple = TRUE,
                                    selected = "Yellow Sea"
                                  ),
                                  radioButtons("SeaA", "Choose Option:",
                                               choices = list(
                                                 "Line Chart" = 1,
                                                 "Smoothed Line" = 2,
                                                 "Line Chart + Smoothed Line" = 3,
                                                 "Bar Chart" = 4
                                               ),
                                               selected = 1
                                  ),
                                  plotlyOutput(outputId = "Sea_Level")
                         ),
                         tabPanel("Box Plot",
                                  sliderInput(
                                    inputId = "Sea_Slider",
                                    label = "Select Number of Regions:",
                                    min = 1,
                                    max = 50,
                                    value = 5,
                                    sep = "",
                                    step = 1
                                  ),
                                  plotlyOutput(outputId = "Sea_Level2")
                         )
                  )
                )
        ),
        tabItem(tabName = "D",
                fluidRow(
                  tabBox(title = "Disasters", width = 12,
                         tabPanel("Main",
                                  plotlyOutput(outputId = "DF", height = "500px")
                         ),
                         tabPanel("Countries",
                                  selectInput(
                                    inputId = "DF_C",
                                    label = "Select Country:",
                                    choices = unique(Disasters$Country),
                                    selected = c("United States", "Philippines", "India"),
                                    multiple = TRUE
                                  ),
                                  radioButtons("DisC", "Choose Option:",
                                               choices = list("Bar Chart" = 1, "Box Plot" = 2),
                                               selected = 1
                                  ),
                                  plotlyOutput(outputId = "DF2"),
                                  selectInput(
                                    inputId = "DF_C1",
                                    label = "Select Country:",
                                    choices = unique(Disasters$Country),
                                    selected = "United States",
                                    multiple = FALSE
                                  ),
                                  plotlyOutput(outputId = "DF1.1")
                         ),
                         tabPanel("Tables",
                                  h3("Which disaster occurs most frequently?"),
                                  dataTableOutput('DisastersTable2'),
                                  h3("Countries with the most disasters recorded"),
                                  dataTableOutput('DisastersTable')
                         )
                  )
                )
        ),
        tabItem(tabName = "AP",
                fluidRow(
                  box(title = "Air Pollution", width = 12, status = "success", solidHeader = TRUE,
                      selectInput(
                        inputId = "AP_C",
                        label = "Select Countries:",
                        choices = unique(Air_Pollution$Country),
                        selected = c("United States", "China", "South Sudan", "Georgia"),
                        multiple = TRUE
                      ),
                      plotlyOutput("Air", height = "500px"),
                      valueBoxOutput("AP_Highest"),
                      valueBoxOutput("AP_Lowest")
                  ),
                  box(width = 12, status = "success",
                      numericInput(
                        width = 250,
                        inputId = "APslider",
                        label = "Select Number of Regions to Display:",
                        min = 1,
                        max = 209,
                        value = 3,
                        step = 1
                      ),
                      plotlyOutput("Air2")
                  )
                )
        ),
        tabItem(tabName = "FA",
                fluidRow(
                  tabBox(title = "Forest Area", width = 12,
                         tabPanel("World Trend",
                                  plotlyOutput(outputId = "Forest_Area", height = "500px")
                         ),
                         tabPanel("By Country",
                                  selectInput(
                                    inputId = "FC_C2",
                                    label = "Select Country:",
                                    choices = unique(Forest_Area$Country),
                                    selected = c("Africa","Asia","Euro Area","Americas","Austra"),
                                    multiple = TRUE
                                  ),
                                  plotlyOutput(outputId = "Forest_Area2", height = "500px")
                         )
                  )
                )
        ),
        tabItem(tabName = "TC",
                fluidRow(
                  tabBox(width = 12,
                         title = "Temperature Change",
                         tabPanel(
                           title = "Global Trend",
                           plotOutput("Temperature_Change")
                         ),
                         tabPanel(
                           title = "Local Trend",
                           selectInput(
                             inputId = "TCC",
                             label = "Select Countries:",
                             choices = unique(Temperature_Change$Country),
                             selected = "Georgia",
                             multiple = TRUE
                           ),
                           plotlyOutput("Temperature_Change2")
                         )
                  ),
                  box(width = 6,
                      valueBoxOutput("TC_top_1st"),
                      valueBoxOutput("TC_bot_1st")
                  )
                )
        ),
        tabItem(tabName = "CC",
                fluidRow(
                  tabBox(title = "CO2 Concentrations", width = 12,
                         tabPanel("World Trend",
                                  plotlyOutput(outputId = "CO2_Concentrations")
                         ),
                         tabPanel("By Country",
                                  selectInput(
                                    inputId = "CO2countries",
                                    label = "Select a Countries:",
                                    choices = unique(CO2_Emissions$`Country Name`),
                                    multiple = TRUE,
                                    selected = c("United States","Russian Federation","China")
                                  ),
                                  plotlyOutput(outputId = "CO2_Emissions")
                         )
                  )
                )
        )
      )
    )
  )
)
