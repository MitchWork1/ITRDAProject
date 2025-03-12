library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Survey Analysis"),
  dashboardSidebar(
    div(style = "text-align: center;", h4("Global Graph Settings")),
    selectInput("axisSize", "Axis Size", choices = seq(1, 18, by = 1),
                selected = 10, width = "100%"),
    selectInput("legendSize", "Legend Size", choices = seq(1, 20, by = 1),
                selected = 4, width = "100%"),
    selectInput("percentageSize", "Percentage Size",
                choices = seq(1, 15, by = 1), selected = 4, width = "100%"),
    selectInput("roundNum", "Rounding Digits", choices = seq(1, 3, by = 1),
                selected = 1, width = "100%"),
    checkboxInput("includeOther", "Include *Other", value = TRUE,
                  width = "100%")
  ),
  dashboardBody(
    navbarPage(
      title = "Graph Selection",
      
      # Programming Languages
      tabPanel("Programming Languages",
               fluidPage(
                 plotOutput("langPlot", height = "70vh"),
                 fluidRow(
                   column(12, sliderInput("numLang", "Top amount for Languages",
                                          min = 2, max = 55, value = 10,
                                          width = "100%"))))),
      # Databases
      tabPanel("Databases",
               fluidPage(
                 plotOutput("databasesPlot", height = "70vh"),
                 fluidRow(
                   column(12, sliderInput("numDb", "Top amount for Databases",
                                          min = 2, max = 35, value = 10,
                                          width = "100%"))))),
      # AI Search
      tabPanel("AI Search",
               fluidPage(
                 plotOutput("aiSearchPlot", height = "70vh"),
                 fluidRow(
                   column(12, sliderInput("numAISearch",
                                          "Top amount for AI Searches",
                                          min = 2, max = 15, value = 10,
                                          width = "100%"))))),
      # AI Used For
      tabPanel("AI Used For",
               fluidPage(
                 plotOutput("aiUsedForPlot", height = "70vh"),
                 fluidRow(
                   column(12, sliderInput("numAIUsedFor",
                                          "Top amount for AI Used For",
                                          min = 2, max = 10, value = 10,
                                          width = "100%"))))),
      # Platforms
      tabPanel("Platforms",
               fluidPage(
                 plotOutput("platformsPlot", height = "70vh"),
                 fluidRow(
                   column(12, sliderInput("numPlatform",
                                          "Top amount for Platforms",
                                          min = 2, max = 25, value = 10,
                                          width = "100%"))))),
      # Web Frameworks
      tabPanel("Web Frameworks",
               fluidPage(
                 plotOutput("webFrameworksPlot", height = "70vh"),
                 fluidRow(
                   column(12, sliderInput("numWebFrameworks",
                                          "Top amount for Web Frameworks",
                                          min = 2, max = 40, value = 10,
                                          width = "100%"))))),
      # Industries
      tabPanel("Industries",
               fluidPage(
                 plotOutput("industriesPlot", height = "70vh"),
                 fluidRow(
                   column(12, sliderInput("numIndustries",
                                          "Top amount for Industries", min = 2,
                                          max = 20, value = 15,
                                          width = "100%"))))),
      # Job Roles
      tabPanel("Job Roles",
               fluidPage(
                 plotOutput("jobRolesPlot", height = "70vh"),
                 fluidRow(
                   column(12, sliderInput("numJobRoles",
                                          "Top amount for Job Roles", min = 2,
                                          max = 45, value = 20,
                                          width = "100%"))))),
      # Job Roles
      tabPanel("Empolyment",
               fluidPage(
                 plotOutput("employmentPlot", height = "70vh")))
    )
  )
)