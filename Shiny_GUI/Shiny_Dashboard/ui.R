# ==============================================================================
# Shiny Dashboard Sederhana (UI)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : shiny, shinydashboard
# Data    : faithful (dataset bawaan R)
# Jalankan: shiny::runApp("Shiny_GUI/Shiny_Dashboard")
# ==============================================================================

library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Gui R Syukron"),
  dashboardSidebar(
    sliderInput("bins", "Number of Breaks", 1, 100, 30),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("gauge"),
               startExpanded = TRUE,
               menuSubItem("Histogram", tabName = "dashboard"),
               menuSubItem("Dashboard Finance", tabName = "finance"),
               menuSubItem("Dashboard Sales", tabName = "sales")),
      menuItem("Detailed Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Raw Data", tabName = "rawdata", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Histogram lama erupsi", plotOutput("histogram")),
                box(title = "Ringkasan", verbatimTextOutput("ringkasan"))
              )),
      tabItem(tabName = "finance",
              h1("Finance Dashboard")),
      tabItem(tabName = "sales",
              h2("Sales Dashboard")),
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "Lama erupsi vs waktu tunggu", plotOutput("scatter"))
              )),
      tabItem(tabName = "rawdata",
              tableOutput("raw"))
    )
  )
)
