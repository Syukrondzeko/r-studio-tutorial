# ==============================================================================
# Shiny GUI: Upload Data CSV dan Regresi Linier (UI)
# ------------------------------------------------------------------------------
# Channel : https://www.youtube.com/c/ManusiaSetengahChiKuadrat
# Paket   : shiny, shinythemes
# Jalankan: shiny::runApp("Shiny_GUI/Gui_Regresi")
#           Contoh file untuk diupload: data/mall_customers.csv
# ==============================================================================

library(shiny)
library(shinythemes)

fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    "GUI Regresi",
    tabPanel(
      "Data",
      sidebarLayout(
        sidebarPanel(
          # Input: pilih file ----
          fileInput("file1", "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          tags$hr(),

          # Input: apakah file memiliki header ----
          checkboxInput("header", "Header", TRUE),

          # Input: pemisah kolom ----
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                       selected = ","),

          # Input: tanda kutip ----
          radioButtons("quote", "Quote",
                       choices = c(None = "", "Double Quote" = '"',
                                   "Single Quote" = "'"),
                       selected = '"'),
          tags$hr(),

          # Input: jumlah baris yang ditampilkan ----
          radioButtons("disp", "Display",
                       choices = c(Head = "head", All = "all"),
                       selected = "head")
        ),
        mainPanel(
          tableOutput("contents")
        )
      )
    ),
    tabPanel(
      "Summary",
      verbatimTextOutput("summ")
    ),
    tabPanel(
      "Regresi",
      sidebarLayout(
        sidebarPanel(
          selectInput("y_var", "Variabel respon (Y)", choices = NULL),
          selectInput("x_var", "Variabel prediktor (X)", choices = NULL,
                      multiple = TRUE)
        ),
        mainPanel(
          verbatimTextOutput("model"),
          plotOutput("diagnostik")
        )
      )
    )
  )
)
