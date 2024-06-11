#V.0.4 of visual data app for USGS
#Created by Tayan Benson
#Devlog 6/10/24

library(shiny)
library(shinydashboard)
library(shinyBS)
library(ggplot2)

ui <- dashboardPage(skin = "black",
                    
  dashboardHeader(title = "USGS Plant Matrix"),
  dashboardSidebar(tags$head(tags$style(HTML('
          .progress-bar {
            background-color: darkolivegreen;
          }
          input[type=checkbox] {
            accent-color: darkolivegreen;
          }
          .skin-black .main-header .logo {
            background-color: darkolivegreen;
            font-weight: bold;
            color: white;
          }
          .skin-black .main-header .logo:hover {
            background-color: darkolivegreen;
          }
                                             '))),
     
                                 
    fileInput("file", "Choose CSV File",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")),
    
    checkboxInput("header", "Header", TRUE),
    
    
    sidebarMenu(
      id = "tabs",
      menuItem("Graph 1", tabName = "graph1", icon = icon("chart-line")),
      menuItem("Graph 2", tabName = "graph2", icon = icon("chart-line")),
      menuItem("Graph 3", tabName = "graph3", icon = icon("chart-line")),
      menuItem("Graph 4", tabName = "graph4", icon = icon("chart-line"))
    ),
    uiOutput("sidebar_ui")
  ),
  
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "graph1",
              fluidRow(
                column(6, box(width = 12, plotOutput("plot1"))),
                column(6, box(width = 12, plotOutput("plot2")))
              ),
              fluidRow(
                column(6, box(width = 12, plotOutput("plot3"))),
                column(6, box(width = 12, plotOutput("plot4")))
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 75 * 1024^2)
  
  
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, dec = ".", sep = ",", fileEncoding = "ISO-8859-1", header = input$header)
    df
  })
  
  
  renderSidebarUI <- function(tab_name, plot_index) {
    df <- data()
    colnames <- names(df)
    
    tagList(
      selectInput(paste0("facet", plot_index), "Facet Filter", choices = c("none", "Wetland"), selected = isolate(input[[paste0("facet", plot_index)]])),
      selectInput(paste0("plotselect", plot_index), "Plot Type", choices = c("Geopoint", "Boxplot"), selected = isolate(input[[paste0("plotselect", plot_index)]])),
      selectInput(paste0("xcol", plot_index), "X Column", choices = colnames, selected = isolate(input[[paste0("xcol", plot_index)]])),
      selectInput(paste0("ycol", plot_index), "Y Column", choices = colnames, selected = isolate(input[[paste0("ycol", plot_index)]]))
    )
  }
  
  
  output$sidebar_ui <- renderUI({
    req(data())
    if (input$tabs == "graph1") renderSidebarUI("graph1", 1)
    else if (input$tabs == "graph2") renderSidebarUI("graph2", 2)
    else if (input$tabs == "graph3") renderSidebarUI("graph3", 3)
    else if (input$tabs == "graph4") renderSidebarUI("graph4", 4)
  })
  
  
  renderPlotOutput <- function(plot_index) {
    renderPlot({
      req(data(), input[[paste0("xcol", plot_index)]], input[[paste0("ycol", plot_index)]])
      df <- data()
      plot_type <- input[[paste0("plotselect", plot_index)]]
      facet_type <- input[[paste0("facet", plot_index)]]
      xcol <- input[[paste0("xcol", plot_index)]]
      ycol <- input[[paste0("ycol", plot_index)]]
      
      p <- ggplot(df, aes_string(x = xcol, y = ycol))
      
      if (plot_type == "Geopoint") {
        p <- p + geom_point() + theme_bw()
      } else if (plot_type == "Boxplot") {
        p <- p + geom_boxplot() + theme_bw()
      }
      
      
      if (facet_type == "Wetland") {
        p <- p + facet_grid(~Wetland)
      }
      
      
      p + xlab(xcol) + ylab(ycol) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
  }
  
  
  output$plot1 <- renderPlotOutput(1)
  output$plot2 <- renderPlotOutput(2)
  output$plot3 <- renderPlotOutput(3)
  output$plot4 <- renderPlotOutput(4)
  
}

shinyApp(ui, server)



