library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(shinydashboard)

# Load data (file must be in same folder)
df <- read_excel("fintemp.xlsx")

# UI
ui <- dashboardPage(
  
  dashboardHeader(title = "HDI Dashboard"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Life Expectancy", tabName = "life", icon = icon("heartbeat")),
      menuItem("GNI", tabName = "gni", icon = icon("dollar-sign")),
      menuItem("Education", tabName = "edu", icon = icon("book")),
      menuItem("HDI Rank", tabName = "hdi", icon = icon("chart-line"))
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # Introduction
      tabItem(
        tabName = "intro",
        
        h2("Human Development Indicators Dashboard"),
        
        p("This dashboard visualizes key development indicators across countries."),
        
        tags$ul(
          tags$li("Life Expectancy"),
          tags$li("Gross National Income (GNI)"),
          tags$li("Education Index"),
          tags$li("HDI Rank")
        ),
        
        p("Use the sidebar to navigate between plots.")
      ),
      
      # Life Expectancy
      tabItem(
        tabName = "life",
        
        selectInput(
          "country1",
          "Select Country:",
          choices = unique(df$Country)
        ),
        
        plotOutput("lifePlot")
      ),
      
      # GNI
      tabItem(
        tabName = "gni",
        
        selectInput(
          "country2",
          "Select Country:",
          choices = unique(df$Country)
        ),
        
        plotOutput("gniPlot")
      ),
      
      # Education
      tabItem(
        tabName = "edu",
        
        selectInput(
          "country3",
          "Select Country:",
          choices = unique(df$Country)
        ),
        
        plotOutput("eduPlot")
      ),
      
      # HDI Rank
      tabItem(
        tabName = "hdi",
        
        selectInput(
          "country4",
          "Select Country:",
          choices = unique(df$Country)
        ),
        
        plotOutput("hdiPlot")
      )
      
    )
  )
)

# Server
server <- function(input, output) {
  
  output$lifePlot <- renderPlot({
    
    ggplot(
      df %>% filter(Country == input$country1),
      aes(x = Year, y = Life_Expectancy)
    ) +
      geom_line() +
      geom_point() +
      labs(title = "Life Expectancy Over Time") +
      theme_minimal()
    
  })
  
  output$gniPlot <- renderPlot({
    
    ggplot(
      df %>% filter(Country == input$country2),
      aes(x = Year, y = GNI)
    ) +
      geom_line() +
      geom_point() +
      labs(title = "GNI Over Time") +
      theme_minimal()
    
  })
  
  output$eduPlot <- renderPlot({
    
    ggplot(
      df %>% filter(Country == input$country3),
      aes(x = Year, y = Education)
    ) +
      geom_line() +
      geom_point() +
      labs(title = "Education Index Over Time") +
      theme_minimal()
    
  })
  
  output$hdiPlot <- renderPlot({
    
    ggplot(
      df %>% filter(Country == input$country4),
      aes(x = Year, y = HDI_Rank)
    ) +
      geom_line() +
      geom_point() +
      labs(title = "HDI Rank Over Time") +
      theme_minimal()
    
  })
  
}

# Run App
shinyApp(ui, server)












