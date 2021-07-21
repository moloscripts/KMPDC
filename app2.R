library(tidyverse)
library(shiny)
library(shinydashboard)
library(lubridate)

theme_set(theme_minimal())

city <- c("NAIROBI", "MOMBASA", "MOMBASA","MOMBASA","NAIROBI")
SalesCount <- c(34, 50, 23, 70, 33)
Qualifications <- c("More than one", "One","One", "More than one","One")
Age <- c(45, 50, 43, 44, 60)
SalesDF <- data.frame(city, SalesCount, Qualifications, Age)
city <- SalesDF$city


cityCountplot <- ggplot(SalesDF, aes(city, SalesCount)) + geom_col()
cityCountplot

qualifyCountplot <- ggplot(SalesDF, aes(Qualifications, SalesCount)) + geom_col()
qualifyCountplot

ui <- fluidPage(
  titlePanel("Sales Data"),
  
  sidebarLayout(
    sidebarPanel(
      sidebarMenu(
        sidebarSearchForm(textId = "Search", buttonId = "searchTown", label = "Search Town")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("plot1", plotOutput("plot1")),
        tabPanel("plot2", plotOutput("plot2"))
      )
    )
  )
)

server <- function(input, output) {
  filteredSales = reactive({         # Make a data frame that changes when the user chooses something
    df = SalesDF %>% dplyr::filter(city == toupper(input$Search))
    df
  })
  
  filteredCityCountPlot = reactive({ # Make a count plot based on the filtered data frame
    df = filteredSales()
    ggplot(df, aes(city, SalesCount)) + geom_col()
  })
  
  filteredQualifyCountPlot = reactive({ # Make a qualification plot based on the filtered data frame
    df = filteredSales()
    ggplot(df, aes(Qualifications, SalesCount)) + geom_col()
  })
  
  output$plot1 <- renderPlot({  # render the count plot with an error if there is no data
    validate(
      need(nrow(filteredSales()) > 0, 'Please select a city')
    )
    filteredCityCountPlot()
  })
  
  output$plot2 <- renderPlot({  # render the qualification plot with an error if there is no data
    validate(
      need(nrow(filteredSales()) > 0, 'Please select a city')
    )
    filteredQualifyCountPlot()
  })
}

shinyApp(ui = ui, server = server)