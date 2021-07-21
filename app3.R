
# App developed my Data Cube Solutions
# contactdatacube@gmail.com / molo.andrew@gmail.com
# Data is reproducible
# Link to the online dataset

library(easypackages)
libraries("shiny","shinydashboard","tidyverse","lubridate", "plotly","Rcpp")
theme_set(theme_minimal())
town <- unique(medicalPractitioners$Town)


ui <- fluidPage(
  dashboardPage(skin = "yellow",
                dashboardHeader(
                  title = "KMPDC Data Dashboard",
                  titleWidth = 250
                ),
                dashboardSidebar(
                  sidebarMenu(
                    sidebarSearchForm(textId = "Search", buttonId = "searchTown",
                                      label = "Search city/town")
                  )
                ),
                dashboardBody(
                  fluidRow(
                    tabBox(width = 12, height = NULL, selected = "Count of Practitioners",
                           tabPanel("Count of medical practitioners", plotlyOutput("practitioners_count")),
                           tabPanel("Number of qualifications per medical practitioners", plotlyOutput("qualifications_count")),
                           tabPanel("Top specialties of medical practitioners", plotlyOutput("specialityCount"))
                    )
                  )
                ) # End of dashboardBody()
  ) # End of dashboardPage()
  
)


server <- function(input, output) {
  
  # Make a data frame that changes when the user chooses something
  filtered_data = reactive({
    df = medicalPractitioners %>% 
      # filter(Town %in% toupper(input$Search)) %>%
      dplyr::filter(Town == toupper(input$Search)) %>%
      # group_by(`Year Range`) %>%
      # summarise(count = n())
    df
  })
  
  # Count of Medical Practitioners Plot - Based on filtered Data 
  filteredMP = reactive({
    df = filtered_data()
    df %>%
      group_by(`Year Range`) %>%
      summarise(count = n()) %>%
      ggplot(aes(`Year Range`, count)) +
      geom_line(aes(group = 1),color="#aa2b1d", size=1) + 
      geom_point(size=4, color="#28527a") + 
      labs(title = "Count of Medical Practitioners in Kenya",
           subtitle = "Data from 1978 to 2020",
           caption = "Source: medicalboard.co.ke",
           x="") +
      theme(
        plot.title = element_text(color = "#23689b", size = 20, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(color = "#161d6f", size = 13, face = "bold",hjust = 0.5),
        plot.caption = element_text(color = "#0f1123", size = 10, face = "italic"),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12)
      ) + 
      geom_label(aes(label=count),
                 nudge_x = 0.1,
                 nudge_y = 0.2,
                 size=5)
  })
  

  # Render Plots in server function
  output$practitioners_count <- renderPlot({  # render the count plot with an error if there is no data
    validate(
      need(nrow(filtered_data()) > 0, 'Please select a city')
    )
    filteredMP()
  })
 

  
}

# Run the application 
shinyApp(ui = ui, server = server)
