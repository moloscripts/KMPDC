
# App developed my Data Cube Solutions
# contactdatacube@gmail.com / molo.andrew@gmail.com
# Data is reproducible
# Link to the online dataset
 
library(easypackages)
libraries("shiny","shinydashboard","tidyverse","lubridate", "plotly")
theme_set(theme_minimal())

town <- unique(medicalPractitioners$Town)

# Define UI for application that draws a histogram
ui <- fluidPage(
    dashboardPage(skin = "yellow",
        dashboardHeader(
            title = "KMPDC Medical Practitioners Analysis",
            titleWidth = 300
        ),
        dashboardSidebar(
            sidebarMenu(
                sidebarSearchForm(textId = "Search", buttonId = "searchTown",
                                  label = "Search Town")
            )
        ),
        dashboardBody(
            fluidRow(
                tabBox(width = 12, height = NULL, selected = "Count of Practitioners",
                       tabPanel("Count of Practitioners", plotlyOutput("practitioners_count")),
                       tabPanel("Number of Qualifications per Medical Practitioners", plotlyOutput("qualifications_count")),
                       tabPanel("Top Specialties of Medical Practitioners", plotlyOutput("specialityCount"))
                       )
            )
        ) # End of dashboardBody()
    ) # End of dashboardPage()

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Server Output - Count of Practitioners ####
    output$practitioners_count <- renderPlotly({
        medicalPractitioners %>%
            group_by(`Year Range`) %>%
            summarise(count = n()) %>%
            ggplot(aes(`Year Range`, count)) +
            geom_line(aes(group = 1),color="#aa2b1d", size=1) + 
            geom_point(size=4, color="#28527a") + 
            labs(
                 subtitle = "Data from 1978 to 2020",
                 caption = "Source: medicalboard.co.ke",
                 x="") +
            theme(
                # plot.title = element_text(color = "#23689b", size = 13, face = "bold",hjust = 0.5),
                plot.subtitle = element_text(color = "#161d6f", size = 9, face = "bold",hjust = 0.5),
                plot.caption = element_text(color = "#0f1123", size = 10, face = "italic"),
                axis.text.x = element_text(face = "bold", size = 8),
                axis.text.y = element_text(face = "bold", size = 8)
            ) + 
            geom_label(aes(label=count),
                       nudge_x = 0.1,
                       nudge_y = 0.2,
                       size=5)
    })
    
    # Server Output - Count of Qualifications ####
    output$qualifications_count <- renderPlotly({
        medicalPractitioners %>%
            group_by(`Year Range`, `Number of Qualifications`) %>%
            summarise(count = n()) %>%
            ggplot(aes(`Year Range`, count, group=`Number of Qualifications`)) +
            geom_line(aes(color=`Number of Qualifications`), size=1) + 
            geom_point(aes(color=`Number of Qualifications`), size=3) + 
            labs(
                 subtitle = "Data from 1978 to 2020",
                 caption = "Source:https://medicalboard.co.ke/DashBoard.php ",
                 x="") +
            theme(
                # plot.title = element_text(color = "#23689b", size = 20, face = "bold",hjust = 0.5),
                plot.subtitle = element_text(color = "#161d6f", size = 13, face = "bold",hjust = 0.5),
                plot.caption = element_text(color = "#0f1123", size = 10, face = "italic"),
                axis.text.x = element_text(face = "bold", size = 8),
                axis.text.y = element_text(face = "bold", size = 8),
                legend.title = element_blank(),
                legend.position = "top"
            ) + 
            geom_label(aes(label=count),
                       nudge_x = 0.15,
                       nudge_y = 4,
                       size=2)
    })
    
    # Server Output - Top Specialties ####
    output$specialityCount <- renderPlotly({
        medicalPractitioners  %>% 
            count(Specialty, sort = T) %>%
            filter(n>20) %>%
            ggplot(aes(reorder(Specialty, n), n)) + 
            geom_col(aes(fill=Specialty)) + 
            coord_flip() + 
            labs(
                 subtitle = "Data from 1978 to 2020",
                 caption = "Source:https://medicalboard.co.ke/DashBoard.php ",
                 x="", y="count") +
            theme(
                # plot.title = element_text(color = "#23689b", size = 20, face = "bold",hjust = 0.5),
                plot.subtitle = element_text(color = "#161d6f", size = 13, face = "bold",hjust = 0.5),
                plot.caption = element_text(color = "#0f1123", size = 10, face = "italic"),
                axis.text.x = element_text(face = "bold", size = 8),
                axis.text.y = element_text(face = "bold", size = 8),
                legend.title = element_blank(),
                legend.position = "none"
            ) + 
            geom_label(aes(label=n),
                       nudge_x = 0.15,
                       nudge_y = 0.9,
                       size=2)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
