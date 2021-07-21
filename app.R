
# App developed my Data Cube Solutions
# contactdatacube@gmail.com / molo.andrew@gmail.com
# Data is reproducible
# https://stackoverflow.com/questions/54914541/global-r-dont-start/66802176#66802176
 
library(easypackages)
libraries("shiny","shinydashboard","tidyverse","lubridate", "plotly","Rcpp","shinyjs","rsconnect")
# theme_set(theme_minimal())

# Visuals ####

# Dataset
medicalPractitioners <- read.csv("Data/MedicalPractitioners.csv")
medicalPractitioners$RegDate <- dmy(medicalPractitioners$RegDate)
medicalPractitioners <- medicalPractitioners %>%
    select(RegDate, SPECIALTY, SUB_SPECIALTY, Qualification.Count, TOWN) %>%
    rename(
        `Registration Date` = RegDate,
        Specialty = SPECIALTY,
        `Sub Specialty` = SUB_SPECIALTY,
        `Number of Qualifications` = Qualification.Count,
        Town = TOWN) %>%
    dplyr::mutate(Year = lubridate::year(`Registration Date`))

medicalPractitioners$`Year Range` = cut(medicalPractitioners$Year, c(1970, 1980, 1990, 2000, 2010, 2020, 2025))
levels(medicalPractitioners$`Year Range`) = c("1970-1980", "1981-1990", "1991-2000","2001-2010", "2011-2020","2021")
medicalPractitioners$`Year Range` <- factor(medicalPractitioners$`Year Range`, ordered = T, levels = c("1970-1980", "1981-1990", "1991-2000","2001-2010", "2011-2020"))

# Function for converting to Factors
to_factor <- c("Specialty","Sub Specialty","Number of Qualifications","Town")
for (col in to_factor) {
    medicalPractitioners[[col]] <- as.factor(as.character(medicalPractitioners[[col]]))
}

## Plots ####

## Count of Medical Practitioners ####
medicalPractitioners %>%
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



####
MPQualificationsDF <- medicalPractitioners %>%
    group_by(`Year Range`, `Number of Qualifications`) %>%
    summarise(count = n()) %>%
    ggplot(aes(`Year Range`, count, group=`Number of Qualifications`)) +
    geom_line(aes(color=`Number of Qualifications`), size=1) + 
    geom_point(aes(color=`Number of Qualifications`), size=5) + 
    labs(title = "Count of Qualifications of Medical Practitioners",
         subtitle = "Data from 1978 to 2020",
         caption = "Source:https://medicalboard.co.ke/DashBoard.php ",
         x="") +
    theme(
        plot.title = element_text(color = "#23689b", size = 20, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(color = "#161d6f", size = 13, face = "bold",hjust = 0.5),
        plot.caption = element_text(color = "#0f1123", size = 10, face = "italic"),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        legend.title = element_blank(),
        legend.position = "top"
    ) + 
    geom_label(aes(label=count),
               nudge_x = 0.15,
               nudge_y = 4,
               size=4)


## Top Specialties in the last decade ####
SpecialtiesDF <- medicalPractitioners  %>% 
    count(Specialty, sort = T) %>%
    filter(n>20)

ggplot(SpecialtiesDF, aes(reorder(Specialty, n), n)) + 
    geom_col(aes(fill=Specialty)) + 
    coord_flip() + 
    labs(title = "Top Specialties of Medical Practitioners",
         subtitle = "Data from 1978 to 2020",
         caption = "Source:https://medicalboard.co.ke/DashBoard.php ",
         x="", y="count") +
    theme(
        plot.title = element_text(color = "#23689b", size = 20, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(color = "#161d6f", size = 13, face = "bold",hjust = 0.5),
        plot.caption = element_text(color = "#0f1123", size = 10, face = "italic"),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        legend.title = element_blank(),
        legend.position = "none"
    ) + 
    geom_label(aes(label=n),
               nudge_x = 0.15,
               nudge_y = 0.9,
               size=4)

# Shiny Dashboard Framework ####
town <- unique(medicalPractitioners$Town)

# Define UI for application that draws a histogram
ui <- fluidPage(
    dashboardPage(skin = "yellow",
        dashboardHeader(
            title = "KMPDC Data Dashboard",
            titleWidth = 250
        ),
        dashboardSidebar(
            sidebarMenu(
                sidebarSearchForm(textId = "Search", buttonId = "searchTown",
                                  label = "Search Town")
            )
        ),
        dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
            ),
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


server <- function(input, output) {
    
    filtered_data <- reactive({
        medicalPractitioners %>%
            filter(Town %in% toupper(input$Search))
    })
    

    # Server Output - Count of Practitioners ####
    output$practitioners_count <- renderPlotly({
        filter(medicalPractitioners, Town==toupper(input$Search)) %>%
            group_by(`Year Range`) %>%
            summarise(count = n()) %>%
            ggplot(aes(`Year Range`, count)) +
            geom_line(aes(group = 1),color="#E9896A", size=1) + 
            geom_point(size=4, color="#00bfc4") + 
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
                       size=5) +
            theme_minimal()
    })
    
    # Server Output - Count of Qualifications ####
    output$qualifications_count <- renderPlotly({
        filter(medicalPractitioners, Town==toupper(input$Search)) %>%
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
                       size=2) + 
            theme_minimal()
    })
    
    # Server Output - Top Specialties ####
    output$specialityCount <- renderPlotly({
        filter(medicalPractitioners, Town==toupper(input$Search)) %>%
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
                       size=2) + 
            theme_minimal()
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
