
# App developed my Data Cube Solutions
# contactdatacube@gmail.com / molo.andrew@gmail.com
# Data is reproducible
# Link to the online dataset
 
library(easypackages)
libraries("shiny","shinydashboard","tidyverse","lubridate", "plotly")

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


set.seed(1234)


# Define UI for application that draws a histogram
ui <- fluidPage(
    dashboardPage(skin = "yellow",
        dashboardHeader(
            title = "KMPDC Medical Practitioners Analysis",
            titleWidth = 300
        ),
        dashboardSidebar(
            sidebarMenu(
                sidebarSearchForm(textId = "SearchTown", buttonId = "searchButton",
                                  label = "Search Town")
            )
        ),
        dashboardBody(
            fluidRow(
                tabBox(width = 12, height = NULL, selected = "Count of Practitioners",
                       tabPanel("Count of Practitioners", plotlyOutput("practitioners_count")),
                       tabPanel("Count of Qualifications", plotlyOutput("qualifications_count")),
                       tabPanel("Specialty of Practitioners", plotlyOutput("specialityCount")),
                       tabPanel("Sub Speciality of Practitioners", plotlyOutput("subSpecialtyCount"))
                       )
            )
        ) # End of dashboardBody()
    ) # End of dashboardPage()

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
