#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(highcharter)
source("./lib.r")

# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
    title = "GCDS",
    
    # Application title
    header = dashboardHeaderPlus(title = "Australian COVID19 Status"),
    
    # Sidebar with a slider input for number of bins 
    sidebar = dashboardSidebar(
       checkboxInput(inputId = "log", "Log Scale", value = FALSE),
       checkboxGroupInput(inputId = "States", label = "States")
    ),
    
    body = dashboardBody(
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                column(8, highchartOutput("trend")),
                column(4, highchartOutput("mort_rate"))
            ),
            fluidRow(
                column(12, highchartOutput("states"))
            )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # get data from wikipedia
    url <- "https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Australia"
    page <- read_html(url)
    
    tables <- get_stats(page)
    
    raw_trend_data <- tables$stats %>% 
                         mutate(Not.Recovered = Confirmed - Recovered - Deaths,
                         Death.Rate = round(Deaths/Confirmed*100, 2)) 
    
    trend <- raw_trend_data %>%
                gather(key = Type, value = Number, Confirmed:Not.Recovered)
    states <- tables$new_cases %>% gather(key = State, value = Number, NSW:NT)
    
    updateCheckboxGroupInput(session = session, inputId = "States", choices = unique(states$State), selected = unique(states$State))
    
    states_data <- reactive({
        states %>% filter(State %in% input$States)
    })
    
    output$trend <- renderHighchart({
        conf_rec <- trend %>% filter(Type %in% c("Not.Recovered", "Recovered", "Deaths"))
        
        hc <- hchart(conf_rec, "column", hcaes(x = Date, y = Number, group = Type)) %>%
            hc_plotOptions(series=list(stacking='normal')) %>% 
            hc_add_theme(hc_theme_google()) %>%
            hc_colors(c("#f20505", "#04060f", "#138f19")) %>%
            hc_title(text = "Australia Trend",
                     margin = 20, align = "left",
                     style = list(color = "#04060f", useHTML = TRUE))
         
        if(input$log){
            hc %>% hc_yAxis(type = 'logarithmic') 
        } else {
            hc
        }
            
    })
    
    output$states <- renderHighchart({
        hc <- hchart(states_data(), "scatter", hcaes(x = Date, y = Number, group = State)) %>%
                hc_add_theme(hc_theme_google()) %>%
                hc_title(text = "New Cases By State",
                         margin = 20, align = "left",
                        style = list(color = "#04060f", useHTML = TRUE))
        
        if(input$log){
            hc %>% hc_yAxis(type = 'logarithmic') 
        } else {
            hc
        }
    })
    
    output$mort_rate <- renderHighchart({
        hchart(raw_trend_data, "line", hcaes(x = Date, y = Death.Rate, group = 1)) %>%
            hc_add_theme(hc_theme_google()) %>%
            hc_title(text = "Mortality Rate",
                     margin = 20, align = "left",
                     style = list(color = "#04060f", useHTML = TRUE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
