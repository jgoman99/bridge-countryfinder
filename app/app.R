library(shiny)
library(wbstats)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
source("find_closest_country.R")


world_bank_indicators = read.csv("assets/world_bank_indicators_08_22.csv")
world_bank_countries = read.csv("assets/world_bank_countries.csv")
world_bank_top_indicators = read.csv("assets/world_bank_top_indicators.csv")$indicator

# Define UI for application that draws a histogram
ui <- navbarPage("CountryFinder",theme = shinytheme("superhero"),
  tabPanel("Main",useShinyjs(),
           sidebarPanel(
           selectInput("selectIndicator1",label="Select Indicator1",
                       choices = world_bank_top_indicators, selected = "Life expectancy at birth, total (years)"),
           selectInput("selectIndicator2",label="Select Indicator2",
                       choices = world_bank_top_indicators, selected = "Urban population (% of total population)"),
           selectInput("selectIndicator3",label="Select Indicator3",
                       choices = world_bank_top_indicators, selected = "GDP per capita (current US$)"),
           selectInput("selectCountry", label = "Select Country", choices = world_bank_countries$country, 
           selected = "United States"),
           selectInput("selectYear", label = "Select Year", choices = 2022:1960,selected=1994),
           actionButton("buttonFindCountry", label = "Find Closest Country!",
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
           ),
           mainPanel(
             fluidRow(
             h2("What Country is Most Similar?", align="left"),
             br()
             ),
             fluidRow(
             textOutput("closestCountryText") %>% withSpinner()  ,
             br()
             ),
             fluidRow(
             tableOutput("closestCountryTable"),
             br()
             ),
             textOutput("descriptionText1"),
             br(),
             textOutput("descriptionText2"),
             br(),
             textOutput("descriptionText3"),
             br(),
             textOutput("sourcesText")
           )
           
           ),
  navbarMenu("About",
             tabPanel("About the Page",
                      h2("About",align='center'),
                      p("I made this application on a lazy sunday to learn how to use Shiny with external APIs. Also, 
                        because I like comparing countries on an oddity of metrics. Hope you enjoy it!", align='center')),
             tabPanel("Data",
                      h2("Data", align = 'center'),
                      p("Data is provided by the World Bank using R's wbstats package. Indicators available are the
                         most popular indicators. They were scaped using rvest. Data was filled  using most recent years 
                        when available. Similarity scores were calculated by first normalizing, and then using euclidean
                        distance.", align='center'))
             )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
  # misc
  indicator_list <- NA
  indicator_id_list <- NA
  indicator_desc_list <- NA
  indicator_source_list <- NA
  
  output$closestCountryText = renderText({""})

  # populate select input
  observe({
    # Note, we will have to fix order here
    subset <- world_bank_indicators %>% 
      filter(indicator %in% c(input$selectIndicator1,input$selectIndicator2,input$selectIndicator3))
      
    indicator_list <<- subset$indicator
    indicator_id_list <<- subset$indicator_id
    indicator_desc_list <<- subset$indicator_desc
    indicator_source_list <<- subset$source
    

  })
  
  observeEvent(input$buttonFindCountry,
               {
                 toggle("closestCountryTable")
                 toggle("descriptionText1")
                 toggle("descriptionText2")
                 toggle("descriptionText3")
                 toggle("sourcesText")
                 output$closestCountryText = renderText({"Loading..."})


                 # inputs
                 country = input$selectCountry
                 country_year = input$selectYear
                 variable_list = indicator_id_list
                 valid_countries = world_bank_countries$country
                 print(variable_list)
                 results_df <- find_closest_country(country,country_year,variable_list,valid_countries)

                 
                 if (nrow(results_df)==2)
                 {
                   results_df <- results_df[,c('country',indicator_id_list)]
                   colnames(results_df) <- c("Country",indicator_list)
                   
                   output$closestCountryText = renderText({
                     paste0("Based on the selected indicators, the country most resembling ",country_year,
                            " ",country," is: ", results_df$Country[2])
                     
                   })
                   
                   output$closestCountryTable = renderTable({results_df})
                   output$descriptionText1 = renderText({paste0(indicator_desc_list[1])})
                   output$descriptionText2 = renderText({paste0(indicator_desc_list[2])})
                   output$descriptionText3 = renderText({paste0(indicator_desc_list[3])})
                   output$sourcesText = renderText({paste0("Sources: ",unique(indicator_source_list),collapse=", ")})
                   
                 }
                 else
                 {
                   output$closestCountryText = renderText({"Something went wrong. This usually happens due
                     to missing data. Try a country year closer to 2000, or more common indicators."})
                   output$closestCountryTable = renderTable({results_df})
                   output$descriptionText1 = renderText({""})
                   output$descriptionText2 = renderText({""})
                   output$descriptionText3 = renderText({""})
                   output$sourcesText = renderText({""})
                 }
                 toggle("closestCountryTable")
                 toggle("descriptionText1")
                 toggle("descriptionText2")
                 toggle("descriptionText3")
                 toggle("sourcesText")
               })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
