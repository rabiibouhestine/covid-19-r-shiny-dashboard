#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(CoordinateCleaner)
library(dplyr)
library(reactable)
library(highcharter)

data <- read.csv("https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv")


data <- data %>% mutate(Date = as.Date(Date),
                        Country = replace(Country, Country=="US", "United States"),
                        Active = Confirmed - Recovered - Deaths,
                        Mortality = Deaths / Confirmed)


coord <- countryref %>%
    filter(type=="country") %>%
    select(Country = name, Long = centroid.lon, Lat = centroid.lat, iso3 = iso3) %>%
    group_by(Country) %>%
    slice_head(n = 1)


data <- data %>%
    left_join(coord, by = "Country")


latest_data <- data %>%
    filter(Date==max(Date)) %>%
    arrange(desc(Confirmed))


indicator_choices <- list("Active cases" = "Active",
                          "Confirmed cases (cumulative)" = "Confirmed",
                          "Deaths (cumulative)" = "Deaths")




# Define UI for application that draws a histogram
ui <- tagList(

    useShinydashboard(),

    tags$style("@import url(https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css);"),

    navbarPage(

        theme = shinytheme("slate"),

        title = "Corona Virus Disease (COVID-19) Dashboard",

        tabPanel(

            title = "Global Summary",

            fluidPage(

                fluidRow(
                    column(10, offset = 1, align="center",
                           p("As of", strong(max(latest_data$Date)), ", this is the global situation of COVID-19")
                    )
                ),

                fluidRow(
                    column(10, offset = 1, align="center",
                           valueBoxOutput("confirmed"),
                           valueBoxOutput("recovered"),
                           valueBoxOutput("deaths")
                    )
                ),

                br(),

                fluidRow(
                    column(10, offset = 1, align="center",
                           reactableOutput("table")
                    )
                ),
                fluidRow(
                    column(10, offset = 1, align="center",
                           highchartOutput("world_map", width = "100%", height = "600px")
                    )
                )
            )
        ),

        tabPanel(

            title = "Daily indicators by country",

            sidebarLayout(

                sidebarPanel(

                    width = 3,

                    selectInput("select_country",
                                label = h3("Country"),
                                choices = sort(latest_data$Country),
                                selected = 1),

                    dateRangeInput("select_dates",
                                   label = h3("Date range"),
                                   start = min(data$Date),
                                   end = max(data$Date),
                                   min = min(data$Date),
                                   max = max(data$Date)),

                    radioButtons("select_indicator",
                                 label = h3("Indicator"),
                                 choices = indicator_choices,
                                 selected = "Active")
                ),

                mainPanel(

                    width = 9,
                    highchartOutput("line_chart")
                )

            )
        )
    ),

    tags$script(HTML("var header = $('.navbar > .container-fluid');
                       header.append('<div style=\"float:right;margin-top:15px;margin-left:10px;\"><a target=\"_blank\" href=\"https://github.com/rabiibouhestine/covid-19-r-shiny-dashboard\"><i class=\"fa fa-code\"></i> Source</a></div>');
                       console.log(header)"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$confirmed <- renderValueBox({
        valueBox(
            format(sum(latest_data$Confirmed),big.mark=","), strong(h4("Confirmed Cases")), icon = icon("meh-o"),
            color = "orange"
        )
    })

    output$recovered <- renderValueBox({
        valueBox(
            format(sum(latest_data$Recovered),big.mark=","), strong(h4("Recoveries")), icon = icon("smile-o"),
            color = "green"
        )
    })

    output$deaths <- renderValueBox({
        valueBox(
            format(sum(latest_data$Deaths),big.mark=","), strong(h4("Deahs")), icon = icon("frown-o"),
            color = "red"
        )
    })

    output$table <- renderReactable({

        reactable(latest_data,
                  searchable = TRUE,
                  defaultPageSize = 4,
                  columns = list(
                      Date = colDef(
                          show = FALSE
                      ),
                      Country = colDef(
                          width = 200
                      ),
                      Confirmed = colDef(
                          format = colFormat(
                              separators = TRUE
                          )
                      ),
                      Recovered = colDef(
                          format = colFormat(
                              separators = TRUE
                          )
                      ),
                      Deaths = colDef(
                          format = colFormat(
                              separators = TRUE
                          )
                      ),
                      Active = colDef(
                          format = colFormat(
                              separators = TRUE
                          )
                      ),
                      Mortality = colDef(
                          format = colFormat(
                              digits = 2,
                              percent = TRUE
                          )
                      ),
                      Long = colDef(
                          show = FALSE
                      ),
                      Lat = colDef(
                          show = FALSE
                      ),
                      iso3 = colDef(
                          show = FALSE
                      )
                  ),
                  theme = reactableTheme(
                      color = "hsl(233, 9%, 87%)",
                      backgroundColor = "hsl(233, 9%, 19%)",
                      borderColor = "hsl(233, 9%, 22%)",
                      stripedColor = "hsl(233, 12%, 22%)",
                      highlightColor = "hsl(233, 12%, 24%)",
                      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
                  )
        )

        })

    output$world_map <- renderHighchart({

        highchart(type = "map") %>%
            hc_add_series_map(map = worldgeojson, df = latest_data, value = "Confirmed", joinBy = "iso3") %>%
            hc_tooltip(useHTML=TRUE,
                       headerFormat='',
                       pointFormat = paste0('{point.Country}: {point.value} Confirmed Cases, as of ',max(latest_data$Date))) %>%
            hc_title(text = 'Global Confirmed Cases of COVID-19',
                     style = list(
                         color = "#c8c8c8"
                     ))
    })

    output$line_chart <- renderHighchart({

        line_chart_data <- data %>%
            filter(Country == input$select_country,
                   Date >= input$select_dates[1],
                   Date <= input$select_dates[2]) %>%
            mutate(Indicator = get(input$select_indicator))

        hchart(line_chart_data,
               name = names(indicator_choices)[indicator_choices == input$select_indicator],
               type = "line",
               hcaes(x = Date,
                     y = Indicator)
               ) %>%
            hc_title(text = paste0(names(indicator_choices)[indicator_choices == input$select_indicator],
                                   ", ", input$select_country, ", ", input$select_dates[1], " to ", input$select_dates[2]),
                     style = list(
                         color = "#c8c8c8"
                     ))
    })




}

# Run the application
shinyApp(ui = ui, server = server)
