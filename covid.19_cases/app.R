#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
############################################
# The Requirement For this Project
#
#
##############################################

library(dplyr)
library(tidyr)
library(plotly)

##############################################
## Front-end
ui <- fluidPage(
        tags$style(
            type='text/css',
            ".selectize-input { font-family: Arial, sans-serif; } .selectize-dropdown { font-family: Arial, sans-serif; }"
        ),
        tags$style(HTML(
            "body { font-family: Arial, sans-serif; line-height: 1.1; }"
        )),

        titlePanel("Case History of the Coronavirus (COVID-19) in the US"),
        fluidRow(
            column(
                3,
                selectizeInput("state", label=h5("States"), choices=NULL, width="100%")
            ),
            column(
              3,
              selectizeInput("county", label = h5("County"),choices=NULL, width="100%")
            ),
            column(
                3,
                checkboxGroupInput(
                    "metrics", label=h5("Please Chose a Metrics"),
                    choices=c("Confirmed", "Deaths"),
                    selected=c("Confirmed", "Deaths"), width="100%")
            )
        ),
        fluidRow(
            column(
                9,
                plotlyOutput("dailyMetrics")
            ),
            column(
                3,
                dateRangeInput("Calander", "Calander", start = "2020-01-22", end = Sys.Date())
            )
        ),
        fluidRow(
            column(
                9,
                plotlyOutput("cumulatedMetrics")
            ),
        ),

)

###################################################

## Back-End

baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-20/master/data_tables/JHU_USCountymap/df_Counties2020.csv"

fontstyle = list(family="Arial, sans-serif", size=12, color="rgb(30,30,30)")

alldata <- read.csv(baseURL,check.names = FALSE,stringsAsFactors = FALSE)

mydates <- as.Date(c("2020-12-25", "2020-09-12", "2020-05-23"))

IMP <- read.csv("Covid19-imp.csv", check.names = FALSE, stringsAsFactors = FALSE)
IMPdate = {}

alldata$IncidenceRate <- NULL
alldata$ST_ID <-NULL
alldata$NewCases <-NULL

colnames(alldata) <- c(" ", "county", "state", "FIPS", "date","CumConfirmed", "CumDeaths", "population")

# Define server logic required to draw a bar chart
server <- function(input, output, session) {
        data = reactive({
            d = alldata %>%
                filter(state == input$state) %>%
                filter(date >= input$Calander[1] & date <= input$Calander[2])
            if(input$county != "<all>") {
                d = d %>%
                    filter(county == input$county)
            } else {
                d = d %>%
                    group_by(date) %>%
                    summarise_if(is.numeric, sum, na.rm=TRUE)
            }
            d %>%
                mutate(
                    dateStr = format(date, format="%b %d, %Y"),
                    NewConfirmed = CumConfirmed - lag(CumConfirmed, default = 0),
                    NewDeaths = CumDeaths - lag(CumDeaths, default=0)
                )


        })


        observeEvent(input$state, {
            county = alldata %>%
                filter(state == input$state) %>%
                pull(county)
            county = c("<all>", sort(unique(county)))
            updateSelectInput(session, "county", choices=county, selected=county[1])
        })

        state = sort(unique(alldata$state))

        updateSelectInput(session, "state", choices=state, selected="Texas")

       # observeEvent(input$state, {
         #   IMPdate = subset(IMP, IMP$States == input$state)
      #  })

        renderBarPlot = function(varPrefix, legendPrefix, yaxisTitle) {
            renderPlotly({
                data = data()
                plt = data %>%
                    plot_ly() %>%
                    config(displayModeBar=FALSE) %>%
                    layout(
                        barmode='group',
                        xaxis=list(
                            title="", tickangle=-90, type='category', ticktext=as.list(data$date),
                            gridwidth=1),
                        yaxis=list(
                            title=yaxisTitle
                        ),
                        legend=list(x=0.05, y=0.95, font=list(size=15), bgcolor='rgba(240,240,240,0.5)'),
                        font=fontstyle
                    )


                for(metric in input$metrics)
                    plt = plt %>%
                    add_trace(
                        x= ~date, y=data[[paste0(varPrefix, metric)]], type='bar',
                        name=paste(legendPrefix, metric, "Cases"),
                        marker=list(
                            color=switch(metric, Deaths='rgb(0,128,0)', Confirmed='rgb(100,140,240)'),
                            # color=switch(date == 2020-12-25, color="rgb(255, 0, 0)", color="rgb(100, 140, 240)"),

                            line=list(color='rgb(8,48,107)', width=1.0)
                        )
                    )
                plt
            })
        }

        output$dailyMetrics = renderBarPlot("New", legendPrefix="New", yaxisTitle="New Cases per Day")
        output$cumulatedMetrics = renderBarPlot("Cum", legendPrefix="Cumulated", yaxisTitle="Cumulated Cases")
    }


# Run the application
shinyApp(ui = ui, server = server)
