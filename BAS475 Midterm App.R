#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(fpp3)
library(lubridate)
library(feasts)
library(plotly)
library(quantmod)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggeasy)
library(fable)

#This data is found on Google trends and tracks the interest of the Halo and Call of Duty franchises on a month-to-month basis. The time period that the data represents is from January 1, 2012 to March 26, 2022 (roughly 10 years). 
file_path <- "halo_vs_callofduty.csv"
#Loading in data
data <- read.csv(file_path)

#naming the columns
names(data) <- c("Month", "Halo", "CallofDuty")
# Convert Month to date
data$Month <- yearmonth(data$Month)
# Convert to tsibble
data <- tsibble(data)

# Google labels low numbers as "<1"
# Convert those to 0s and ensure the column is numbers
data$Halo <- as.numeric(
    ifelse(data$Halo == "<1", 0, data$Halo))
data$CallofDuty <- as.numeric(
    ifelse(data$CallofDuty == "<1", 0, data$CallofDuty))

#Original Time Series Graph w/ggplot
Int <- ggplot() + 
    geom_line(data=data, aes(x=Month, y = Halo), colour = "purple") +
    geom_line(data=data, aes(x=Month, y = CallofDuty), colour = "gold") +
    labs(x = "Month", y = "Search Interest", title = "Google Trends Search Interest") 

#Decomposition Graph for Halo and Call of Duty

gimme <- data %>% 
    select(Month, Halo, CallofDuty)

part <- gimme %>% 
    pivot_longer(-Month)

Decomp <- part %>% 
    model(classical_decomposition(value)) %>% 
    components() %>% 
    autoplot()

#Seasonal Models
season1 <- data %>%
    gg_season(Halo, labels = "both") +
    labs(y = "Interest",
         title = "Seasonal plot: Halo Interest")

season2 <- data %>%
    gg_season(CallofDuty, labels = "both") +
    labs(y = "Interest",
         title = "Seasonal plot: Call of Duty Interest")

#Autocorrelation Models
acf1 <- data %>%
    ACF(Halo, lag_max = 12) %>%
    autoplot() + labs(title="Halo Interest")

acf2 <- data %>%
    ACF(CallofDuty, lag_max = 12) %>%
    autoplot() + labs(title="Call of Duty Interest")

#Forecasting Models

fit1 <- part %>%
    model(trend_model = TSLM(value~trend()))

Haloforecast <- fit1 %>%
    forecast(h = "3 years") %>%
    filter(name == "Halo") %>%
    autoplot(part) +
    labs(y = "Search Interest", title = "Forecast for Halo")

CODforecast <- fit1 %>%
    forecast(h = "3 years") %>%
    filter(name == "CallofDuty") %>%
    autoplot(part) +
    labs(y = "Search Interest", title = "Forecast for Call of Duty")

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            textOutput("panel")
        ),
        mainPanel(
            tabsetPanel(
                id = "tabset",
                tabPanel("Instructions", "Hi! My name is Myles Terry 
                         and this is an app made to present multiple time series plots 
                         using data I found on the Internet. This data is found on Google 
                         trends and tracks the search interest of the Halo and Call of Duty 
                         franchises on a month-to-month basis. The time period that the 
                         data represents is from January 1, 2012 to March 26, 2022 (roughly 
                         10 years). The first graph shown is meant to be a representation
                         of the original data. There are 8 other plots to accompany it.
                         In order to change which plot you want to view, just click on the 
                         tabs near the top. Enjoy!"),
                tabPanel("Original Time Series Analysis", plotlyOutput ("original", height = 250), "This 
                        is the orginal data of the search interest on Google Trends for the Halo and
                         Call of Duty(COD) franchises. Halo shows up in purple and COD shows up in gold. 
                         Both Call of Duty and Halo have many peaks occuring in October and November.
                         These peaks happen when new games for the franchises are being released. Both 
                         of the franchises have slightly downward trends due to gamers feeling like 
                         the newer releases have less of the classic feel to them."),
                tabPanel("Decomposition Plot", plotOutput("decomp", height = 250), "This graph shows the 
                         additive decomposition of the components for the search interest. I used additive
                         decomposition since the seasonlity patterns don't get bigger over time. The 
                         components are split into trend-cyclic, seasonal, and random. The decomposition plot attempts 
                         tor root out the main cause for why the data may be shaped a certain way. The
                         small grey bar in the bottom panel suggest that most of the data in Google 
                         searches for Halo and Call of Duty are random. This makes sense since people often
                         buy games randomly. However, the decompositon does show that there is a good amount
                         of seasonality in the data. The seasonality comes from search interest 
                         spikes when a new game comes out and the trend goes slightly downwards. There is
                         no much trend in the data."),
                tabPanel("Seasonal Plot (Halo)", plotOutput("haloseason", height = 250), "This graph shows the 
                         seasonaity of Halo. A lot of the seasonality occurs in the later half of the year 
                         October, November, and December. New Halo games come out on October and November while
                         December is when people buy new games for Christmas when they are on sale. The highest
                         peak in the data is during November 2012 when Halo 4 came out. This was a very popular
                         game for the franchise and it shows."), 
                tabPanel("Seasonal Plot (Call of Duty)", plotOutput("CODseason", height = 250), "This graph shows the 
                         seasonaity of Halo. A lot of the seasonality occurs in the later half of the year 
                         October, November, and December. Like Halo, New Call of Duty games come out on October and 
                         November while December is when people buy new games for Christmas when they are on sale. 
                         The peaks for Call of Duty are higher because it has more fans than Halo. It also has more 
                         peaks throughout the years because its fans are more attached to the newer releases of the 
                         franchise. The highest peak in the data is during November 2013 when Call of Duty: Ghosts 
                         came out. Although I've heard not so great reviews about this game, it came out after Black 
                         Ops 2 which is a favorite for many fans. They were likely anticipating that Ghosts would
                         give them the same joy."),
                tabPanel("ACF Model (Halo)", plotOutput("acfHalo", height = 250), "This model shows the autocorrelation
                         between google search interests. The autocorrelation test goes from month to month. When a
                         data point goes above the blue line, it means that one month's search interest is correlated
                         with the previous month's search interest. In other words, the autocorrelation between the
                         two is statistically significant. In this case, Halo has a lot of months in the earlier part of the 
                         year that are heavily correlated with their predecessors. It is clear that this autocorrelation does not depend 
                         on white noise because there are multiple data points that are statistically significant."),
                tabPanel("ACF Model (Call of Duty)", plotOutput("acfCOD", height = 250), "The same goes for Call of Duty.
                         It also has multiple data points that are statiscally significant, but the autocorrelation is more
                         spread out here than it is with Halo. Its autocorrelation also does not depend on white noise due to having multiple
                         data points that are statistically significant."),
                tabPanel("Forecating Model (Halo)", plotOutput("foreHalo", height = 250), "This model is used to forecast the
                         trend of what Halo's search interest will look like in the next few years, As you can see, there is
                         a slight downwards trend with a 80% and 95% confidence interval. The forecast is only slightly trending
                         because this data does not have a lot of trend in it. We can see this in the decomposition model as well."), 
                tabPanel("Forecasting Model (Call of Duty)", plotOutput("foreCOD", height = 250), "This model shows the forecast of
                         what the search interest trend for Call of Duty will look like the next few years. It has 80% and 95% confidence intervals. 
                         There is hardly any trend at all for this data and is basically zero. This makes sense because the decomposition model 
                         showed that there was not much trend to begin with. This graph supports that.")
            )
        )
    )
)
server <- function(input, output, session) {
    output$panel <- renderText({
        paste("Current panel: ", input$tabset)})
    output$original <- renderPlotly(ggplotly(Int))
    output$decomp <- renderPlot(Decomp)
    output$haloseason <- renderPlot(season1)
    output$CODseason <- renderPlot(season2)
    output$acfHalo <- renderPlot(acf1)
    output$acfCOD <- renderPlot(acf2)
    output$foreHalo <- renderPlot(Haloforecast)
    output$foreCOD <- renderPlot(CODforecast)
}

shinyApp(ui = ui, server = server)



