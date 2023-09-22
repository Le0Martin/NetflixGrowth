# Web App to Big Data: How do GDP and Social Inequality Influence Netflix’s Growth?

setwd("C:/Users/Leo/Desktop/Portfolio/Netflix-R")
getwd()

########## Analitical Dashboard ##########

# Imports
library(shiny)
library(plotly)
library(shinythemes)

# Load the first clean dataset
dataset1 <- read.csv("clean_datasets/dataset1.csv")

# Adjust the data type of some columns
dataset1$X..of.Subscribers.Q4.2021..Estimate. <- as.numeric(gsub(",", "", dataset1$X..of.Subscribers.Q4.2021..Estimate.))
dataset1$Q4.2021.Revenue....Estimate. <- as.numeric(gsub(",", "", dataset1$Q4.2021.Revenue....Estimate.))

# Create dataframes by filtering outliers
dataset1_scat_out <- filter(dataset1, Country != "United States")
dataset1_bar <- filter(dataset1, Country != "Switzerland")
dataset1_bar_out <- filter(dataset1_bar, Country != "South Africa")

# Load datasets 2, 3 and 6
genre <- read.csv("clean_datasets/dataset2.csv")
tree <- read.csv("clean_datasets/dataset3.csv")
countries <- read.csv("clean_datasets/dataset6.csv")

# Removing NA values in country list
country_list <- filter(countries, is.na(parents))


########## UI - User Interface ##########

ui <- navbarPage(theme = shinytheme("cerulean"), 
                 
                 "Influences of Netflix Growth",
                 
                 tabPanel("Overview",
                          sidebarLayout(
                            sidebarPanel(                              
                              selectInput("select", 
                                          label = h4("Select the Y-Axis Variable:"), 
                                          choices = list("Netflix Billing Q4-2021" = "Q4.2021.Revenue....Estimate.", 
                                                         "Netflix Subscriptions Q4-2021" = "X..of.Subscribers.Q4.2021..Estimate.",
                                                         "Total Catalog Size" = "Total.Library.Size", 
                                                         "Basic Subscription Price" = "Cost.Per.Month...Basic....", 
                                                         "Standard Subscription Price"= "Cost.Per.Month...Standard....",
                                                         "Premium Subscription Price" = "Cost.Per.Month...Premium...."), 
                                          selected = 1),
                              checkboxInput("outlierscatter", "Show Outlier", FALSE)),
                            mainPanel(
                              plotlyOutput("scatPlot")))
                 ),
                 tabPanel("Pay Inequality",
                          h4("Income Disparity and Differences in Netflix Basic, Standard and Premium Subscription Prices (Monthly)"),
                          sidebarPanel(
                            checkboxInput("outlierbar", "Show Outlier", FALSE)),
                          mainPanel(
                            plotlyOutput("barPlot"))
                 ),
                 tabPanel("Popular Genres",
                          tabPanel("Country", 
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("Country", 
                                                   label = h3("Select the Country:"), 
                                                   choices = country_list$labels, 
                                                   selected = 1),
                                     ),
                                     mainPanel(
                                       h3("Film Genre Popularity By Country"),
                                       h5("Based on the number of times a film/TV show of a genre has been in the weekly Netflix Top 10 in a country (Data from June/2021 - March/2022)."),
                                       plotlyOutput("countryPlot")
                                     )
                                   )
                          ), 
                 ),
                 tabPanel("Netflix Subscribers",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("select3", 
                                          label = h3("Select Scale:"), 
                                          choices = list("Netflix Billing Q4-2021" = "Q4.2021.Revenue....Estimate.", 
                                                         "Netflix Subscriptions Q4-2021" = "X..of.Subscribers.Q4.2021..Estimate."),
                                          selected = 1),
                              checkboxInput("outliermap", "Show Outlier", FALSE),),
                            mainPanel(
                              plotlyOutput("mapPlot"),
                              h3("Billing x Subscriptions"),
                              plotlyOutput("mapscatPlot")),
                            
                          )
                 )
)

########## Server Logic ##########

server <- function(input, output) {
  
  # Scatter Plot
  output$scatPlot <- renderPlotly({
    if (input$outlierscatter){
      dfs <- dataset1
    } else {
      dfs <- dataset1_scat_out
    }
    fig <- plot_ly(data = dfs, x = ~X2020.GDP..World.Bank., y = ~get(input$select), type = "scatter", mode = "markers", text = ~Country)
    fig <- fig %>% layout(yaxis = list(title = 'Selected variable'), xaxis = list(title = 'GDP (USD)'))
    fig
  })
  
  # Bar Plot
  output$barPlot <- renderPlotly({
    if (input$outlierbar){
      dfb <- dataset1_bar
    } else {
      dfb <- dataset1_bar_out
    }
    fig <- plot_ly(dfb, x = ~gini_disp, y = ~Cost.Per.Month...Basic...., type = 'bar', name = 'Basic', text = ~Country)
    fig <- fig %>% add_trace(y = ~basic_standard_diff, name = 'Standard')
    fig <- fig %>% add_trace(y = ~standard_premium_diff, name = 'Premium')
    fig <- fig %>% layout(yaxis = list(title = 'Monthly subscription cost (USD)'), xaxis = list(title = 'Pay Inequality'), barmode = 'stack')
    fig
  })
  
  # Country Plot
  output$countryPlot <- renderPlotly({
    country <- filter(countries, parents == input$Country)
    country <- rbind(filter(countries, labels == input$Country), country)
    fig <- plot_ly(country, ids = ~id, labels = ~labels, parents = ~parents, values = ~n, type = 'treemap', branchvalues = 'total', pathbar = list(visible = TRUE))
    
    fig
  })
  
  # Treemap Plot
  output$treePlot <- renderPlotly({
    fig <- plot_ly(tree, ids = ~id, labels = ~label, parents = ~parent, values = ~n, type = 'treemap', branchvalues = 'total', pathbar = list(visible = TRUE))
    
    fig
  })
  
  # Map
  output$mapPlot <- renderPlotly({
    if (input$outliermap){
      dfm <- dataset1
    } else {
      dfm <- dataset1_scat_out
    }
    
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # https://en.wikipedia.org/wiki/List_of_map_projections
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Miller')
    )
    
    fig <- plot_geo(dfm)
    fig <- fig %>% add_trace(z = ~get(input$select3), 
                             color = ~get(input$select3), 
                             colorscale = 'Purples', 
                             text = ~Country, 
                             locations = ~Alpha.3.code, 
                             marker = list(line = l))
    
    fig <- fig %>% colorbar(title = 'Scale')
    fig <- fig %>% layout(title = 'Netflix Global Map in Q4-2021')
    fig
  })
  
  # Scatter Plot
  output$mapscatPlot <- renderPlotly({
    if (input$outliermap){
      dfms <- dataset1
    } else {
      dfms <- dataset1_scat_out
    }
    fig <- plot_ly(data = dfms, x = ~X..of.Subscribers.Q4.2021..Estimate., y = ~Q4.2021.Revenue....Estimate., type = "scatter", mode = "markers", text = ~Country)
    fig <- fig %>% layout(yaxis = list(title = 'Netflix Billing in Q4-2021'), xaxis = list(title = 'Netflix Subscribers in Q4-2021'))
    fig
  })
}

# Executa a app
shinyApp(ui, server)