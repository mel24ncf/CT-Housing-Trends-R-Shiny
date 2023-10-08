library(shiny)
library(dplyr)
library(plotly)
library(tidyr)
library(data.table)
library(viridis)
library(shinythemes)

CTre <- fread("Data/Real_Estate_Sales_2001-2020_GL.csv", na.strings = c("", "NA"))
colnames(CTre) <- gsub(" ", ".", colnames(CTre))
CTre <- CTre %>%
  mutate(Location = gsub("POINT|\\(|\\)", "", Location)) %>%
  extract(Location, into = c("Longitude", "Latitude"), regex = "([-+]?\\d+\\.?\\d*)\\s+([-+]?\\d+\\.?\\d*)", convert = TRUE) %>%
  mutate(Date.Recorded = as.Date(Date.Recorded, format = "%m/%d/%Y")) %>%
  mutate(Month.Recorded = format(Date.Recorded, "%m"),
         Day.Recorded = format(Date.Recorded, "%d"),
         Year.Recorded = format(Date.Recorded, "%Y")) %>%
  select(-Date.Recorded) %>%
  filter(Longitude >= -73.7272 | is.na(Longitude),
         Longitude <= -71.7863 | is.na(Longitude),
         Latitude >= 40.9802 | is.na(Latitude),
         Latitude <= 42.0511 | is.na(Latitude))

all_res <-  CTre %>% filter(Year.Recorded >= 2001 & Year.Recorded <= 2021,
                            Residential.Type %in% c("Condo", "Single Family", "Two Family", "Three Family", "Four Family"))

# Define UI for application that draws a histogram
ui <- fluidPage( 
  theme = shinytheme("superhero"),
  titlePanel("Connecticut Real Estate Data: 2006 - 2021"),
  
  
  tabsetPanel(
    tabPanel("Overview",
             sidebarLayout(
               fluid = TRUE,
               sidebarPanel(
                 selectInput("plotType", "Select Visualization", choices = c("Pie Chart", "Line Chart")),
                 conditionalPanel(
                   condition = "input.plotType == 'Pie Chart'",
                   sliderInput("year", "Select Year", min = 2006, max = 2021, value = 2006, ticks=FALSE, step = 1, animate=FALSE, sep="")
                 ),
                 br(),
                 p(em("Utilize the filters to change the data displayed")),
                 
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.plotType == 'Pie Chart'",
                   plotlyOutput("pie1"),
                   plotlyOutput("pie2")
                 ),
                 conditionalPanel(
                   condition = "input.plotType == 'Line Chart'",
                   plotlyOutput("line", height = "1000px")
                 )
               )
             ),
    ),
    
    # Second tab: Scatter plot panel
    tabPanel("Regression Analysis",                              
             # Add your inputs here, if any
             sidebarLayout(
               sidebarPanel(
                 selectInput("propType", "Select Property Type", choices = c("Condo", "Single Family", "Two Family", "Three Family", "Four Family")),
                 sliderInput("yearRange", "Select Year Range", min = 2006, max = 2021, value = c(2007,2008), step = 1, sep="", ticks=FALSE), 
                 radioButtons("add_line", "Add Linear Regression Line:", choices = c("Yes", "No"), selected="No")
               ),
               
               mainPanel(
                 plotlyOutput("scatPlot"),
                 verbatimTextOutput("messageOutput")
               )
             )
             
    ),
    
    tabPanel("Geographical Data",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(column(4, checkboxInput("removeoutliers", "Remove Outliers", value = TRUE)), column(8, sliderInput("yearInput", "Select Year", 2006, 2021, 2008, sep="", ticks=FALSE)),),
                 
               ),
               mainPanel(
                 plotlyOutput("heatmap"),
                 plotlyOutput("scattermap")
               )
             )
             
    )
  )
)
