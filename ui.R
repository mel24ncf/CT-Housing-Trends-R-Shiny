#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(dplyr)
library(plotly)
library(tidyr)
library(data.table)
library(viridis)
library(shinythemes)


# Define UI for application that draws a histogram
ui <- fluidPage( 
  theme = shinytheme("cosmo"),
  titlePanel("Connecticut Real Estate Data: 2006 - 2021"),
  
  
  tabsetPanel(
    tabPanel("Annual Sales Distribution",
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
                 verbatimTextOutput("messageOutput"),
                 verbatimTextOutput("lm_summary_output") #
               )
             )
             
    ),
    
    tabPanel("Sales Heatmap",
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