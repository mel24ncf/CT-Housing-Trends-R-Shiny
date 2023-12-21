#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(plotly)
library(tidyr)
library(data.table)
library(viridis)
library(shinythemes)
library(shiny)

# Read data files using relative paths
all_res <- fread("data/all_res.csv")

# Define server logic required to draw plots and tables
server <- function(input, output, session) {
  
  output$pie1 <- renderPlotly({
    year <- input$year
    filtered_data <- all_res %>% 
      filter(Year.Recorded == year,
             Residential.Type %in% c("Condo", "Single Family", "Two Family", "Three Family", "Four Family"))
    
    # Create a data frame with the total number of each residential type
    rescounts_data <- data.frame(
      type = c("Condo", "Single Family", "Two Family", "Three Family", "Four Family"),
      count = c(
        sum(filtered_data$Residential.Type == "Condo"),
        sum(filtered_data$Residential.Type == "Single Family"),
        sum(filtered_data$Residential.Type == "Two Family"),
        sum(filtered_data$Residential.Type == "Three Family"),
        sum(filtered_data$Residential.Type == "Four Family"))
    )
    
    # Calculate the percentage for each residential type
    rescounts_data$percentage <- rescounts_data$count / sum(rescounts_data$count) * 100
    
    # Create the pie chart
    plot_ly(rescounts_data,
            labels = ~type,
            values = ~count,
            textinfo = "label + percent",
            customdata = ~count,
            hovertemplate = "%{label}: %{customdata}<extra></extra>",   
            name = "",
            type = "pie") %>%
      # Add a title to the pie chart
      layout(title = paste("Annual Percentage Breakdown of Residential Property Types Sold in Connecticut in", year),
             annotations = list(text = "", showarrow = FALSE))
    
  })
  
  output$pie2 <- renderPlotly({
    year <- input$year
    filtered_data <- all_res %>% 
      filter(Year.Recorded == year,
             Residential.Type %in% c("Condo", "Single Family", "Two Family", "Three Family", "Four Family"))
    
    sales_amount <- filtered_data %>%
      group_by(Residential.Type) %>%
      summarise(sales_amount = sum(Sale.Amount))
    
    # Calculate the percentage of total sales amount for each residential type
    sales_amount$percentage <- sales_amount$sales_amount / sum(sales_amount$sales_amount) * 100
    
    # Create the pie chart
    plot_ly(sales_amount, 
            labels = ~Residential.Type, 
            values = ~sales_amount,
            textinfo = "label + percent",
            customdata = ~sales_amount,
            hovertemplate = "%{label}: $%{customdata:,.0f}<extra></extra>",
            name = "",
            type = "pie") %>%
      
      # Add a title to the pie chart
      layout(title = paste("Annual Percentage Breakdown of Sales by Residential Property Type in Connecticut in",year), annotations = list(text = "", showarrow = FALSE))
  })
  
  
  output$line <- renderPlotly({
    
    all_res_prop_counts <- all_res %>% select(Year.Recorded, Residential.Type, Sale.Amount) %>%
      filter(Year.Recorded >= 2006) %>%
      group_by(Year.Recorded) %>%
      summarise(Total.Properties = n())
    
    
    avg_sale_amounts <- all_res %>% 
      select(Year.Recorded, Sale.Amount) %>%
      filter(Year.Recorded >= 2006) %>%
      group_by(Year.Recorded) %>%
      summarise(med = median(Sale.Amount))
    
    bp <- plot_ly(data = all_res_prop_counts,
                  type = "scatter",
                  mode = "lines + markers",
                  x = ~Year.Recorded,
                  y = ~Total.Properties,
                  name = "Number of Properties Sold",
                  hoverinfo = "y") %>%
      layout(title = list(text = ""),
             xaxis = list(title = "Year"),
             yaxis = list(title = "",
                          tickformat = ".0f")) 
    
    lp <- plot_ly(data = avg_sale_amounts,
                  type = "scatter",
                  mode = "lines + markers",
                  x = ~ Year.Recorded,
                  y = ~ med,
                  hovertemplate = "$%{y:}",
                  name = "Median Sale Amount",
                  yaxis = "y")
    
    subplot(bp, lp, nrows = 2, shareX = TRUE) %>% 
      layout(
        title = list(text = "Number of Residential Property sales and Median Sale Amounts by Year", xref = "paper"),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Properties Sold"),
        yaxis2 = list(title = "Median Sale Amounts", side = "right", overlaying = "y", tickformat="$"), 
        showlegend = TRUE,
        barmode = "group",
        legend = list(
          x = 1.1,
          y = 0.5,
          xanchor = "left",
          yanchor = "right"  
        ))
  })
  
  
  output$scatPlot <- renderPlotly({
    prop <- input$propType
    year_range <- input$yearRange
    all_res_prop <- all_res %>% filter(Residential.Type == prop, Year.Recorded >= year_range[1], Year.Recorded <= year_range[2], Assessed.Value > 0) 
    
    all_res_prop$Sale.Amount_Formatted <- scales::dollar(all_res_prop$Sale.Amount, prefix = "$", suffix = "", big.mark = ",")
    all_res_prop$Assessed.Value_Formatted <- scales::dollar(all_res_prop$Assessed.Value, prefix = "$", suffix = "", big.mark = ",")
    
    pscatter <- plot_ly(data = all_res_prop,
                        x = ~Sale.Amount,
                        y = ~Assessed.Value,
                        text = ~paste("Sale Amount: ", Sale.Amount_Formatted, "<br>Assessed Value: ", Assessed.Value_Formatted), 
                        type = "scatter",
                        name = "<extra></extra>",
                        mode = "markers",
                        hoverinfo = "text") %>%  
      layout(xaxis = list(title = "Sale Amount ($)",
                          tickprefix = "$"),
             yaxis = list(title = "Assessed Value ($)",
                          tickprefix = "$"),
             title = paste("Assessed Value vs Sale Amount for", prop, "Properties in Connecticut"), xref="paper")
    if (input$add_line == "Yes"){
      lm_model <- lm(Assessed.Value ~ Sale.Amount, data = all_res_prop)
      pscatter <- add_trace(pscatter, x = ~Sale.Amount, y = predict(lm_model), mode = "lines", name = "<extra></extra>", line = list(color="red", lwd=2))
      ####
      # Extract the linear regression summary
      lm_summary <- summary(lm_model)
      
      # Print the summary
      output$lm_summary_output <- renderPrint({
        print(lm_summary)
      })
      
      ####
      lin_eqn <- paste0("y = ", round(coef(lm_model)[1], 2), " + ", round(coef(lm_model)[2], 2), "x")
      pscatter <- layout(pscatter, annotations = list(
        x=1,
        y=1,
        xref = "paper",
        yref = "paper",
        text = lin_eqn,
        showarrow = FALSE,
        font = list(size = 12, color = "black")
        
      ))
      pscatter <- pscatter %>% layout(showlegend = FALSE)
    }
    pscatter
    
  })
  
  
  mapdata <- reactive({ 
    temp <- data.frame(all_res %>% filter(Year.Recorded == input$yearInput, 
                                       !is.na(Latitude), !is.na(Longitude), 
                                       Property.Type %in% c("Residential", "Apartments", "Condo", "Single Family", "Two Family", "Three Family", "Four Family") |
                                         Residential.Type %in% c("Condo", "Single Family", "Two Family", "Three Family", "Four Family")))
    
    if (input$removeoutliers == TRUE) {
      q1 <- quantile(temp$Sale.Amount, 0.25, na.rm = TRUE)
      q3 <- quantile(temp$Sale.Amount, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      
      temp <- temp[temp$Sale.Amount >= lower_bound & temp$Sale.Amount <= upper_bound, ] 
      
    } else {
      temp <- temp
    }
    
    temp
    
  })
  
  output$heatmap <- renderPlotly(
    {
      year <- input$year#
      plot_ly(mapdata(), 
              type = "densitymapbox",
              lat = ~Latitude,
              lon = ~Longitude,
              z = ~Sale.Amount,
              colorscale = "Viridis",
              colorbar = list(title = "Sale Amount")) %>%
        layout(mapbox = list(style = "open-street-map",
                             center = list(lat = 41.6, lon = -72.4), 
                             zoom = 7.8), 
               title = list(text = "Density Map of Connecticut Residential Property Sale Amounts", xref="paper")
               #               title = list(text = paste("Density Map of Connecticut Residential Property Sale Amounts", year), xref="paper")
        )
    }
  )
  
  # Create the Mapbox scattermap
  output$scattermap <- renderPlotly(
    {
      
      plot_ly(mapdata(),
              type = "scattermapbox",
              mode = "markers",
              lat = ~Latitude,
              lon = ~Longitude,
              marker = list(color = ~Sale.Amount,
                            colorscale = "Viridis",
                            colorbar = list(title = "Sale Amount")),
              hoverinfo = "text",
              text = ~paste("Year: ", Year.Recorded, "<br>",
                            "Sale Amount: $", Sale.Amount, "<br>",
                            "Assessed Value: $", Assessed.Value, "<br>",
                            "Sales Price Ratio:", Sales.Ratio, "<br>",
                            "Property Type: ", Property.Type) #, "<br>",
              #"Residential Type", Residential.Type, "")
      ) %>%
        layout(mapbox = list(style = "open-street-map",
                             center = list(lat = 41.6, lon = -72.4),
                             zoom = 7.4),
               title = list(text = "Location of Connecticut Residential Property Sales", xref ="paper")
        ) 
    }
  )
  
  
}