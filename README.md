## Revealing CT Housing Market Trends with R Shiny

Web application built with R Shiny to visualize information on Connecticut's residential real estate data, between 2006 to 2022. 

Data was sourced from data.gov and can be accessed here: [Dataset Link](https://catalog.data.gov/dataset/real-estate-sales-2001-2018/resource/f7cb94d8-283c-476f-a966-cc8c9e1308b4?inner_span=True)

This Shiny app has three interactive dashboards: 

1. Annual Sales Distribution

> The initial dashboard allows users to view *pie charts* illustrating the proportion of properties sold by type, as well as the share of sales contributed by each property type. The slider enables the user to     control for the year of interest. Additionally, there is a dropdown to visualize information with a *line chart*. This graph shows the total number of residential properties sold each year, along with the median price paid for all properties sold in that year.

2. Regression Analysis

> The second dashboard provides a scatter plot of the assessed property value versus the amount that it sold for. This enables the user to see the correlation between these variables. Points aligned with the identity line signify residential properties where the buyer paid the market value. Additionally, the user has the option to visualize the regression line which quantifies the relationship between the assessed value and the amount paid. This information is provided by the **summary** function in the R programming language.

Further enhancements for this page could include checking for any departure from linearity, heteroscedasticity, potential outliers and high-leverage points by analyzing the residuals.

normality assumptions using methods such as residual analysis and addressing high-leverage points.

3. Sales Heatmap

> The final dashboard showcases details about the sold residential properties through a heatmap overlaid with geographical data on the map of Connecticut. This provides users with the ability to visually explore the distribution of sales information across various areas, including neighborhoods and communities. It also facilitates the identification of high-priced areas within the state.

The application can be accessed via this link:

[Connecticut Housing Insights App](https://madkins.shinyapps.io/myctreswebapp/)
