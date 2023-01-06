library(shiny)
library(shinydashboard)
library(tidyverse)
library(googleVis)
library(maps)
library(plotly)
library(ggthemes)
library(gganimate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(leaflet)
library(highcharter)

load_pckg <- function(){
  if(!require('pacman'))install.packages('pacman')
  pacman::p_load(shiny,shinythemes)
}

load_pckg()
source('main.R')

shinyServer(function(input, output){
  
  ########## Home #####################################
  
  output$introduction_title <- renderText({
    intro <- paste0("An Introduction to Our Findings, Our Purpose, and Our Goal")
  })
  output$introduction1 <- renderText({
    text <- paste("Welcome to the Suicide Rate Analysis Project homepage. This project is dedicated to understanding and addressing the issue of suicide, 
                  which is a significant public health concern. Our purpose in creating this application is to provide a tool for understanding and analysing 
                  the suicide rates in different countries. Our application allows users to examine the trends in suicide rates over time, including the 
                  change in rates over a period of years. This feature enables users to gain a better understanding of the trajectory of suicide rates and 
                  whether they are increasing or decreasing. Our application also allows users to examine the patterns in suicide rates among different age 
                  groups. This feature enables users to identify which age groups may be at higher risk of suicide and prioritize efforts to support and 
                  protect these individuals. 
                  ")
  })
  output$introduction2 <- renderText({
    text <- paste("Additionally, our application presents data on the relationship between different variables, providing a glimpse into potential contributing
                   factors to suicide rates. By examining these patterns and correlations, we hope to gain a deeper understanding of the factors that influence 
                   suicide rates and inform the development of effective prevention strategies. By presenting the data in an accessible and user-friendly way, 
                   we hope to raise awareness about this important issue and encourage people to take action to prevent suicide. Our goal is to use the insights 
                   gained from our analysis to develop strategies for reducing suicide rates and supporting those who are at risk. We hope that the information 
                   and resources provided on this site will be helpful in this important effort. We believe that with better understanding and knowledge, we 
                   can work towards a future where fewer lives are lost to suicide.")
  })
  
  
  #### Map Tab #########################################################
  df1 <- reactive({
    if(is.null(input$checkGroup)){
      df1 = data %>% 
        filter(between(year, input$slider[1], input$slider[2])) %>%
        group_by(country) %>%
        summarise(suicide_rate = (suicide_per_100k = sum(suicides_no) / sum(population) * 100000),
                  suicides = sum(suicides_no))
    }
    else{
      df1 = data%>%
        filter(age %in% input$checkGroup) %>%
        filter(between(year, input$slider[1], input$slider[2])) %>% 
        group_by(country) %>%
        summarise(suicide_rate = (suicide_per_100k = sum(suicides_no) / sum(population) * 100000),
                  suicides = sum(suicides_no))
    }
  })
  
  
  ## Render Map
  output$map <-renderGvis({
    gvisGeoChart(data = df1(), locationvar = "country", colorvar = input$type,
                 options = list(region="world", displayMode="auto",
                                resolution="countries", width="100%", height="100%",
                                colorAxis="{colors:['#6f92e6', '#f9897e']}"))
  })
  
  output$maxBox <- renderInfoBox({
    max_value <- round(max(df1()$suicide_rate), 2)
    max_state <- df1()$country[df1()$suicide_rate == (max(df1()$suicide_rate))]
    infoBox("Country with highest suicide rates",max_state, max_value, icon = icon("hand-up", lib = "glyphicon"), color = "light-blue")
  })
  output$avgBox <- renderInfoBox({
    avg_value <- round(mean(df1()$suicide_rate), 2)
    infoBox("Average Global Suicides (/100k)", avg_value, icon = icon("calculator"), color = "light-blue")
  })
  output$minBox <- renderInfoBox({
    min_state <-
      df1()$country[df1()$suicide_rate == 0]
    min_state = paste(min_state, collapse = ", ")
    infoBox(min_state, title = "Countries w/ 0 Suicides (/100k): ",
            icon = icon("user-times"), color = "navy")
  })
  
  #### Graphs Tab ######################################################
  ## Graph 1.1 #######################
  df2 <- reactive({
    df2 = data %>% 
      filter(continent == input$cont) %>%
      filter(sex == input$sex) %>%
      filter(between(gdp_per_capita, input$gdp[1], input$gdp[2])) %>%
      filter(between(year, input$year[1], input$year[2])) %>% 
      arrange(desc(suicides_no))
  })
  
  output$line <- renderPlotly(
    # check by gdp per capita for each country in the eu of suicides per 100k by generation (density)
    ggplotly(df2() %>%
               group_by(country, year) %>% 
               summarise(suicides100 = mean(suicide_per_100k)) %>% 
               ggplot(aes(year, suicides100)) +
               geom_line(aes(color = country), show.legend = FALSE,
                         alpha = 0.25) +
               labs(title = "Suicides (/100k) vs. Year", x = "Year", y = "Suicides (/100k)") +
               scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
               theme_gdocs()) %>% 
      layout(legend = list(x = 100, y = 0.5))
  )
  
  output$hist <- renderPlotly(
    ggplotly(df2() %>% 
               group_by(country, year) %>% 
               ggplot(aes(year, gdp_per_capita)) + 
               geom_col(aes(fill = country), position = "dodge") +
               theme_gdocs() +
               labs(title = "GDP per Capita vs. Year", x = "Year", y = "GDP per Capita"))
    # ggplot(df1()) + geom_smooth()
  )
  
  output$scat <- renderPlotly(
    ggplotly(df2() %>%
               group_by(country) %>% 
               summarise(population = mean(population), suicides = sum(suicides_no)) %>% 
               ggplot() +
               geom_point(aes(x = population, y = suicides, size = population, color = country),
                          show.legend = FALSE, alpha = 0.5) +
               theme_gdocs() +
               labs(title = "Suicides vs. Population", x = "Population", y = "Suicides"))
  )
  
  
  ## Graph 1.2 #######################
 
  overall_tibble <- data %>%
    select(year, suicides_no, population) %>%
    group_by(year) %>%
    summarise(suicide_per_100k = round((sum(suicides_no)/sum(population))*100000, 2)) 
  
  output$general <- renderHighchart({
    highchart() %>% 
      hc_add_series(overall_tibble, hcaes(x = year, y = suicide_per_100k, color = suicide_per_100k), type = "line") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br> Suicides: <b>{point.y}</b>")) %>%
      hc_title(text = "Global Suicides by Year") %>% 
      hc_subtitle(text = "1985-2016") %>%
      hc_xAxis(title = list(text = "Year"),
               min = input$slider[1],
               max = input$slider[2]) %>%
      hc_yAxis(title = list(text = "Suicides per 100K population"),
               allowDecimals = FALSE,
               plotLines = list(list(
                 color = "black", width = 1, dashStyle = "Dash", 
                 value = mean(overall_tibble$suicide_per_100k),
                 label = list(text = "Mean = 13.12", 
                              style = list(color = "black"))))) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_add_theme(custom_theme)
  })
  
  age_tibble <- data %>%
    select(year, age, suicides_no, population) %>%
    group_by(year, age) %>%
    summarise(suicide_per_100k = round((sum(suicides_no)/sum(population))*100000, 2))
  
  output$age <- renderPlotly({
    
    plot_ly(age_tibble, y = ~suicide_per_100k, color= ~age, type = 'box', showlegend = FALSE) %>% layout(
      title = 'Global Suicides Per Age',
      xaxis = list(title = 'Age Range'),
      yaxis = list(title = 'Suicide per 100K Population'))                                                                     
    
  })
  
  pie_sex <- data %>%
    select(sex, suicides_no, population) %>%
    group_by(sex) %>%
    summarise(suicide_per_100k = round((sum(suicides_no)/sum(population))*100000, 2))
  
  output$pie <- renderHighchart({
    highchart() %>% 
      hc_add_series(pie_sex, hcaes(x = sex, y = suicide_per_100k, color = sex_color), type = "pie") %>%
      hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("Gender: <b>{point.sex} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
      hc_title(text = "Global suicides by Gender") %>% 
      hc_subtitle(text = "1985-2015") %>%
      hc_plotOptions(pie = list(dataLabels = list(distance = 15, 
                                                  style = list(fontSize = 10)), 
                                size = 130)) %>% 
      hc_add_theme(custom_theme)
  })
  
  
  ## Graph 1.3 #######################
  
  output$analysis <- renderText({
    analysis_data <- data %>% filter(input$country_for_analysis == country) %>% group_by(year) %>% summarize(suicides = sum(suicides_no)) %>% 
      filter( input$input_range_analysis[1] <= year & input$input_range_analysis[2] >= year ) %>% select(suicides)
    
    # Message to display if nothing selected
    validate(
      need(nrow(analysis_data) > 1, message = "Please select a country to analyze and look at.")  
    )
    
    old <- as.integer(analysis_data[1,1])
    new <- as.integer(analysis_data[nrow(analysis_data),1])
    change <- new - old
    if(old != 0){
      perc_inc <- round((change/old)*100,2)
    } else {
      perc_inc <- 0
    }
    if(change > 0 & nrow(analysis_data) > 1) {
      text <- paste0("In ", input$country_for_analysis," there was a ", perc_inc,"% increase in suicide rates.")
    } else if(change <= 0  & nrow(analysis_data) > 1){
      text <- paste0("In ", input$country_for_analysis, " there was a ", abs(perc_inc),"% decrease in suicide rates.")
    }  else {
      text <- paste0("Data is not available")
    }
    text
  })
  
  # graph in analysis that shows gdp line graph
  output$analysis_gdp <- renderPlot({
    gdp <- data %>% 
      filter(input$country_for_analysis == country) %>% select(country, year, suicides_no, gdp_per_capita) %>% 
      group_by(country, year, gdp_per_capita) %>% summarize(suicides = sum(suicides_no)) %>% 
      filter( input$input_range_analysis[1] <= year & input$input_range_analysis[2] >= year )
    
    validate(
      need(nrow(gdp) > 1, "Please select a country.")
    )
    
    plot <- ggplot(gdp, aes(gdp$year, gdp$gdp_per_capita)) + geom_line() + labs(title = "A View of GDP-per-capita in each year of the Selected Nation") +
      xlab("Year") + ylab("GDP-per-capita")
    plot
  })
  
  # graph in analysis that shows suicide number line graph
  output$analysis_suicides <- renderPlot({
    test <- data %>% filter(input$country_for_analysis == country) %>% group_by(year) %>% summarize(suicides = sum(suicides_no)) %>% 
      filter( input$input_range_analysis[1] <= year & input$input_range_analysis[2] >= year )
    
    validate(
      need(nrow(test) > 1 , "Please select a country.")
    )
    
    testing <- ggplot(test, aes(year, suicides)) + geom_line() + labs(title = "A View of total suicide numbers for each year in the selected Country") +
      xlab("Year") + ylab("Total number of suicides")
    testing
  })
  
  
 ## bar chart for age 
  
  output$bar_title <- renderText({
    explanation <- paste0("What is the most vulnerable age group in ", input$analysis_for_year, " in ", input$country_for_analysis, "?")
  })
  output$barg <- renderPlot({
    
    # the user selects both in the input gender widget then display bar graph with data containing both sexes
  
      both_case <- data %>% filter(input$analysis_for_year == year, input$country_for_analysis == country)
      
      # Error message that there is no data available (data for graph is empty)
      validate(
        need(nrow(both_case) > 1, message = "Data not available / No recorded data available.")
      )
      
      # bar graph with ggplot 
      output <- ggplot(both_case, aes(both_case$age, both_case$suicides_no, fill = both_case$sex)) + geom_bar(stat="identity", position ="dodge") +
        scale_fill_brewer(palette = "Set1") + labs(title = " A Look into Suicide numbers for each Age Group for both genders", fill = "Gender") + 
        xlab("Age Groups") + ylab("Number of Suicides") 
      output
  })
  
  # Text explanation for bar graph 
  output$bar_explanation <- renderText(
    explanation <- paste("The graph (if displayed) shows information about suicide numbers in each particular age group in the selected 
                         year that is chosen. A comparison of this allows us to be able to conclude which age group, in that country in the
                         particular year contains the highest suicide numbers.")
  )
  
  
  output$analysis_title <- renderText({
    text <- paste("Analysis")
  })
 
  output$analysis_introduction <- renderText({
    text <- paste("Given the information from the user-selected COUNTRY and the given range of YEARS, we are able to calculate that : ")
  })
  
  output$analysis_gdp_explanation <- renderText({ 
    text <- paste("Given the information of the selected country and the year, the graphs below show the GDP in the specific range of years and
                  the graph of the number of suicides with the same range of years. With the side-by-side graphical visual representation, we can 
                  see that for most of the countries, with higher gradual growing GDP, comes with also a higher number of suicides. This then, can lead to future
                  investigations of other factors such as: economics, healthcare, support, wages, etc... Our analysis shows a potential relationship
                  of GDP increase or decrease in relation to suicide rates. Another trend in which we see is that a sudden spike of decrease 
                  in GDP also result in a high increase of suicide numbers - 1995-2005 Argentina. Likewise, a sudden burst of increase in GDP, may
                  result in a big decrease in suicide rates - 2002 - 2005 Australia. This suggests that when a nation suffers 
                  economically, the people also are more prone to suicide. From these observations, we can also reach to a conclusion that countries
                  with a gradual growth results in the gradual growth in both factors, whereas countries with sudden drops or spikes may result 
                  in one factor being increased and the other decreased. GDP is not the lone factor that determines suicide rates, but from this 
                  analysis we can observe that there may be some significant relation between the two.")
  })
  
  
  #### ML Tab ##########################################################
  selectedData <- reactive({
    df3[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlotly({
    vcol = (c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    ggplotly(ggplot(selectedData(), aes(x = selectedData()[[1]], y = selectedData()[[2]],
                                        col = vcol[clusters()$cluster])) +
               geom_point(alpha = 0.5, show.legend = FALSE) +
               theme_gdocs() +
               scale_fill_manual("Clusters") +
               labs(title = "K Means Clustering", x = as.character(input$xcol), y = as.character(input$ycol))) %>% 
      layout(showlegend = FALSE)
  })
  
  output$plot2 <- renderPlotly(
    ggplotly(ggplot(selectedData(), aes(x = selectedData()[[1]], y = selectedData()[[2]],
                                        col = df3$country)) +
               geom_point(alpha = 0.5) +
               theme_gdocs() +
               labs(title = "Original Data", x = as.character(input$xcol), y = as.character(input$ycol)))
  )
  
  output$scat <- renderPlotly(
    ggplotly(df2() %>%
               group_by(country) %>% 
               summarise(population = mean(population), suicides = sum(suicides_no)) %>% 
               ggplot() +
               geom_point(aes(x = population, y = suicides, size = population, color = country),
                          show.legend = FALSE, alpha = 0.5) +
               theme_gdocs() +
               labs(title = "Suicides vs. Population", x = "Population", y = "Suicides"))
  )
  
  #### Data Tab #######################################################
  output$table_text <- renderText({
    text <- paste("A Look Into Our Dataset, specified just for your choosings.")
  })
  
  output$table <- renderDataTable({
    # Warning messages for user to see if no values selected
    validate(
      need(input$input_country, message = "Please select country."),
      need(input$input_age, message = "Please select intended age group.")
    ) 
    
   desired_df <-  data %>%
      filter(input$input_country == country,age %in% input$input_age,input$input_year == year) %>% 
      select(country,year,sex,age,suicides_no,population,suicide_per_100k,generation)
    
  }) 
})  
  