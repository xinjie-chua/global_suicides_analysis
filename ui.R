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

load_pckg <- function(){
  if(!require('pacman'))install.packages('pacman')
  pacman::p_load(shiny,shinythemes)
}

load_pckg()
source('main.R')

#######

shinyUI(
  fluidPage(
    dashboardPage(
      dashboardHeader(
        title = "Global Suicide Rates Analysis",
        titleWidth = 230
      ),
      dashboardSidebar(
        sidebarMenu(
          menuItem(text = 'Home', icon = icon('house'), tabName = 'home'),
          menuItem(text = 'Maps', icon = icon('globe'), tabName = 'maps'),
          menuItem(text = 'Graphs', icon = icon('chart-bar'), tabName = 'graphs'),
          menuItem(text = 'Machine Learning', icon = icon('laptop-code'), tabName = 'ml'),
          menuItem(text = 'Data', icon = icon('database'), tabName = 'data'),
          menuItem(text = 'Need Help?', icon = icon('circle-info'), tabName = 'help')
        )
      ),
      dashboardBody(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        tabItems(
          tabItem(tabName = "home", 
                  textOutput("introduction_title"),
                  textOutput("introduction1"), 
                  HTML( paste('<br/>')),
                  textOutput("introduction2"), 
                  HTML('<center><img src="https://vajiramandravi.s3.us-east-1.amazonaws.com/media/2022/11/22/12/22/32/suicideee.jpg" width="600"></center>'),
                  HTML( paste('<br/>')),
                  tags$head(tags$style("#introduction_title{ color:#00BFFF; font-size: 20px; text-align: center;}
                                        #introduction1{text-align: justify;font-size: 15px;}
                                        #introduction2{text-align: justify;font-size: 15px;}"))
          ),
          
          
          tabItem(tabName = "maps",
                  fluidRow(infoBoxOutput("maxBox"),
                           infoBoxOutput("avgBox"),
                           infoBox("Average US Suicides", subtitle = "in 2018 (/100k)", 13.9,
                                   icon = icon("globe-americas"), fill = TRUE,
                                   href = "https://www.americashealthrankings.org/explore/annual/measure/Suicide/state/ALL", color = "light-blue")),
                  fluidRow(
                    column(9, box(htmlOutput("map"), height = "auto", width = "auto")),
                    column(3,
                           radioButtons("type", label = h3("Display map by: "),
                                        choices = list("Suicides" = "suicides",
                                                       "Suicides (/100k)" = "suicides100"),
                                        selected = "suicides"),
                           checkboxGroupInput("checkGroup", label = h3("Filter by age group: "),
                                              choices = list("5-14 years", "15-24 years",
                                                             "25-34 years", "35-54 years",
                                                             "55-74 years", "75+ years"),
                                              selected = c("5-14 years", "15-24 years", "25-34 years",
                                                           "35-54 years", "55-74 years","75+ years")),
                           sliderInput("slider", label = h3("Year Range"), min = 1985, 
                                       max = 2016, value = c(1985, 2016), sep = "")
                           
                    )
                  ),
                  fluidRow(
                    infoBoxOutput("minBox", width = 12)
                  )
          ),
          tabItem(tabName = "graphs",
                  navbarPage("", id = "myNavbar",
                             tabPanel("Graph 1.1", 
                                      fluidRow(
                                        column(2, h3("Filter graphs by:"), align = "center"),
                                        column(2,
                                               selectInput("cont", label = h3("Continent"), 
                                                           choices = list("Americas", "Africa",
                                                                          "Asia", "Europe", "Oceania"))),
                                        column(2,
                                               radioButtons("sex", label = h3("Sex"), 
                                                            choices = list("Female" = "female", "Male" = "male"))),
                                        column(3,
                                               sliderInput("gdp", label = h3("GDP Range"), min = 251, 
                                                           max = 126352, value = c(251, 126352))),
                                        column(3,
                                               sliderInput("year", label = h3("Year Range"), min = 1985, 
                                                           max = 2016, value = c(1985, 2016), sep = ""))
                                        
                                      ),
                                      fluidRow(
                                        box(plotlyOutput("line"), height = 420),
                                        box(plotlyOutput("hist"), height = 420)
                                      ),
                                      fluidRow(
                                        box(plotlyOutput("scat"), height = 420, width = 12)
                                      )),
                             tabPanel("Graph 1.2",
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("country_for_analysis", "Country", choices = sort(data$country), selected = "Albania"),
                                          sliderInput("input_range_analysis", "Year Range to be Displayed", 
                                                      min = 1985, max = 2016, value = c(1996, 2005), sep = "")
                                        ), mainPanel(
                                          textOutput("analysis_title"),
                                          textOutput("analysis_introduction"),
                                          textOutput("analysis"),
                                          textOutput("analysis_gdp_explanation"),
                                          splitLayout(cellWidths = c("50%","50%"), plotOutput("analysis_gdp"), plotOutput("analysis_suicides")),
                                          textOutput("analysis_one_year_input"), 
                                          selectInput("analysis_for_year", "Select One Specific Year", choices = sort(data$year)),
                                          selectInput("analysis_for_sex", "Select Sex", choices = unique(data$sex)),
                                          plotlyOutput("pie_analysis"),
                                          
                                          tags$head(tags$style("#analysis_title{ color: #00BFFF; font-size: 20px}"))
                                        )
                                      )
                  ))),
          tabItem(tabName = "ml",
                  fluidRow(
                    column(9,
                           fluidRow(
                             box(plotlyOutput('plot1'), height = 420),
                             box(plotlyOutput('plot2'), height = 420))
                    ),
                    column(3,
                           selectInput('xcol', 'X Variable',
                                       choices = list("suicides", "population",
                                                      "suicides.per.100k", "gdp.capita")),
                           selectInput('ycol', 'Y Variable',
                                       choices = list("suicides", "population",
                                                      "suicides.per.100k", "gdp.capita"),
                                       selected = "population"),
                           numericInput('clusters', 'Cluster count', 5,
                                        min = 1, max = 9))
                  )
          ),
          tabItem(tabName = "data",
                  sidebarLayout(
                    sidebarPanel(
                      sliderInput("input_year", "Year", value = 1985, min = 1985, max = 2016, sep=""),
                      selectInput("input_country","Country", choices = sort(data$country)),
                      radioButtons("input_gender","Sex", choices = c("Male" , "Female", "Both")),
                      selectInput("input_age","Age Group", choices = sort(data$age))
                    ), mainPanel(
                      textOutput("table_text"),
                      dataTableOutput("table"),
                      tags$head(tags$style("#table_text{color:#00BFFF; font-size:20px}"))
                    ))
          ),
        tabItem(tabName = "help",
                textOutput("help_title"),
                textOutput("help_description"), 
                tags$head(tags$style("#help_title{ color:#00BFFF; font-size: 20px; text-align: center;}
                                          #help{text-align: justify;font-size: 15px;}"))
                )
        )
      )
    )
  )
)  