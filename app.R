options(shiny.maxRequestSize = 30*1024*1024)

library(shiny) # Package for shiny applications
library(dplyr) # Package for data manipulations
library(magrittr) # Package for pipe operator
library(ggplot2) # Package for creating graphs
library(shinythemes) # Package for universal theme of Shiny app

course_data <- readRDS("data/europe.rds") %>% # Load the course data
  mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius

# Define UI for application
ui <- fluidPage(
  
  theme = shinytheme("journal"),
  
  # Application title
  titlePanel(strong("COURSE SHINY APP")),
  
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      "This is the sidebar panel",
      
      # Input: A simple slider ----
      sliderInput(inputId = "year", label = "Year",
                  min = 2000,
                  max = 2019,
                  step = 1, 
                  value = 2000,
                  sep = ''),
      
      # Input: A simple drop down list  ----
      selectInput(inputId = "country", label = "Country:",
                  choices = c(sort(unique(course_data$Country)))),
      
      # Input: A simple drop down list  ----
      selectInput(inputId = "city", label = "City:",
                  choices = c(sort(unique(course_data$City)))),
      
      # Input: A simple text input  ----
      textInput(inputId = "text_input", label = "Input text here:"),
      
      # Input: A simple radio button input  ----
      radioButtons(inputId = "temp_scale", label = "Temperature scale:",
                   choices = list("Fahrenheit" = "fahrenheit",
                                  "Celsius" = "celsius"),
                   selected = "fahrenheit"),
      
      actionButton(inputId = "button", label="Submit"),
      
      downloadButton(outputId = "download", label = "Download Data")
      
    ),
    
    # Main panel
    mainPanel(
      "This is the main panel",
      
      hr(),
      br(),
      
      textOutput(outputId = "text_output"),
      
      # Layout: Tabset with info, data, and plots tabs ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Info",
                           h3("Info on the Shiny App:"),
                           hr(),
                           "This is the course shiny app. It is created during the course 
                           exercises using the europe.rds data: 
                           Average daily temperatures (in Fahrenheit) from cities around
                           Europe from 2000 to 2019",
                           br(),
                           h3("Summary Statistics of Dataset:"),
                           hr(),
                           verbatimTextOutput("data_summary")
                  ),
                  tabPanel(title = "Data", 
                           h3("Datatable of Selection:"),
                           hr(),
                           br(),
                           dataTableOutput("data_table")
                  ),
                  tabPanel(title = "Plots",
                           fluidRow(
                             h3("Lineplot of daily average temperatures:"),
                             hr(),
                             column(width = 12, plotOutput("lineplot")),
                             #downloadButton("downloadPlot1", "Download Plot")
                           ),
                           br(),
                           fluidRow(
                             h3("Boxplot of months and lineplot of min/max and avg. temperature per month:"),
                             hr(),
                             column(width = 6, plotOutput("boxplot")),
                             #downloadButton("downloadPlot2", "Download Plot"),
                             column(width = 6, plotOutput("lineplotTemp")),
                             #downloadButton("downloadPlot3", "Download Plot")
                           )
                  )
      )
    )
  )
)

# Define server side logic
server <- function(input, output, session) {
  
  output$download <- downloadHandler(
    filename = paste0(input$country,".csv"),
    content = function(file) {
      write.csv(country_df(), file, row.names = FALSE)
    }
    )
  
  country_df <- eventReactive(eventExpr = input$button, valueExpr = {
    course_data %>%
      filter(Year >= input$year) %>% # Subset the rows to keep data more than or equal to a year
      filter(Country == input$country) # Subset the rows to keep a specific country
  })
  
  city_df <- reactive({
    country_df() %>% 
      filter(City == input$city) %>% # Subset the rows for specific City
      filter(Year == input$year) # Subset the rows for specific Year
  })
  
  year_df <- reactive({
    country_df() %>% 
      filter(City == input$city) %>% # Subset the rows for specific City
      filter(Year == input$year) %>%  # Subset the rows for specific Year
      group_by(Country, City, Year, Month) %>% 
      summarise(MinTempF = min(AvgTemperatureF),
                MeanTempF = round(mean(AvgTemperatureF), 1),
                MaxTempF = max(AvgTemperatureF),
                MinTempC = min(AvgTemperatureC),
                MeanTempC = round(mean(AvgTemperatureC), 1),
                MaxTempC = max(AvgTemperatureC)) %>% 
      ungroup()
    
  })
  
  # Output: Render a text output  ----
  output$text_output <- renderText({
    paste("Your inputs are:", input$year, input$country, input$city, input$text_input, input$temp_scale)
  })
  
  # Output: Render a print output  ----
  output$data_summary <- renderPrint({
    summary(course_data)
  })
  
  # Output: Render a (dynamic) table output  ----
  output$data_table <- renderDataTable({
    city_df()
  })
  
  # Output: Render a plot output  ----
  output$lineplot <- renderPlot({
    
    if(input$temp_scale == "fahrenheit"){
      lineplotlarge <- ggplot(data = city_df()) +
        geom_line(mapping = aes(x = Date, y = AvgTemperatureF), size = 1) +
        ylab("Average daily temperatures (in Fahrenheit)")+
        theme_bw()
    }
    
    if(input$temp_scale == "celsius"){
      lineplotlarge <- ggplot(data = city_df()) +
        geom_line(mapping = aes(x = Date, y = AvgTemperatureC), size = 1) +
        ylab("Average daily temperatures (in Celsius)")+
        theme_bw()
    }
    
    return(lineplotlarge)
  })
  
  # Output: Render a plot output  ----
  output$boxplot <- renderPlot({
    
    if(input$temp_scale == "fahrenheit"){
      res1 <- ggplot(data = country_df()) +
        geom_boxplot(mapping = aes(x = Month, y = AvgTemperatureF, group = Year))+
        theme_bw()
    }
    
    if(input$temp_scale == "celsius"){
      res1 <-  ggplot(data = country_df()) +
        geom_boxplot(mapping = aes(x = Month, y = AvgTemperatureC, group = Year))+
        theme_bw()
      
    }
    
    return(res1)
    
  })
  
  # Output: Render a plot output  ----
  output$lineplotTemp <- renderPlot({
    
    if(input$temp_scale == "fahrenheit"){
      res <- ggplot(data = year_df()) +
        geom_line(mapping = aes(x = Month, y = MinTempF), size = 1, colour = "red", linetype = "dotted") +
        geom_line(mapping = aes(x = Month, y = MeanTempF), size = 1, colour = "black") +
        geom_line(mapping = aes(x = Month, y = MaxTempF), size = 1, colour = "red", linetype = "dotted") +
        scale_x_discrete(name = "", limits = month.abb) +
        ylab("Average daily temperatures (in Fahrenheit)")+
        theme_bw()
    }
    
    if(input$temp_scale == "celsius"){
      res <- ggplot(data = year_df()) +
        geom_line(mapping = aes(x = Month, y = MinTempC), size = 1, colour = "red", linetype = "dotted") +
        geom_line(mapping = aes(x = Month, y = MeanTempC), size = 1, colour = "black") +
        geom_line(mapping = aes(x = Month, y = MaxTempC), size = 1, colour = "red", linetype = "dotted") +
        scale_x_discrete(name = "", limits = month.abb) +
        ylab("Average daily temperatures (in Celsius)")+
        theme_bw()
    }
    
    return(res)
    
  })
  
  observe({
    
    new_choices <- unique(course_data$City[course_data$Country == input$country])
    updateSelectInput(session, inputId = "city", choices = new_choices)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


