## Needed Lib's
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(plotly)

# Set max upload limit to 100MB
options(shiny.maxRequestSize=2048*(1024**2))

shinyServer(
  function(input, output, session) {
    
    # Date 
    output$date <- renderUI({
      selectInput("date", label = "Dates Column:",
                  choices = names(dataset()), selected = names(dataset())[1])  
    })
    # Variable #1
    output$varselect1 <- renderUI({
      selectInput("var1", label = "TV Impressions Column:",
                  choices = names(dataset()), selected = names(dataset())[2])  
    })
    # Variable #2
    output$varselect2 <- renderUI({
      selectInput("var2", label = "Search Impressions Column:",
                  choices = names(dataset()), selected = names(dataset())[3])  
    })
    
    ## Read in the data
    dataset <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        return(NULL)
      }
      else {read_excel(infile$datapath)}
    })
    
    ## Calculate the Adstock 
    compute_ad_stock <- reactive({
      # Define Adstock Function
      adstock <- function(x){
        # The Adstock rate
        adstock_rate <- input$adstock
        # Grab length of variable that we are applying this to 
        adstocked_advertising <- numeric(length(x))
        # Grab the first instance of the variablw
        adstocked_advertising[1] <- x[1]
        # Iterate and append to adstocked_advertising
        for(i in 2:length(x)){
          adstocked_advertising[i] <- x[i] + adstock_rate * adstocked_advertising[i-1]
        }
        
        return(adstocked_advertising)
      }
      # Bring in the data 
      data <- dataset()
      TV_var <- data[[input$var1]]
      # Calculate the add stock
      data$TV_adstock <- adstock(TV_var)
      
      return(data)
    })
    
    ## Correlation Plot
    output$plot_data <- renderPlotly({
      if (input$tabChoice == 'Correlation') {
      # Bring in the data 
      data <- subset(dataset(), select = c(input$date, input$var1, input$var2))
      date <- data[[input$date]]
      y_var1 <- data[[input$var1]]
      y_var2 <- data[[input$var2]]
      
      plot <- ggplot(data, aes(date)) + 
                     geom_line(aes(y = y_var1, 
                                   colour = input$var1)) + 
                     geom_line(aes(y = y_var2, 
                                   colour = input$var2)) +
                     ggtitle("Data Over Time") +
                     labs(x = input$date, y = "TV & Search Impressions") +
                     scale_colour_discrete("Variables")
      # Use Plotly for interactivity
      plotly_plot <- ggplotly(plot)
      
      return(plotly_plot)
      }
      else if (input$tabChoice == 'Correlation with Adstock') {
        data <- compute_ad_stock()
        date <- data[[input$date]]
        y_var2 <- data[[input$var2]]
        
        plot <- ggplot(data, aes(date)) + 
          geom_line(aes(y = TV_adstock)) + 
          geom_line(aes(y = y_var2, 
                        colour = input$var2)) +
          ggtitle("Data Over Time") +
          labs(x = input$date, y = "TV Adstock & Search Impressions") +
          scale_colour_discrete("Variables")
        # Use Plotly for interactivity
        plotly_plot <- ggplotly(plot)
        
        return(plotly_plot)
      }
    })
    
    ## Correlation plot
    output$correlation_plot <- renderPlotly({
      if (input$tabChoice == 'Correlation') {
        # Bring in the data 
        data <- subset(dataset(), select = c(input$var1, input$var2))
        y_var <- data[[input$var1]]
        x_var <- data[[input$var2]]
        
        plot <- ggplot(data = data, 
                       aes(x = x_var, 
                           y = y_var)) +
                       geom_point(size = 3, colour = "orange") +
                       geom_smooth(method = "lm") +
                       ggtitle("Correlation Plot") +
                       labs(x = input$var2, y = input$var1)
        # Use Plotly for interactivity
        plotly_plot <- ggplotly(plot)
        
        return(plotly_plot)
      }
      else if (input$tabChoice == 'Correlation with Adstock') {
        # Bring in the data 
        data <- compute_ad_stock()
        x_var <- data[[input$var2]]
        
        plot <- ggplot(data = data, 
                       aes(x = x_var, 
                           y = TV_adstock)) +
          geom_point(size = 3, colour = "orange") +
          geom_smooth(method = "lm") +
          ggtitle("Correlation Plot") +
          labs(x = input$var2, y = 'TV_adstock')
        # Use Plotly for interactivity
        plotly_plot <- ggplotly(plot)
        
        return(plotly_plot)
      }
    })
    
    ## The output
    output$correlation_result <- renderUI({
      if (input$tabChoice == 'Correlation') {
        # Bring in the data 
        data <- subset(dataset(), select = c(input$var1, input$var2))
        # Compute the correlation between the two variables
        correlate_data <- cor.test(data[[input$var1]], data[[input$var2]])$estimate
        if (correlate_data == 0) {
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>NO Correlation Here!</strong>
                      </h4> 
                      <p>
                      There is no relationship between the two variables.
                      </p>"))
        }
        else if ((correlate_data >= 0.1) & (correlate_data <= 0.3)) {
          return(HTML("<h4 style='color:#008000'>
                      <strong>Positive Correlation - Weak</strong>
                      </h4> 
                      <p>
                      There is weak relationship between the two variables. As TV Impressions increases, Search Impressions increases, 
                      and conversely, as TV Impressions decreases, Search Impressions decreases. In other words, the 
                      variables move in the same direction when there is a positive correlation.
                      </p>"))
        }
        else if ((correlate_data >= 0.4) & (correlate_data <= 0.6)) {
          return(HTML("<h4 style='color:#008000'>
                      <strong>Positive Correlation - Moderate</strong>
                      </h4> 
                      <p>
                      There is moderate relationship between the two variables. As TV Impressions increases, Search Impressions increases, 
                      and conversely, as TV Impressions decreases, Search Impressions decreases. In other words, the 
                      variables move in the same direction when there is a positive correlation.
                      </p>"))
        }
        else if ((correlate_data >= 0.7) & (correlate_data <= 0.9)) {
          return(HTML("<h4 style='color:#008000'>
                      <strong>Positive Correlation - Strong</strong>
                      </h4> 
                      <p>
                      There is strong relationship between the two variables. As TV Impressions increases, Search Impressions increases, 
                      and conversely, as TV Impressions decreases, Search Impressions decreases. In other words, the 
                      variables move in the same direction when there is a positive correlation.
                      </p>"))
        }
        else if ((correlate_data >= 0.9) & (correlate_data <= 1)) {
          return(HTML("<h4 style='color:#008000'>
                      <strong>Positive Correlation - Perfect</strong>
                      </h4> 
                      <p>
                      There is almost perfect relationship between the two variables. As TV Impressions increases, Search Impressions increases, 
                      and conversely, as TV Impressions decreases, Search Impressions decreases. In other words, the 
                      variables move in the same direction when there is a positive correlation.
                      </p>"))
        }
        else if ((correlate_data >= -0.1) & (correlate_data <= -0.3)) {
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>Negative Correlation - Weak</strong>
                      </h4> 
                      <p>
                      There is weak relationship between the two variables. As TV Impressions increases, Search Impressions decreases, 
                      and conversely, as TV Impressions decreases, Search Impressions increase. In other words, the 
                      variables move in the opposite direction when there is a negative correlation.
                      </p>"))
        }
        else if ((correlate_data >= -0.4) & (correlate_data <= -0.6)) {
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>Negative Correlation - Moderate</strong>
                      </h4> 
                      <p>
                      There is moderate  relationship between the two variables. As TV Impressions increases, Search Impressions decreases, 
                      and conversely, as TV Impressions decreases, Search Impressions increase. In other words, the 
                      variables move in the opposite direction when there is a negative correlation.
                      </p>"))
        }
        else if ((correlate_data >= -0.7) & (correlate_data <= -0.9)) {
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>Negative Correlation - Strong</strong>
                      </h4> 
                      <p>
                      There is strong  relationship between the two variables. As TV Impressions increases, Search Impressions decreases, 
                      and conversely, as TV Impressions decreases, Search Impressions increase. In other words, the 
                      variables move in the opposite direction when there is a negative correlation.
                      </p>"))
        }
        else if ((correlate_data >= -0.9) & (correlate_data <= -1)) {
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>Negative Correlation - Perfect</strong>
                      </h4> 
                      <p>
                      There is almost perfect relationship between the two variables. As TV Impressions increases, Search Impressions decreases, 
                      and conversely, as TV Impressions decreases, Search Impressions increase. In other words, the 
                      variables move in the opposite direction when there is a negative correlation.
                      </p>"))
        }
      }
      else if (input$tabChoice == 'Correlation with Adstock') {
        # Bring in the data 
        data <- compute_ad_stock()
        # Compute the correlation between the two variables
        correlate_data <- cor.test(data$TV_adstock, data[[input$var2]])$estimate
        if (correlate_data == 0) {
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>NO Correlation Here!</strong>
                      </h4> 
                      <p>
                      There is no relationship between the two variables.
                      </p>"))
        }
        else if ((correlate_data >= 0.1) & (correlate_data <= 0.3)) {
          return(HTML("<h4 style='color:#008000'>
                      <strong>Positive Correlation - Weak</strong>
                      </h4> 
                      <p>
                      There is weak  relationship between the two variables. As TV Impressions increases, Search Impressions increases, 
                      and conversely, as TV Impressions decreases, Search Impressions decreases. In other words, the 
                      variables move in the same direction when there is a positive correlation.
                      </p>"))
        }
        else if ((correlate_data >= 0.4) & (correlate_data <= 0.6)) {
          return(HTML("<h4 style='color:#008000'>
                      <strong>Positive Correlation - Moderate</strong>
                      </h4> 
                      <p>
                      There is moderate relationship between the two variables. As TV Impressions increases, Search Impressions increases, 
                      and conversely, as TV Impressions decreases, Search Impressions decreases. In other words, the 
                      variables move in the same direction when there is a positive correlation.
                      </p>"))
        }
        else if ((correlate_data >= 0.7) & (correlate_data <= 0.9)) {
          return(HTML("<h4 style='color:#008000'>
                      <strong>Positive Correlation - Strong</strong>
                      </h4> 
                      <p>
                      There is strong relationship between the two variables. As TV Impressions increases, Search Impressions increases, 
                      and conversely, as TV Impressions decreases, Search Impressions decreases. In other words, the 
                      variables move in the same direction when there is a positive correlation.
                      </p>"))
        }
        else if ((correlate_data >= 0.9) & (correlate_data <= 1)) {
          return(HTML("<h4 style='color:#008000'>
                      <strong>Positive Correlation - Strong</strong>
                      </h4> 
                      <p>
                      There is almost perfect relationship between the two variables. As TV Impressions increases, Search Impressions increases, 
                      and conversely, as TV Impressions decreases, Search Impressions decreases. In other words, the 
                      variables move in the same direction when there is a positive correlation.
                      </p>"))
        }
        else if ((correlate_data >= -0.1) & (correlate_data <= -0.3)) {
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>Negative Correlation - Weak</strong>
                      </h4> 
                      <p>
                      There is weak relationship between the two variables. As TV Impressions increases, Search Impressions decreases, 
                      and conversely, as TV Impressions decreases, Search Impressions increase. In other words, the 
                      variables move in the opposite direction when there is a negative correlation.
                      </p>"))
        }
        else if ((correlate_data > -0.4) & (correlate_data <= -0.6)) {
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>Negative Correlation - Moderate</strong>
                      </h4> 
                      <p>
                      There is moderate  relationship between the two variables. As TV Impressions increases, Search Impressions decreases, 
                      and conversely, as TV Impressions decreases, Search Impressions increase. In other words, the 
                      variables move in the opposite direction when there is a negative correlation.
                      </p>"))
        }
        else if ((correlate_data > -0.7) & (correlate_data <= -0.9)) {
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>Negative Correlation - Strong</strong>
                      </h4> 
                      <p>
                      There is strong  relationship between the two variables. As TV Impressions increases, Search Impressions decreases, 
                      and conversely, as TV Impressions decreases, Search Impressions increase. In other words, the 
                      variables move in the opposite direction when there is a negative correlation.
                      </p>"))
        }
        else if ((correlate_data > -0.9) & (correlate_data <= -1)) {
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>Negative Correlation - Perfect</strong>
                      </h4> 
                      <p>
                      There is almost perfect relationship between the two variables. As TV Impressions increases, Search Impressions decreases, 
                      and conversely, as TV Impressions decreases, Search Impressions increase. In other words, the 
                      variables move in the opposite direction when there is a negative correlation.
                      </p>"))
        }
      }
    })
    
    output$sig_testing_result <- renderUI({
      if (input$tabChoice == 'Correlation') {
        # Bring in the data 
        data <- subset(dataset(), select = c(input$var1, input$var2))
        # Compute the correlation between the two variables
        correlate_data <- cor.test(data[[input$var1]], data[[input$var2]])$p.value
        if (correlate_data <= 0.05) {
          return(HTML("<h4 style='color:#008000'>
                      <strong>This is real!</strong>
                      </h4> 
                      <p>
                      The relationship here is statistically significant. That is,
                      the relationship between these two variables is true. There is a statistically significant 
                      relationship between TV Impressions and Search Impressions, such that the probability of this 
                      correlation occurring by chance is less than five times out of 100.
                      </p>"))
        }
        else{
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>This is NOT real!</strong>
                      </h4> 
                      <p>
                      The relationship here is not statistically significant. 
                      The relationship between these two variables is due to random chance.
                      </p>"))
        }
      }
      else if (input$tabChoice == 'Correlation with Adstock') {
        # Bring in the data 
        data <- compute_ad_stock()
        # Compute the correlation between the two variables
        correlate_data <- cor.test(data$TV_adstock, data[[input$var2]])$p.value
        if (correlate_data <= 0.05) {
          return(HTML("<h4 style='color:#008000'>
                      <strong>This is real!</strong>
                      </h4> 
                      <p>
                      The relationship here is statistically significant. That is,
                      the relationship between these two variables is true. There is a statistically significant 
                      relationship between TV Impressions and Search Impressions, such that the probability of this 
                      correlation occurring by chance is less than five times out of 100.
                      </p>"))
        }
        else{
          return(HTML("<h4 style='color:#F20D06'>
                      <strong>This is NOT real!</strong>
                      </h4> 
                      <p>
                      The relationship here is not statistically significant. 
                      The relationship between these two variables is due to random chance.
                      </p>"))
        }
      }
    })
})
