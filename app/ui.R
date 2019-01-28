## Needed Lib's
library(shiny)
library(plotly)

## Start the UI renderer
shinyUI(
  pageWithSidebar(
    
    ## The TITLE!
    headerPanel("Finding Correlation"),
    
    ## This is where the up-loader and drop downs live
    sidebarPanel(
      fileInput('datafile', 
                'Choose Excel file'),
      uiOutput("date"),
      uiOutput("varselect1"),
      uiOutput("varselect2"),
      ## The description
      h2("Description"),
      p("This tool's main functionality is to find is there is a correlation between two variables. The file that is being uploaded
        should only be the data that you want to be correlated. Pick your two variables, via the drop downs."),
      ## Instructions for using this app
      h2("Instructions"),
      p("Please follow these instructions to find your correlation."),
      p("1. Upload a data file (.xlsx or .xls) that will be used for making your groups.
        The first row in the file should be the column headers of the data. 
        The first column in the file should be where your data starts.
        THERE SHOULD BE NO 'BUFFERS' AROUND YOUR DATA FILE, i.e. empty rows and columns."),
      p("2. Pick the variables you want to correlate via the drop-downs."),
      p("3. If on the 'Correlation with Adstock' tab, select your Adstock rate with the slider.")
      ),
    
    ## The main panel where all the shit happens 
    mainPanel(
      ## Make it so errors don't show in app, only in logs
      tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }"
      ),
      ## Tabs
      textOutput("text"),
      tabsetPanel(id = 'tabChoice', 
                  tabPanel("Correlation", value = 'Correlation'
                           ),
                  tabPanel("Correlation with Adstock", value = 'Correlation with Adstock',
                           h2("Adstock"),
                           p("Adstock is the prolonged or lagged effect of advertising on consumer purchase behavior. 
                             Exposure to Television Advertising builds awareness in consumer markets, resulting in sales. 
                             Each new exposure to advertising increases awareness to a new level and this awareness will
                             be higher if there have been recent exposures and lower if there have not been. This is the 
                             decay effect of Adstock and this decay eventually reduces awareness to its base level, unless
                             or until this decay is reduced by new exposures."),
                           sliderInput("adstock", "Adstock Rate %:",
                                       min = 0, max = 1,
                                       value = 0.5, step = 0.025))
                  ),
      ## The correlation
      h2("Is there correlation here?"),
      # The output of correlation
      uiOutput("correlation_result"),
      # The output of sig test
      uiOutput("sig_testing_result"),
      ## The plot instructions 
      h2("Visualizing Your Data"),
      # Plotly function that links the UI to the Server
      plotlyOutput('plot_data', height = 500),
      # The correlation plot
      plotlyOutput('correlation_plot', height = 500)
      )
    )
  )

