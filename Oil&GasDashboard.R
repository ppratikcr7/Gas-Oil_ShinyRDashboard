# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.
# Install packages using the below commands:
# install.packages("shiny")
# install.packages("plotly")
# install.packages("shinydashboard")
# install.packages("tidyverse")
# install.packages("DT")
# install.packages("shinyjs")
# Required libraries:
library(shiny)
library(plotly)
library(shinydashboard)
library(DT)
library(tidyverse)
library(shinyjs)

# Dashboard UI items:
header <- dashboardHeader(title = "Gas & Oil Profiles")

sidebar <- dashboardSidebar(
  sidebarMenu(
    img(src = "logo.png", height = 130, width = 230),
    selectInput("GasorOilProfile","Profile:",c("Gas Profile", "Oil Profile"), selected = "Gas Profile"),
    hr(),
    # Selector for file upload (import data in csv format)
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    hr(),
    "Display Squeezed CRX plot:",
    textInput("crx_value", label = h4("CRX limit:"), value = 600),
    actionButton("squeezedplot", "Squeezed Profile"),
    tags$style(type="text/css", "#downloadSqueezedCsvButton {background-color:white;color: black;font-family: Courier New}"),
    downloadButton("downloadSqueezedCsvButton", 'Download Squeezed CSV', class = "button1"),
    actionButton("do", 'save', icon = icon('play-circle')),
    hr(),
    selectInput("DataPlot","Plot Type:",c("MainPlot", "SingleFieldPlot"), selected = "MainPlot"),
    #These column selectors are dynamically created when the file is loaded
    uiOutput("fromCol"),
    uiOutput("toCol"),
    uiOutput("Fieldflag"),
    #The conditional panel is triggered by the preceeding checkbox
    conditionalPanel(
      condition="input.Fieldflag==true",
      uiOutput("FieldCol")
    ),
    hr(),
    "Delete a trace from the plot:",
    uiOutput("DeleteFieldCol"),
    actionButton("deleteTrace", "Delete Trace")
  )
)

body <- dashboardBody(
  tags$head(tags$style(".sidebar-menu li { margin-bottom: 10px; }")),
  fluidPage(
    # Application title
    titlePanel("Oil and Gas Profile Dashboard"),
    # tabs UI
    tabsetPanel(id="tabs",
                tabPanel("Landing Dashboard", value="1", br(), br(), DTOutput(outputId = "table"), br(), br(), textOutput("text0"), br(), plotlyOutput("gasPlotStatic", height = 300)),
                #tabPanel("Gas Profile", value="2", br(), textOutput("text0"), br(), plotlyOutput("gasPlotStatic", height = 300)),
                tabPanel("Squeezed Profile", value="2", br(), textOutput("text1"), br(), plotlyOutput("gasPlotSqueezed", height = 300))
                #tabPanel("Oil Profile", value="4", plotlyOutput("OilPlot"))
    ) ,  style='width: 1300px; height: 1250px'
  )
)

# Define UI for application dashboard:
ui <- dashboardPage(
  header,
  sidebar,
  body,
  useShinyjs() # Set up shinyjs
)

# Define server logic required to draw a plot
server <- function(input, output, session){
  # Side Panel Functionalities:
  
  # Data for normal table on landing page:
  filedata1 <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    # Preparing a new dataframe
    df <- read.csv(infile$datapath);
    click("do")
    return(df)
  })
  
  # Data for squeezed CRX plot:
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    # Preparing a new dataframe
    df <- read.csv(infile$datapath);
    df1 <- df;
    df <- df  %>% select(-Fields) %>% select(-Rank)
    
    # Logic for Squeezing below CRX value:
    
    # first finding the sum across rows:
    total_col = apply(df[,-1], 1, sum);
    
    # fetch the order:
    colOrderFinal <- NA
    colOrder <- df1[!(is.na(df1$Rank) | df1$Rank==""),ncol(df1)];
    colNames1 <- colnames(df1 %>% select(-Year) %>% select(-Fields) %>% select(-Rank))
    # Arrange ColNames in order:
    count = 1
    for (i in colOrder) {
      colOrderFinal[i] <- colNames1[count];
      count = count + 1;
    }
    count_reset <- count;
    col_num <- NA
    # recursive function to check iteratively if all values are below CRX value:
    squeezed_df <- function(df,total_col, count, j){
      validate(need(!is.na(as.numeric(input$crx_value)),"Please enter proper CRX value!"));
      crx <- input$crx_value;
      for (i in total_col) {
        cnt <- count;
        if( j <= 4){
          crx <- '300';
          #browser();
        }
        else{
          crx <- input$crx_value;
          #browser();
        }
        total_col_for_yr_j <- i;
        if(i >= as.numeric(crx)){
          extra_val = total_col_for_yr_j - as.numeric(crx);
          while(total_col_for_yr_j > as.numeric(crx)){
            # deduction from lowest priority first
            col_num <- which(colNames1 == colOrderFinal[cnt-1])
            #print(col_num)
            count  <- count - 1;
            cnt <- cnt-1;
            # total_col_for_yr_j = 700
            # CRX 600
            # Case 1: df[j,col_num+1] = 150, extra_val 100
            # Case 2: df[j,col_num+1] = 40, extra_val 100, remain_val = 60
            # Case 3: df[j,col_num+1] = 100, extra_val 100
            # Case 1:
            validate(need(!is.na(df[j,col_num+1] - extra_val),"Please enter proper CRX value!"));
            if(df[j,col_num+1] - extra_val > 0){
              df[j, col_num+1] <- df[j,col_num+1] - extra_val;
              total_col_for_yr_j <- apply(df[j,-1], 1, sum);
              if(cnt == 1){
                cnt <- count_reset;
                count <- count_reset;
              }
              next;
            }
            
            # Case 2:
            else if(df[j,col_num+1] - extra_val < 0){
              extra_val = extra_val - df[j,col_num+1];
              df[j,col_num+1] = 0;
            }
            else{
              df[j, col_num+1] <- df[j,col_num+1] - extra_val;
            }
            total_col_for_yr_j <- apply(df[j,-1], 1, sum);
            
            if(cnt == 1){
              cnt <- count_reset;
              count <- count_reset;
            }
          }
        }
        else {
          cnt <- count_reset;
          count <- count_reset;
        }
        cnt <- count_reset;
        count <- count_reset;
        j <- j + 1;
      }
      # Again find the total after x% deduction
      total_col <- apply(df[,-1], 1, sum);
      click("do");
      if(length(which(total_col[1:4] > 300)) == 0 & length(which(total_col[5:count_reset-1] > as.numeric(input$crx_value))) == 0) {
        return(df)
      } else {
        return(squeezed_df(df,total_col, count, j))
      }
    }
    # Calling the squeezing function:
    j <- 1;
    df <- squeezed_df(df,total_col, count, j);
    return(df)
  })
  
  ####### Main Gas Plot functionalities: #####
  
  # Realtime plot for gas profile as per user's selection:
  data <- reactive({
    
    x <- filedata1() %>% 
      filter(between(as.integer(Year), as.integer(input$from), as.integer(input$to)));
    x <- x[, c("Year", input$field)]
    return(x)
  })
  
  output$text0 <- renderText({
    "Main CRX Plot"
  })
  
  ###### Functions for plots: ##########
  
  #Realtime gas plot:
  rtgasplot <- function(df){
    #browser();
    p <- plot_ly(df, x = ~Year, y = ~get(input$field), name = as.character(input$field), type = 'scatter', mode = 'markers+lines', stackgroup = 'one', fillcolor = '#4C74C9', showlegend = TRUE) %>%
      layout(shapes=list(
        list(type='line', x0= min(df$Year), x1= 2022, y0=300, y1=300, line=list(dash='dot', width=2)),
        list(type='line', x0= 2022, x1= 2023, y0=300, y1=input$crx_value, line=list(dash='dot', width=2)),
        list(type='line', x0= 2023, x1= max(df$Year), y0=input$crx_value, y1=input$crx_value, line=list(dash='dot', width=2))
      ),
      title = 'CRX Supply',
      xaxis = list(title = "Year", showgrid = TRUE, showline = TRUE),
      yaxis = list(title = "Fields", showgrid = TRUE, showline = TRUE))
    return(p)
  }
  
  #Main Gas plot:
  maingasplot <- function(df, trace1, trace1_name, colNames){
    p <- plot_ly(df, x = ~Year, y = trace1, name = trace1_name, type = 'scatter', mode = 'markers+lines', stackgroup = 'one', fillcolor = '#00fbff')
    color_list = c( '#eb8705' , '#fcc203', '#fc0314', '#eb34e2', '#522c18', '#022beb', '#fcba03', '#50CB86', '#4f23a1', '#42f587', '#d12c7c','#3434eb', '#e60206', '#edb707');
    i = 1;
    for(trace in colNames){
      p <- p %>% add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace, fillcolor = color_list[i])
      i = i+1;
    }
    p <- p %>% layout(shapes=list(
      list(type='line', x0= min(df$Year), x1= 2022, y0=300, y1=300, line=list(dash='dot', width=2)),
      list(type='line', x0= 2022, x1= 2023, y0=300, y1=input$crx_value, line=list(dash='dot', width=2)),
      list(type='line', x0= 2023, x1= max(df$Year), y0=input$crx_value, y1=input$crx_value, line=list(dash='dot', width=2))
    ),
    title = 'CRX Supply',
    xaxis = list(title = "Year", showgrid = TRUE, showline = TRUE),
    yaxis = list(title = "Fields", showgrid = TRUE, showline = TRUE
    ))
    return(p)
  }
  
  # Output Static Plot for Gas or Oil profile (Both MainPlot and user selections based RealTime Plot):
  output$gasPlotStatic <- renderPlotly({
    if(input$DataPlot == "SingleFieldPlot"){
      # Single Field Plot
      df <- data()
      validate(
        need( nrow(df) > 0, "Data insufficient for plot, please select the year range and a field from the sidepanel")
      )
      p <- rtgasplot(df);
    }
    else{
      # Main Plot (This plot can be changed by user editing in the landing tab table and clicking on save button.)
      df <- filedata1();
      validate(
        need( nrow(df) > 0, "Please upload the CSV to get the plot")
      );
      # fetch the order:
      colNames <- NA
      colOrder <- df[!(is.na(df$Rank) | df$Rank==""),ncol(df)];
      colNames1 <- colnames(df %>% select(-Year)  %>% select(-Fields) %>% select(-Rank))
      
      # Arrange ColNames in order:
      count = 1
      for (i in colOrder) {
        colNames[i] <- colNames1[count];
        count = count + 1;
      }
      trace1_name <- colNames[1]
      
      # we place first trace in plotly first line
      colNames <- colNames[-1];
      trace1 = as.formula(paste0("~`", trace1_name, "`"))
      p <- maingasplot(df, trace1, trace1_name, colNames);
    }
  })
  
  ######## Squeezed Plot functionalities:  #########
  output$text1 <- renderText({
    "Squeezed CRX Plot"
  })
  
  observeEvent(input$squeezedplot, {
    # Squeezed Static Plot:
    output$gasPlotSqueezed <- renderPlotly({
      if(input$DataPlot == "SingleFieldPlot"){
        # Real time squeezed Plot for single user selected field:
        df <- data()
        validate(
          need( nrow(df) > 0, "Data insufficient for plot, please select the year range and a field from the sidepanel")
        )
        p <- rtgasplot(df);
      }
      else{
        # squeezed Plot for all fields:
        df <- filedata();
        df1 <- filedata1();
        validate(
          need( nrow(df) > 0, "Please upload the CSV and click 'Squeezed Gas Profile' button to get the plot")
        );
        # fetch the order:
        colNames <- NA
        colOrder <- df1[!(is.na(df1$Rank) | df1$Rank==""),ncol(df1)];
        colNames1 <- colnames(df %>% select(-Year))
        
        # Arrange ColNames in order:
        count = 1
        for (i in colOrder) {
          colNames[i] <- colNames1[count];
          count = count + 1;
        }
        trace1_name <- colNames[1]
        # we place first trace in plotly first line
        colNames <- colNames[-1];
        trace1 = as.formula(paste0("~`", trace1_name, "`"))
        p <- maingasplot(df, trace1, trace1_name, colNames);
      }
    })
  })
  
  ### Download Squeezed Data ###
  output$downloadSqueezedCsvButton <- downloadHandler(
    filename = function() {
      paste(Sys.time(), '_data_squeezed.csv', sep='')
    },
    content = function(file) {
      write.csv(filedata(), file, row.names = FALSE)
    }
  )
  
  ######### Landing Tab functionalities: #############################
  output$table <- renderDataTable(server = FALSE, {
    df <- filedata1();
    validate(
      need( nrow(df) > 0, "Please upload the CSV to view the Data Table")
    );
    DT::datatable(df, editable = "all", rownames = FALSE, filter = "top", # location of column filters,
                  extensions = "Buttons", options = list(fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE,
                                                         scrollX = TRUE, # allow user to scroll wide tables horizontally
                                                         dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf'), pageLength = 10,lengthMenu = list(c(10,30, 50, 100, -1), list('10', '30', '50','100', 'All')), paging = T)
    )
  }) 
  
  # hide save button:
  observe({ toggle(id="do", condition=!is.null(input$location))})
  
  ###Tracking Changes###
  rvs <- reactiveValues(
    data = NULL #dynamic data object
  )
  
  observe({
    rvs$data <- filedata1()
  })
  
  #proxy = dataTableProxy('table')
  observe({
    tryCatch({
      replaceData(rvs$data, rownames = FALSE, resetPaging = FALSE)
    }, error = function(e) {
    }
    )
  })
  
  observeEvent(input$table_cell_edit, {
    tryCatch({
      rvs$data <<- editData(rvs$data, input$table_cell_edit, rownames = FALSE)
      click("do")
    }, error = function(e) {
      print("Please provide all different ranks.");
    }
    )
  })
  
  ####### DeleteTrace functionality:  #######
  output$DeleteFieldCol <- renderUI({
    df <-filedata1()
    if (is.null(df)) return(NULL)
    # Let's only show field names
    items= colnames(df %>% select(-Year)  %>% select(-Fields) %>% select(-Rank))
    selectInput("delfield", "Field to Delete:",choices=items)
  })
  
  # Track delete trace button click:
  observeEvent(
    input$deleteTrace,{
      
      output$gasPlotStatic <- renderPlotly({
        df <- filedata1()
        # fetch the order:
        colNames <- NA
        colOrder <- df[!(is.na(df$Rank) | df$Rank==""),ncol(df)];
        colNames1 <- colnames(df %>% select(-Year)  %>% select(-Fields) %>% select(-Rank))
        
        # Arrange ColNames in order:
        count = 1
        for (i in colOrder) {
          colNames[i] <- colNames1[count];
          count = count + 1;
        }
        colNames <- colNames[-which(colNames == input$delfield)]
        trace1_name <- colNames[1]
        colNames <- colNames[-1]
        trace1 = as.formula(paste0("~`", trace1_name, "`"))
        p <- maingasplot(df, trace1, trace1_name, colNames);
      })
    })
  
  #### RealTime Plot functionality options ########
  # The following set of functions populate the column selectors
  output$fromCol <- renderUI({
    df <-filedata1()
    if (is.null(df)) return(NULL)
    items= df[,1]
    selectInput("from", "From Year:",choices= items)
  })
  
  output$toCol <- renderUI({
    df <-filedata1()
    if (is.null(df)) return(NULL)
    items= df[,1]
    selectInput("to", "To Year:",items)
  })
  
  #The checkbox selector is used to determine whether we want an optional column Field
  output$Fieldflag <- renderUI({
    checkboxInput("Fieldflag", "Do you want to select specific field?", FALSE)
  })
  
  #If we do want the optional column, this is where it gets created
  output$FieldCol <- renderUI({
    df <-filedata1()
    if (is.null(df)) return(NULL)
    #Let's only show field names
    items= colnames(df %>% select(-Year)  %>% select(-Fields) %>% select(-Rank))
    selectInput("field", "Field:",choices=items)
  })
  
  #### Save editable table values and populate plots: #########
  observeEvent(
    input$do,{
      reacdata <- rvs$data
      # Output Static Plot for Gas profile (Both MainPLot and user selections based RealTime Plot):
      output$gasPlotStatic <- renderPlotly({
        if(input$DataPlot == "SingleFieldPlot"){
          # Real time plot
          df <- data()
          validate(
            need( nrow(df) > 0, "Data insufficient for plot, please select the year range and a field from the sidepanel")
          )
          p <- rtgasplot(df);
        }
        else{
          # Main Plot (This plot can be changed by user editing in the landing tab table and clicking on save button.)
          df <- reacdata
          # fetch the order:
          colNames <- NA
          colOrder <- df[!(is.na(df$Rank) | df$Rank==""),ncol(df)];
          colNames1 <- colnames(df %>% select(-Year)  %>% select(-Fields) %>% select(-Rank))
          
          # Arrange ColNames in order:
          count = 1
          for (i in colOrder) {
            colNames[i] <- colNames1[count];
            count = count + 1;
          }
          trace1_name <- colNames[1]
          # we place first trace in plotly first line
          colNames <- colNames[-1];
          trace1 = as.formula(paste0("~`", trace1_name, "`"))
          p <- maingasplot(df, trace1, trace1_name, colNames);
        }
      })
    })
}
# Run the application:
shinyApp(ui = ui, server = server)