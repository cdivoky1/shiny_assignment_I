#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiny App: Modeling CSV Data with Linear Regression"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(    
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            actionButton("go", "Model Data"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents"),
           plotOutput("lmStats"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    modeled_input <- eventReactive(input$go, {
        model = lm(formula = y ~ x, data = dataInput())
        model_summary <- summary(model)
        return(model)
    })
    
    modeled_stats <- eventReactive(input$modelStats, {
        model = lm(formula = y ~ x, data = dataInput())
        model_summary <- summary(model)
        return(model)
    })
    
    
    output$distPlot <- renderPlot({
        ggplot() +
            geom_point(aes(x = dataInput()$x, y = dataInput()$y),
            colour = 'purple') + ggtitle('Values from Input Data') +
            xlab('X Values in uploaded data file') +
            ylab('Y Values in uploaded data file')
    })
    
    output$lmPlot <- renderPlot({
        summary_model <- summary(modeled_input())
        lm_stats <- paste("y=", summary_model$coefficients[2,1], "x+", summary_model$coefficients[1,1], 
                            "\nr2=", summary_model$adj.r.squared)
        ggplot() +
            geom_point(aes(x = dataInput()$x, y = dataInput()$y), colour = 'red') +
            geom_line(aes(x = dataInput()$x, y = predict(modeled_input(), newdata = dataInput())),
            colour = 'blue') +
            ggtitle(lm_stats) +
            xlab('X Values in uploaded data file') +
            ylab('Y Values in uploaded data file')
    })
    
    
    output$contents <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }  
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
