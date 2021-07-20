#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library (plotly)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Linear Modeling Data"),
    
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
            
            
            actionButton("lmPlot", "GO GO Linear Model"),
            htmlOutput("RSquared"),
            htmlOutput("Slope"),
            htmlOutput("Intercept"),
            tags$hr(), # Horizontal line
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("lmPlot"),
            tableOutput("contents"),
            textOutput("summary"),
            plotlyOutput("plotlyScatterPlot"),
            plotlyOutput("plotlyLinearModel"),
            # htmlOutput("RSquared"),
            # htmlOutput("Slope"),
            # htmlOutput("Intercept")
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
    
    LinearModel <- eventReactive(input$lmPlot, {
        y <- dataInput()$y
        x <- dataInput()$x
        lmPlot <- lm(y ~ x)
        
    })
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y, xlab = "x", ylab = "y", main = "Scatter Plot of CSV Data", pch = 19)
    })
    
    output$lmPlot <- renderPlot({
        y <- dataInput()$y
        x <- dataInput()$x
        lmPlot <- lm(y ~ x)
        plot(dataInput()$x,dataInput()$y, xlab = "x", ylab = "y", main = "Linear Model of CSV Data", pch = 19)
        abline(LinearModel())
    })
    
    output$RSquared <- renderUI({
        str1 <- paste("R", tags$sup(2),":", sep = "")
        str2 <- paste(format(round(summary(LinearModel())$r.squared, 3)))
        HTML(paste(str1, str2))
    })
    
    output$Slope <- renderUI({
        str1 <- paste("Slope:")
        str2 <- paste(format(round(summary(LinearModel())$coefficients[2], 3)))
        HTML(paste(str1, str2))
    })
    
    output$Intercept <- renderUI({
        str1 <- paste("Intercept:")
        str2 <- paste(format(round(summary(LinearModel())$coefficients[1], 3)))
        HTML(paste(str1, str2))
    })
    
    
    output$plotlyScatterPlot <- renderPlotly({
        plot <- plot_ly(dataInput(), x = ~x, y = ~y, type = 'scatter', mode = 'markers')%>%
            layout(title="Scatter Plot")
    })
    
    output$plotlyLinearModel <- renderPlotly({plot <- plot_ly(dataInput(), x = ~x, y = ~y, type = 'scatter', mode = 'markers')%>%
        layout(title="LinearModel")%>% 
    add_trace(dataInput(), x = ~x, y = fitted(LinearModel()), mode = "lines", showlegend = F)})
    
    output$summary <- renderPrint({
        y <- dataInput()$y
        x <- dataInput()$x
        lmPlot <- lm(y ~ x)
        #attributes(summary(lmPlot))
        # summary(lmPlot)$slope
        # summary(lmPlot)$coefficients
        # summary(lmPlot)$r.squared
        
        # 
        # 
        # paste("R2 = ",signif(summary(lmPlot)$adj.r.squared, 5),
        #       "Intercept =",signif(lmPlot$coef[[1]],5 ),
        #       " Slope =",signif(lmPlot$coef[[2]], 5))
        
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