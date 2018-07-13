library(shiny)
library(tidyverse)

ui <- fluidPage(
        headerPanel("Shiny Elicitor: Four Point Expert Elicitation Visualiser"),
        sidebarPanel(
                fileInput("file", label = h3("File input")),
                helpText("Default max. file size is 5MB", "Input File should contain the column names: 'group', 'upper', 'best', 'lower', 'confidence'"),
                tags$hr(),
                h5(helpText("Select the read.table parameters below")),
                checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                br(),
                radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')),
        
        mainPanel(
                uiOutput("tb")
                  )
)

server <- function(input, output, session){
        # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
        # file$datapath -> gives the path of the file
        data <- reactive({
                file1 <- input$file
                if(is.null(file1)){return()} 
                read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
                
        })
        
        # this reactive output contains the summary of the dataset and display the summary in table format
        output$filedf <- renderTable({
                if(is.null(data())){return ()}
                input$file
        })        
        
        # This reactive output contains the dataset and display the dataset in table format
        output$table <- renderTable({
                if(is.null(data())){return ()}
                data()
        })
        
        # This reactive output contains the ggplot and displays the plot
        output$plot1 <- renderPlot({
                if(is.null(data())){return()}
                ggplot(data(), aes(x = group, y = best, colour = group)) +
                        geom_linerange(aes(ymin =lower, ymax = upper)) +
                        geom_pointrange(aes(ymin = lower, ymax = upper)) +
                        geom_label(aes(label = confidence)) +
                        labs(y = "Elevation") +
                        theme_bw() +
                        theme(legend.position = "none")
        })
        
        output$tb <- renderUI({
                if(is.null(data()))
                        h5("Powered by", tags$img(src='qaeco_img.png', heigth=200, width=200))
                else
                        tabsetPanel(tabPanel("Data", plotOutput("plot1"), tableOutput("table")),
                                    tabPanel("About file", 
                                             tableOutput("filedf"))
                                    ) 
        })
}

shinyApp(ui, server)


