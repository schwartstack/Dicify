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
library(imager)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("Turn any image into dice!"),
    
    #number input form
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Choose a file", accept = c("jpg","png","bmp")),
            textInput("numDice", "Maximum number of dice", value = "400")
        ),
        
        # Show result
        mainPanel(
            
            plotOutput("plot")
            
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input,output,session) {
    #observe the add click and perform a reactive expression
    observeEvent( input$file,{
        img = load.image(input$file$datapath)
        dims = dim(img)
        scaler = sqrt(parse_number(input$numDice)/(dims[1]*dims[2]))
        print(dims)
        print(scaler)
        output$plot <- renderPlot({
            par(mar=c(0,0,0,0))
            par(oma=c(0,0,0,0))
            #par(omd=c(0,0,0,0))
            img %>%
                dicify(round(scaler*dims[1]),scaler*dims[2]) %>%
                plot(axes = F)
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)