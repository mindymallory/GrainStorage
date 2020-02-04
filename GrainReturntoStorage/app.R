#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Cost and Return of Storing Grain"),
    
    fluidRow(
        
        column(4,
               
               wellPanel(
                   numericInput("num", 
                                h5("Bushels to Store"), 
                                value = 30000),
               sliderInput("price", h5("Cash Grain Price at Harvest $/bu"),
                           min = 0, max = 8.00, value = 4.00, step = .1),
               sliderInput("interest", h5("Short Term Interst Rate %"),
                           min = 2, max = 20, value = 7.00, step = 1)
               ),
               
               wellPanel(
                   h4("On Farm Costs to Storage"),
                   sliderInput("moist", h5("Moisture of Grain %"),
                               min = 10, max = 45, value = 13.5, step = .5),
                   sliderInput("shrink", h5("Shrink Factor %"),
                               min = 0, max = 3, value = 1.5, step = .1),
                   sliderInput("handling", h5("Extra handling cost into and out $/bu"),
                               min = 0, max = .2, value = .02, step = .01),
                   sliderInput("transp", h5("Extra transportation cost $/bu"),
                               min = 0, max = .2, value = .08, step = .01)
               )
               
               )       
        ),
        
        column(8,
               plotOutput("distPlot")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
