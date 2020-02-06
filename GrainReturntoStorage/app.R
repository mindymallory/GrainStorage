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
library(tidyr)
library(dplyr)
library(shinythemes)
library(ggpubr)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),
    
   
    #titlePanel("Cost and Return of Storing Grain"),
    titlePanel(img(src='logo.PNG', width = '25%')),
  
    # fluidRow(h3(id="big-heading", "   Cost and Return of Storing Grain"),
    #          tags$style(HTML("#big-heading{color: black, face: times;}")), style = "background-color: #B1810B;",
    #          font = "face: times;"),
    
    fluidRow(
        
        column(2,
               
               
               wellPanel(
                   selectInput("county", "County:",
                               c("Tippacanoe" = "Tippacanoe",
                                 "Adams" = "Adams")),
                   numericInput("bushels", 
                                h5("Bushels to Store"), 
                                value = 30000),
               sliderInput("price", h5("Cash Grain Price at Harvest $/bu"),
                           min = 0, max = 8.00, value = 4.00, step = .1),
               sliderInput("interest", h5("Short Term Interst Rate %"),
                           min = 2, max = 20, value = 7.00, step = 1),
               sliderInput("moist", h5("Moisture of Grain %"),
                           min = 10, max = 25, value = 15, step = .5),
               sliderInput("managehrs", h5("Hours Spent Managing"),
                           min = 0, max = 20, value = 5, step = 1),
               sliderInput("timevalue", h5("Value of Time $/hr"),
                           min = 0, max = 100, value = 30, step = 5),
               sliderInput("shrink", h5("Shrink Factor % per point removed"),
                           min = 0, max = 3, value = 1.5, step = .1)
               )
               ),
        column(2,
               
               wellPanel(
                   h4("On Farm Costs to Storage"),
                   
                   
                   sliderInput("handling", h5("Extra handling cost into and out $/bu"),
                               min = 0, max = .2, value = .02, step = .01),
                   sliderInput("transp", h5("Extra transportation cost $/bu"),
                               min = 0, max = .2, value = .08, step = .01),
                   sliderInput("quality", h5("Quality Deterioration %"),
                               min = 0, max = 5, value = 1.00, step = 1),
                   sliderInput("dollkwh", h5("Electricity Cost $/kwh"),
                               min = 0, max = .3, value = .11, step = .1)
                   # it takes .1672727 kw to dry 1 bushel 2 percentage points based on my backing out from Edwards
                   
               )
               
               ),
        column(2, 
              wellPanel(
                  h4("Commercial Storage Costs"),
                  sliderInput("rate", h5("Base Rate $/bu/mo"),
                              min = 0, max = .1, value = .05, step = .02),
                  sliderInput("comDry", h5("Commercial Drying Charge $/bu per percent moisture removed"),
                              min = 0, max = .1, value = .04, step = .01)
              ) ),
        column(6,
               plotOutput("distPlot")
        )
        )
        
        
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # definitely not calculating drying cost correctly
    hours <- 1440
    output$distPlot <- renderPlot({
        
        # Trans, handling, quality, managehrs, drying
        base_cost <- input$transp + input$handling + input$price*input$quality/100 + input$managehrs*input$timevalue/input$bushels + 
            max(input$moist - 13.5, 0)*.1672727/2*input$dollkwh/100*hours + input$price*input$shrink/100
        #
        cost_per_month <- input$price*input$interest/(12*100)
        
        months <- 1:10
        monthsT <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug" ) 
        Farm <- base_cost + cost_per_month*months
        
        Commercial <- (input$rate + input$price*input$interest/(12*100))*months + input$price*input$shrink/100 + max(input$moist - 13.5, 0)*input$comDry
        
       Ret <- cbind.data.frame(c(1,2,3,4,5,6,7,8,9,10, 1,2,3,4,5,6,7,8,9,10), 
                    c("Tippacanoe","Tippacanoe","Tippacanoe","Tippacanoe","Tippacanoe","Tippacanoe","Tippacanoe","Tippacanoe","Tippacanoe","Tippacanoe",
                       "Adams","Adams","Adams","Adams","Adams","Adams","Adams","Adams","Adams","Adams"), 
                    10.5*c(.01,.02,.03,.04,.05,.06,.07,.08,.07,.060, .011,.021,.031,.041,.051,.061,.051,.041,.031,.0210) )
        colnames(Ret) <- c('Months', 'County', 'Return')
        data <- cbind.data.frame(months, monthsT, Farm, Commercial)
        data <- gather(data, key = "type", value = "cost", Farm:Commercial)
       
        Ret <- filter(Ret, County == input$county)
        
        ggplot(data, aes(x = months, y = cost, color = type)) + 
            geom_line() + 
            geom_line(data = Ret, mapping = aes(x = Months, y = Return, color = 'Return to Storage')) +
            geom_point(size = 3) +
            geom_point(data = Ret, mapping = aes(x = Months, y = Return, color = 'Return to Storage'), size = 3) +
            dark_theme_dark() +
            theme(
                #panel.grid.major = element_blank(),
               # panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "#272B30",colour = NA),
                plot.background = element_rect(fill = "#272B30",colour = NA),
                legend.background = element_rect(fill = "#272B30"),
                axis.text = element_text(size = 14)
            ) +
            ylim(0,1)
            
        }, bg="transparent")
}

# Run the application 
shinyApp(ui = ui, server = server)
