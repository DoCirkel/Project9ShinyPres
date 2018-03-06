library(shiny)
library(ggplot2)
library(datasets)
data(mtcars)

ui <- fluidPage(
   
   # Application title
   titlePanel("ChickWeight "),
   
   # Sidebar with a slider input for Time 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Time",
                     "Desired time in days:",
                     min = 0,
                     max = 21,
                     step = 0.1,
                     value = 10),
      
      # Sidebar with a slider input for DIET
      selectInput("Diet",
                  "Select a diet:",
                  choices = c( 1:4))
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot1"),
        h4("Diet:"),
        textOutput("text3"),
        h4("Time:"),
        textOutput("text1"),
        h4("Predicted weight:"),
        textOutput("text2")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  fit <- reactive ({loess(weight ~ Time, data = ChickWeight[ChickWeight$Diet == input$Diet,])})
  pred <- reactive ({predict(fit(), newdata = input$Time)})
   output$plot1 <- renderPlot({

      # generate bins based on input$bins from ui.R
     ggplot() + 
       geom_point(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
       geom_smooth(data = ChickWeight[ChickWeight$Diet == input$Diet,], aes(x = Time, y = weight, colour = Diet), method = "loess") +
       geom_point(aes(x = input$Time, y = pred()), colour = "black", size = 3) +
       geom_label(aes(x = input$Time, y = pred()+30, label = paste0("x = ", input$Time, ", y = ", round(pred(), digits = 1)))) +
       coord_cartesian(y = c(0:max(ChickWeight$weight)))
   })
   output$text1 <- renderText({
     input$Time 
   })
   output$text2 <- renderText({
     pred() 
   })
   output$text3 <- renderText({
     input$Diet
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

