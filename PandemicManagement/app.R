library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Pandemic Management"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            selectInput("selection", 
                        h5("Choose a scenario"),
                        choices = list("Believe the Positive(BP)"=1, 
                                       "Believe the Negative(BN)"=2)
            ),
            sliderInput("p1", 
                        h5("Sensitivity of Test 1:"),
                        min = 0, max = 1, value = 0.5, step = 0.01),
            
            sliderInput("q1", 
                        h5("Specificity of Test 1:"),
                        min = 0, max = 1, value = 0.5, step = 0.01),
            sliderInput("p2", 
                        h5("Sensitivity of Test 2:"),
                        min = 0, max = 1, value = 0.5, step = 0.01),
            sliderInput("q2", 
                        h5("Specificity of Test 2:"),
                        min = 0, max = 1, value = 0.5, step = 0.01),
            
            sliderInput("pi", 
                        h5("Range of prevalence"),
                        min = 0, max = 1, value = 0.5, step = 0.1
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ppv"),
           plotOutput("npv")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ppv_res1<-function(pi){pi*(input$p1+input$p2*(1-input$p1))/(pi*
                                                                    (input$p1+input$p2*(1-input$p1))+(1-pi)*(1-input$q1*input$q2))}
    npv_res1<-function(pi){(1-pi)*input$q1*input$q2/((1-pi)*input$q1*input$q2+pi*
                                                         (1-input$p1-input$p2+input$p1*input$p2))}
    ppv_res2<-function(pi){pi*input$p1*input$p2/(pi*input$p1*input$p2+(1-pi)*
                                                     (1-(input$q1+input$q2*(1-input$q1))))}
    npv_res2<-function(pi){(1-pi)*(input$q1+input$q2*(1-input$q1))/((1-pi)*
                                                                        (input$q1+input$q2*(1-input$q1))+pi*(1-input$p1*input$p2))}
    getPPV<-function(pi){
        if(input$selection==1){
            ppv_res<-ppv_res1(pi)
        }else if(input$selection==2){
            ppv_res<-ppv_res2(pi)
        }
    }
    getNPV<-function(pi){
        if(input$selection==1){
            npv_res<-npv_res1(pi)
        }else if(input$selection==2){
            npv_res<-npv_res2(pi)
        }
    }
    
    
    output$ppv <- renderPlot(
        curve(getPPV, from = 0, to = input$pi, n = 101, add = FALSE, 
              type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
              xlim = NULL, ylim = c(0,1), main = "Positive predictive values (combined) 
          as a function of prevalence")
    )
    output$npv <- renderPlot(
        curve(getNPV, from = 0, to = input$pi, n = 101, add = FALSE, 
              type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
              xlim = NULL, ylim = c(0,1), main = "Negativee predictive values (combined) 
          as a function of prevalence")
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
