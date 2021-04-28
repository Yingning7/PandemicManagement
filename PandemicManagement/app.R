library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(


    navbarPage(
        "Pandemic Management",
        tabPanel("Two Tests Independent",
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
        ),
        tabPanel("Two Tests Dependent",
                 sidebarPanel(
                     selectInput("selectiond", 
                                 h5("Choose a scenario"),
                                 choices = list("Believe the Positive(BP)"=1, 
                                                "Believe the Negative(BN)"=2)
                     ),
                     sliderInput("p1d", 
                                 h5("Sensitivity of Test 1:"),
                                 min = 0, max = 1, value = 0.5, step = 0.01),
                     
                     sliderInput("q1d", 
                                 h5("Specificity of Test 1:"),
                                 min = 0, max = 1, value = 0.5, step = 0.01),
                     sliderInput("p2d", 
                                 h5("Sensitivity of Test 2:"),
                                 min = 0, max = 1, value = 0.5, step = 0.01),
                     sliderInput("q2d", 
                                 h5("Specificity of Test 2:"),
                                 min = 0, max = 1, value = 0.5, step = 0.01),
                     
                     sliderInput("pid", 
                                 h5("Range of prevalence"),
                                 min = 0, max = 1, value = 0.5, step = 0.1
                     ),
                     sliderInput("a",
                                 h5("Dependent variable a:"),
                                 min= 0, max = 10000, value = 4,  step = 0.1),
                     sliderInput("b",
                                 h5("Dependent variable b:"),
                                 min = 0, max = 10000, value = 4,  step = 0.1)),
                 mainPanel(
                     plotOutput("ppvd"),
                     plotOutput("npvd")
                 )
                     
                 ),
        tabPanel(
          "More Combined Tests",
          sidebarPanel(
            selectInput("selectionm", 
                        h5("Choose a scenario"),
                        choices = list("Believe the Positive(BP)"=1, 
                                       "Believe the Negative(BN)"=2)
            ),
            numericInput("num", label = h5("Input the number of weeks"), value = 1, step = 1),
            sliderInput("p1m", 
                        h5("Sensitivity of Test 1:"),
                        min = 0, max = 1, value = 0.5, step = 0.01),
            
            sliderInput("q1m", 
                        h5("Specificity of Test 1:"),
                        min = 0, max = 1, value = 0.5, step = 0.01),
            sliderInput("p2m", 
                        h5("Sensitivity of Test 2:"),
                        min = 0, max = 1, value = 0.5, step = 0.01),
            sliderInput("q2m", 
                        h5("Specificity of Test 2:"),
                        min = 0, max = 1, value = 0.5, step = 0.01),
            
            sliderInput("pim", 
                        h5("Range of prevalence"),
                        min = 0, max = 1, value = 0.5, step = 0.1
            ),
            sliderInput("am",
                        h5("Dependent variable a:"),
                        min= 0, max = 10000, value = 4,  step = 0.1),
            sliderInput("bm",
                        h5("Dependent variable b:"),
                        min = 0, max = 10000, value = 4,  step = 0.1)),
          mainPanel(
            plotOutput("ppvm"),
            plotOutput("npvm")
          )
        )
                 )
    )


# Define server logic required to draw a histogram
server <- function(input, output, session) {


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
    observe({
      val1<-input$p1d
      val2<-input$q1d
      val3<-input$p2d
      val4<-input$q2d
      if(input$selectiond==1){
        updateSliderInput(session, "a", value = 4,
                          min = 0, max = round((1/((1-val1)*(1-val3)))-0.1,digits = 2), step = 0.1)
        
        updateSliderInput(session, "b", value = 4,
                          min = 0, max = round((1/(val2*val4))-0.1,digits = 2), step = 0.1)}
      if(input$selectiond==2){
        updateSliderInput(session, "a", value = 4, min = 0,
                          max = round((1/(val1*val3))-0.1,digits = 2), step = 0.1)
        updateSliderInput(session, "b", value = 4,
                          min = 0, max = round((1/((1-val2)*(1-val4)))-0.1,digits = 2), step = 0.1)}
        v1<-input$p1m
        v2<-input$q1m
        v3<-input$p2m
        v4<-input$q2m
        if(input$selectionm==1){
          updateSliderInput(session, "am", value = 4,
                            min = 0, max = round((1/((1-v1)*(1-v3)))-0.1,digits = 2), step = 0.1)
          
          updateSliderInput(session, "bm", value = 4,
                            min = 0, max = round((1/(v2*v4))-0.1,digits = 2), step = 0.1)}
        if(input$selectionm==2){
          updateSliderInput(session, "am", value = 4, min = 0,
                            max = round((1/(v1*v3))-0.1,digits = 2), step = 0.1)
          updateSliderInput(session, "bm", value = 4,
                            min = 0, max = round((1/((1-v2)*(1-v4)))-0.1,digits = 2), step = 0.1)
      }})
    ppvd_res1<-function(pid){(pid*(1-input$a*(1-input$p1d)*(1-input$p2d)))/(pid*(1-input$a*
                                                                                   (1-input$p1d)*(1-input$p2d))+(1-pid)*(1-input$b*input$q1d*input$q2d))}
    npvd_res1<-function(pid){((1-pid)*input$b*input$q1d*input$q2d)/((1-pid)*input$b*input$q1d*
                                                                      input$q2d+pid*input$a*(1-input$p1d)*(1-input$p2d))}
    ppvd_res2<-function(pid){(pid*input$a*input$p1d*input$p2d)/(pid*input$a*input$p1d*input$p2d+
                                                                  (1-pid)*input$b*(1-input$q1d)*(1-input$q2d))}
    npvd_res2<-function(pid){((1-pid)*(1-input$b*(1-input$q1d)*(1-input$q2d)))/((1-pid)*(1-input$b*
                                                                                         (1-input$q1d)*(1-input$q2d))+pid*(1-input$a*input$p1d*input$p2d))}
    getPPVD<-function(pid){
        if(input$selectiond==1){
            ppvd_res<-ppvd_res1(pid)
        }else if(input$selectiond==2){
            ppvd_res<-ppvd_res2(pid)
        }
    }
    getNPVD<-function(pid){
        if(input$selectiond==1){
            npvd_res<-npvd_res1(pid)
        }else if(input$selectiond==2){
            npvd_res<-npvd_res2(pid)
        }
    }
    ppvm_res1<-function(pim){(pim*(1-(input$am*(1-input$p1m)*(1-input$p2m))^input$num))/(pim*(1-(input$am*
                                (1-input$p1m)*(1-input$p2m))^input$num)+(1-pim)*(1-(input$bm*input$q1m*input$q2m)^input$num))}
    npvm_res1<-function(pim){((1-pim)*(input$bm*input$q1m*input$q2m)^input$num)/((1-pim)*(input$bm*input$q1m*
                                input$q2m)^input$num+pim*(input$am*(1-input$p1m)*(1-input$p2m))^input$num)}
    ppvm_res2<-function(pim){(pim*(input$am*input$p1m*input$p2m)^input$num)/(pim*(input$am*input$p1m*input$p2m)^input$num+
                                (1-pim)*(input$bm*(1-input$q1m)*(1-input$q2m))^input$num)}
    npvm_res2<-function(pim){((1-pim)*(1-(input$bm*(1-input$q1m)*(1-input$q2m))^input$num))/((1-pim)*(1-(input$bm*
                                (1-input$q1m)*(1-input$q2m))^input$num)+pim*(1-(input$am*input$p1m*input$p2m)^input$num))}
    getPPVM<-function(pim){
      if(input$selectionm==1){
        ppvm_res<-ppvm_res1(pim)
      }else if(input$selectionm==2){
        ppvm_res<-ppvm_res2(pim)
      }
    }
    getNPVM<-function(pim){
      if(input$selectionm==1){
        npvm_res<-npvm_res1(pim)
      }else if(input$selectionm==2){
        npvm_res<-npvm_res2(pim)
      }
    }
    output$ppv <- renderPlot(
        curve(getPPV, from = 0, to = input$pi, n = 101, add = FALSE, 
              type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
              xlim = NULL, ylim = c(0,1), main = "Overall positive predictive values
          as a function of prevalence (independent case)")
    )
    output$npv <- renderPlot(
        curve(getNPV, from = 0, to = input$pi, n = 101, add = FALSE, 
              type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
              xlim = NULL, ylim = c(0,1), main = "Overall negative predictive values 
          as a function of prevalence (independent case)")
    )
    output$ppvd <- renderPlot(
        curve(getPPVD, from = 0, to = input$pid, n = 101, add = FALSE, 
              type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
              xlim = NULL, ylim = c(0,1), main = "Overall positive predictive values 
              as a function of prevalence (dependent case)")
    )
    output$npvd <- renderPlot(
        curve(getNPVD, from = 0, to = input$pid, n = 101, add = FALSE, 
              type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
              xlim = NULL, ylim = c(0,1), main = "Overall negative predictive values
          as a function of prevalence (dependent case)")
    )
    output$ppvm <- renderPlot(
      curve(getPPVM, from = 0, to = input$pim, n = 101, add = FALSE, 
            type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
            xlim = NULL, ylim = c(0,1), main = "Overall positive predictive values 
              as a function of prevalence (dependent case)")
    )
    output$npvm <- renderPlot(
      curve(getNPVM, from = 0, to = input$pim, n = 101, add = FALSE, 
            type = "l", xname = "x", xlab = "prevalence",  log = NULL, 
            xlim = NULL, ylim = c(0,1), main = "Overall negative predictive values
          as a function of prevalence (dependent case)")
    )
   
}


# Run the application 
shinyApp(ui = ui, server = server)
