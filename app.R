library(shiny)

ui <- fluidPage(
  fluidRow(
    h3("The original population is assumed to be in Hardy-Weinberg equillibrium."),
    column(5, 
           h4("Select your original population allele frequencies."),
          p("(Must be between 0 and 1.)"),
            numericInput("p", label = "p = f(M)?", min = 0.01, max = 0.99, value = 0.5, step = 0.01),
           h4("Original population genotypic frequencies:"),
           tableOutput("HW")
           ), 
    column(7, 
           h4("The selection weight for MM will be fixed at 1."),
           numericInput("wMM", label = "Selection weight for MM?", min = 1, max = 1, value = 1),
           numericInput("wMN", label = "Selection weight for MN?", min = 0, max = 100, value = 1),
           numericInput("wNN", label = "Selection weight for NN?", min = 0, max = 100, value = 1)
           )
    ),
  hr(),
  h3("You can press the button below to generate a new generation from this population with your given selection weights."),
  actionButton("new", "Click for a new generation"),
  actionButton("reset", "Click to reset table"),
  tableOutput("gen")
  )


server <- function(input, output, session) {
  
  HWvals <- reactive({
    hw <- list(MM = input$p^2, MN = input$p*(1-input$p)*2, NN = (1-input$p)^2)
  })
  
  
  v <- reactiveValues(valueButton = 1) #
  
  # Each time button is click, add 1 to custom value of the button
  observeEvent(input$new, {
    v$valueButton = v$valueButton + 1 
  })
  
  observeEvent(input$reset, {
    # reset value of valueButton
    v$valueButton = 1
  })
  
  
  output$HW <- renderTable({
    pop <- HWvals()
    data.frame(Genotype = c("Proportion"), MM = pop$MM, MN = pop$MN, NN = pop$NN, Total = 1)
  })
  
  output$gen <- renderTable({
    pop <- HWvals()
    
    counter <- 1
    while(counter < v$valueButton){
      total <- pop$MM[counter]*input$wMM + pop$MN[counter]*input$wMN + pop$NN[counter]*input$wNN

      pop$MM[counter + 1] <- pop$MM[counter]*input$wMM/total
      pop$MN[counter + 1] <- pop$MN[counter]*input$wMN/total
      pop$NN[counter + 1] <- pop$NN[counter]*input$wNN/total

      counter <- counter + 1
    }
    
    print(v$valueButton)
    data.frame(Genotype = rep("Proportion", v$valueButton), 
             MM = pop$MM, 
             MN = pop$MN, 
             NN = pop$NN, 
             Total = rep(1, v$valueButton))
  }, digits = 4, rownames = TRUE)


}
  
# Run the application 
shinyApp(ui = ui, server = server)
