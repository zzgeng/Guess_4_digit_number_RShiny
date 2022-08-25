library(shiny)
library(dplyr)

ui <- basicPage(
    titlePanel("GUESS NUMBER!"), 
    mainPanel("Guess a 4-digit number, each try will tell you how many right numbers and how many is in right position"),
    mainPanel("There won't be repeat numbers and 0 can be the first number"),
    mainPanel("right number means how many numbers from input are also present in the answer, right position means how many numbers from input are at right digit (position)"), 
    textInput("number", "Enter here"),
    actionButton("Button", label = "submit"),
    textOutput("number"),
    textOutput("position"),
    textOutput("count")

)
    
    puzzle <- sample(0:9, 4)
    print(puzzle)
    
server <- function(input, output){ 
                
    test <- eventReactive(input$Button, {
        as.numeric(strsplit(as.character(input$number), "")[[1]])
        })
                
    values <- reactiveValues(i = -1)
                
    observe({
        input$Button
        isolate(values$i <- values$i + 1)
    })
                
    output$count <- renderText({
        paste0("ATTEMP: ", values$i)
    })
                
    output$number <- renderText({
        if(sum(test() == puzzle) == 4){
            return("GOOD JOB!")
        }else{
            paste0("RIGHT NUMBER: ", 
            sum(unique(test()) %in% puzzle))
            }
        })
    
    output$position <- renderText({
        if(sum(test() == puzzle) == 4){
            return("")
        }else{
            paste0("RIGHT POSITION: ", 
            sum(test() == puzzle))
        }
    })
    
                
        
}
        
shinyApp(ui, server)
