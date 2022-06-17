library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Probabilidade de falha em equipamentos"),
    
    fluidRow(
        column(6,
               radioButtons("Opcao","Selecione o cálculo",
                            choices=list("Probabilidade Exata"=1,
                                         "Menos que"=2,
                                         "Mais que"=3),selected = 1)
               ),
        column(6,
               numericInput("Ocorrencia","Ocorrência Atual:",
                            value=2,min=1,max=99),
               actionButton("Processar","Processar"))
    ),
    fluidRow(
        column(12,plotOutput("Graf"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$Processar, {
        lamb = input$Ocorrencia
        tipo = input$Opcao
        inic = lamb - 2
        fim = lamb + 2
        
        if(tipo==1){
            x = dpois(inic:fim,lambda = lamb)
            tit = "Probabilidade de Ocorrência"
        }
        if(tipo==2){
            x = ppois(inic:fim,lambda = lamb)
            tit = "Probabilidade de Ocorrência menor que"
        }
        if(tipo==3){
            x = ppois(inic:fim,lambda = lamb,lower.tail = F)
            tit = "Probabilidade de Ocorrência maior que"
        }
        
        y = as.character(round(x,3))
        z = as.character(inic:fim)
        
        lab = paste(y, " - Prob:", z)
        
        output$Graf = renderPlot({
            barplot(x,names.arg = lab,main=tit,col=gray.colors(5))
            box()
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
