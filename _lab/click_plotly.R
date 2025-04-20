library(shiny)
library(plotly)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Exemplo de clique em geom_tile"),

  fluidRow(
    column(
      width = 6,
      plotlyOutput("sigma2")  # sem o argumento source aqui!
    ),
    column(
      width = 6,
      h4("Clique detectado:"),
      verbatimTextOutput("click_info")
    )
  )
)

server <- function(input, output, session) {

  # Dados de exemplo: um grid com valores aleatórios
  grid_data <- expand.grid(
    X = LETTERS[1:5],
    Y = as.character(1:5)
  )
  grid_data$Value <- runif(nrow(grid_data), min = 0, max = 100)

  # Renderizando o gráfico com ggplotly + source para capturar clique
  output$sigma2 <- renderPlotly({
    p <- ggplot(grid_data, aes(x = X, y = Y, fill = Value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#fdc6a7", high = "#a70000") +
      theme_minimal() +
      labs(x = "Coluna", y = "Linha", fill = "Valor")

    ggplotly(p, source = "my_tile_plot")
  })

  # Reactive que captura o clique
  tile_click <- reactive({
    event_data("plotly_click", source = "my_tile_plot")
  })

  # Mostra as coordenadas do clique
  output$click_info <- renderPrint({
    click <- tile_click()
    if (is.null(click)) {
      cat("Nenhum clique detectado ainda.")
    } else {
      cat("Clique detectado em:\n")
      cat("X:", click$x, "\n")
      cat("Y:", click$y, "\n")
    }
  })
}

shinyApp(ui, server)
