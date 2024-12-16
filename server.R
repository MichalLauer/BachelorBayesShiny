server <- function(input, output, session) {

  # Uložení uživatelských rozdělení
  sud <- session$userData
  sud$population <- reactiveVal()
  sud$sampleData <- reactiveVal()
  observe({
    # Kontrola vstupu
    # TODO...

    pop <- list()
    sam <- list()
    pop$x1 <- dparse(input$distr1)
    set.seed(input$seed)
    sam$x1 <- get_sample(d = pop$x1, n = input$n)
    if (input$distr2 != "") {
      pop$x2 <- dparse(input$distr2)
      sam$x2 <- get_sample(d = pop$x2, n = input$n)
    } else {
      pop$x2 <- NULL
    }

    sud$population(NULL)
    sud$population(pop)
    sud$sampleData(NULL)
    sud$sampleData(sam)
  }) |>
    bindEvent(input$go)

  populationServer("population", control = input)
  parametricServer("parametric", control = input)
  nonparametricServer("nonparametric", control = input)









  output$distrib_nonparametric <-

  output$distrib_bootstrap <- renderPlotly({
    sam <- sampleData()
    if (is.null(sam$x2)) {
      sample <- sam$x1
    } else {
      sample <- sam$x1 - sam$x2
    }

    means <- sapply(
      X = seq_len(input$B),
      FUN = \(i) {
        set.seed(input$seed + i)
        mean(sample(x = sample, size = length(sample), replace = TRUE))
      }
    )

    plot <-
      plot_ly(type = 'histogram') |>
      add_trace(x = ~means) |>
      layout(
        title = "Neparametrický bootstrap",
        xaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        showlegend = FALSE
      )


    plot
  }) |>
    bindEvent(sampleData())

}


