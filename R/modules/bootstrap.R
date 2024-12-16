bootstrapUI <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      height = "100%",
      plotlyOutput(ns("distribution"))
    ),
    layout_columns(
      verbatimTextOutput(ns("hypothesis")),
      verbatimTextOutput(ns("stats"))
    )
  )
}

bootstrapServer <- function(id, control) {
  moduleServer(id, function(input, output, session) {
    sud <- session$userData

    output$distribution <- renderPlotly({
      sam <- sud$sampleData()
      if (is.null(sam$x2)) {
        sample <- sam$x1
      } else {
        sample <- sam$x1 - sam$x2
      }

      means <- sapply(
        X = seq_len(control$B),
        FUN = \(i) {
          set.seed(control$seed + i)
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
      bindEvent(control$go)
  })
}