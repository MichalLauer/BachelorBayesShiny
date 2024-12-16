parametricUI <- function(id) {
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

parametricServer <- function(id, control) {
  moduleServer(id, function(input, output, session) {
    sud <- session$userData

    output$distribution <- renderPlotly({
      sam <- sud$sampleData()
      # t-test
      test <- conduct_t_test(sam$x1, sam$x2, control)
      dH0 <- StudentT$new(df = unname(test$parameter))
      test.stat <- test$statistic
      p.test.stat <- dH0$pdf(test.stat)
      abs.test.stat <- abs(test$statistic)
      # Pravá strana H0
      xH0r <- seq(from = abs.test.stat,
                  to = max(abs.test.stat, dH0$quantile(1 - (1/1000))),
                  by = 0.01)
      yH0r <- dH0$pdf(xH0r)
      # Levá strana HO
      xH0l <- seq(from = -abs.test.stat,
                  to = min(-abs.test.stat, dH0$quantile(1/1000)),
                  by = -0.01)
      yH0l <- dH0$pdf(xH0l)
      # Data pro H0
      xy <- get_xy(d = dH0)
      plot <-
        plot_ly(type = 'scatter', mode = 'lines') |>
        add_trace(x = ~xy$x, y = ~xy$y, name = xy$n) |>
        add_trace(x = ~test.stat, y = ~p.test.stat, mode = "marker") |>
        add_trace(x = ~xH0r, y = ~yH0r, fill = "tozeroy") |>
        add_trace(x = ~xH0l, y = ~yH0l, fill = "tozeroy") |>
        layout(
          title = test$method,
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = paste("Hustota pro", xy$n)
          ),
          showlegend = FALSE
        )

    }) |>
      bindEvent(control$go)

    output$hypothesis <- renderPrint({
      sam <- sud$sampleData()
      test <- conduct_t_test(sam$x1, sam$x2, control)

      glue(
        "H0: mu = {control$H0}\n",
        "T: {test$statistic}\n",
        "p-val.: {test$p.value}\n"
      )
    }) |>
      bindEvent(control$go)

    output$stats <- renderPrint({
      pop <- sud$population()
      # t-test
      errorI <- sapply(
        X = seq_len(control$K),
        FUN = \(i) {
          set.seed(control$seed + i)
          s1 <- pop$x1$rand(control$n)
          mu <- pop$x1$mean()
          s2 <- NULL
          if (!is.null(pop$x2)) {
            s2 <- pop$x2$rand(control$n)
            mu <- mu - pop$x2$mean()
          }

          conduct_t_test(s1, s2, control)$p.value <= control$alpha
        }
      ) |> mean()

      errorII <- sapply(
        X = seq_len(control$K),
        FUN = \(i) {
          set.seed(control$seed + i)
          s1 <- pop$x1$rand(control$n)
          s2 <- NULL
          if (!is.null(pop$x2)) {
            s2 <- pop$x2$rand(control$n)
          }

          conduct_t_test(s1, s2, control, use_h0 = FALSE)$p.value >= control$alpha
        }
      ) |> mean()

      glue(
        "Chyba I. typu: {errorI}\n",
        "Chyba II. typu: {errorII}\n",
        "Síla testu: typu: {1 - errorII}\n",
      )
    }) |>
      bindEvent(control$go)
  })
}