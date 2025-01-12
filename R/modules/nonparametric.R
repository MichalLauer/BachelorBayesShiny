nonparametricUI <- function(id) {
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

nonparametricServer <- function(id, control) {
  moduleServer(id, function(input, output, session) {
    sud <- session$userData

    # Rozdělení podle h0
    output$distribution <-  renderPlotly({
      sam <- sud$sampleData
      test <- conduct_wilcox_test(sam$x1, sam$x2, control)
      test.stat <- wilcox_test_stat(test, c = control, s = sam)

      dH0 <- Normal$new()
      p.test.stat <- dH0$pdf(test.stat)
      abs.test.stat <- abs(test.stat)
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
        add_trace(x = ~xy$x, y = ~xy$y, name = xy$n,
                  hoverinfo = 'text',
                  text = ~ glue("{xy$n}<br>",
                                "f({xy$x}) = {xy$y}")) |>
        add_trace(x = ~test.stat, y = ~p.test.stat, mode = "marker",
                  hoverinfo = 'text',
                  text = ~ glue("T={test.stat}")) |>
        add_trace(x = ~xH0r, y = ~yH0r, fill = "tozeroy", fillcolor = '#ff4e4e',
                  hoverinfo = 'text',
                  hoveron = 'fills',
                  text = ~ glue("p-value: {test$p.value}")) |>
        add_trace(x = ~xH0l, y = ~yH0l, fill = "tozeroy", fillcolor = '#ff4e4e',
                  hoverinfo = 'text',
                  hoveron = 'fills',
                  text = ~ glue("p-value: {test$p.value}")) |>
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

      plot
    }) |>
      bindEvent(sud$go())

    # Charakteristika h0
    output$hypothesis <- renderPrint({
      sam <- sud$sampleData
      test <- conduct_wilcox_test(sam$x1, sam$x2, control)
      test.stat <- wilcox_test_stat(test, c = control, s = sam)

      glue(
        "H0: mu = {control$H0}\n",
        "W: {test.stat}\n",
        "p-val.: {test$p.value}\n"
      )
    }) |>
      bindEvent(sud$go())

    # Paralelizace
    SimulationTask <- ExtendedTask$new(function(pop, control) {
      future_promise({
        errorI <- sapply(
          X = seq_len(control$K),
          FUN = \(i) {
            s1 <- get_sample(d = pop$x1, c = control, i = i)
            true_H0 <- pop$x1$mean()
            s2 <- NULL
            if (!is.null(pop$x2)) {
              s2 <- get_sample(d = pop$x2, c = control)
              true_H0 <- true_H0 - pop$x2$mean()
            }

            # H0 díky úpravě dat platí. Zkoumám pravděpodobnost, že zamítná H0,
            # která ve skutečnosti platí.
            conduct_wilcox_test(s1, s2, control, h0 = true_H0)$p.value <= control$alpha
          }
        ) |> mean()

        errorII <- sapply(
          X = seq_len(control$K),
          FUN = \(i) {
            s1 <- get_sample(d = pop$x1, c = control, i = i)
            s2 <- NULL
            if (!is.null(pop$x2)) {
              s2 <- get_sample(d = pop$x2, c = control)
            }

            # H1 v populaci platí. Zkoumám pravděpodobnost, že za platnosti H1
            # nezamítnu H0.
            conduct_wilcox_test(s1, s2, control, h0 = control$H0)$p.value >= control$alpha
          }
        ) |> mean()

        list(
          errorI = errorI,
          errorII = errorII
        )
      }, seed = TRUE)
    })

    # Zahájení simulace
    observe({
      control_list <- reactiveValuesToList(control)
      SimulationTask$invoke(pop = sud$population,
                            control = control_list)
    }) |>
      bindEvent(sud$go())

    # Zobrazení výsledků ze simulace
    output$stats <- renderPrint({
      r <- SimulationTask$result()

      glue(
        "Chyba I. typu: {r$errorI}\n",
        "Chyba II. typu: {r$errorII}\n",
        "Síla testu: {1 - r$errorII}\n",
      )
    })
  })
}
