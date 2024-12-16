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

    sud$population(pop)
    sud$sampleData(sam)
  }) |>
    bindEvent(input$go)

  populationServer("population")



  output$distrib_parametric <- renderPlotly({
    sam <- sampleData()
    # t-test
    test <- t.test(sam$x1, sam$x2, mu = input$H0)
    dH0 <- StudentT$new(df = unname(test$parameter))
    test.stat <- test$statistic
    p.test.stat <- dH0$pdf(test.stat)
    test.stat <- abs(test$statistic)
    # Pravá strana H0
    xH0r <- seq(from = test.stat,
                to = max(test.stat, dH0$quantile(1 - (1/1000))),
                by = 0.01)
    yH0r <- dH0$pdf(xH0r)
    # Levá strana HO
    xH0l <- seq(from = -test.stat,
                to = min(-test.stat, dH0$quantile(1/1000)),
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
    bindEvent(sampleData())

  output$distrib_parametric_test <- renderText({
    sam <- sampleData()
    # t-test
    test <- t.test(sam$x1, sam$x2, mu = input$H0)

    glue::glue(
      "H0: mu = {input$H0}\n",
      "T: {test$statistic}\n",
      "p-val.: {test$p.value}\n"
    )
  }) |>
    bindEvent(sampleData())

  output$distrib_parametric_stats <- renderText({
    pop <- population()
    # t-test
    errorI <- sapply(
      X = seq_len(input$K),
      FUN = \(i) {
        set.seed(input$seed + i)
        s1 <- pop$x1$rand(input$n)
        mu <- pop$x1$mean()
        s2 <- NULL
        if (!is.null(pop$x2)) {
          s2 <- pop$x2$rand(input$n)
          mu <- mu - pop$x2$mean()
        }

        t.test(s1, s2, mu = mu)$p.value <= input$alpha
      }
    ) |> mean()

    errorII <- sapply(
      X = seq_len(input$K),
      FUN = \(i) {
        set.seed(input$seed + i)
        s1 <- pop$x1$rand(input$n)
        s2 <- NULL
        if (!is.null(pop$x2)) {
          s2 <- pop$x2$rand(input$n)
        }

        t.test(s1, s2, mu = input$H0)$p.value >= input$alpha
      }
    ) |> mean()

    glue(
      "Chyba I. typu: {errorI}\n",
      "Chyba II. typu: {errorII}\n",
      "Síla testu: typu: {1 - errorII}\n",
    )
  }) |>
    bindEvent(population())

  output$distrib_nonparametric <- renderPlotly({
    sam <- sampleData()
    # t-test
    test <- wilcox.test(sam$x1, sam$x2, mu = input$H0)
    if (is.null(sam$x2)) {
      mu <- input$n*(input$n + 1)/4
      s2 <- input$n*(input$n + 1)*(2*input$n + 1)/24
    } else {
      mu <- input$n*(2*input$n + 1)/2
      s2 <- input$n*input$n*(2*input$n + 1)/12
    }
    test.stat <- (test$statistic - mu)/ sqrt(s2)
    dH0 <- Normal$new()
    p.test.stat <- dH0$pdf(test.stat)
    test.stat <- abs(test.stat)
    # Pravá strana H0
    xH0r <- seq(from = test.stat,
                to = max(test.stat, dH0$quantile(1 - (1/1000))),
                by = 0.01)
    yH0r <- dH0$pdf(xH0r)
    # Levá strana HO
    xH0l <- seq(from = -test.stat,
                to = min(-test.stat, dH0$quantile(1/1000)),
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

    plot
  }) |>
    bindEvent(sampleData())

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


