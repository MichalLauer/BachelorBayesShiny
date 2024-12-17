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

    bootstrapped <- reactiveVal()

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

      observed_mean <- mean(sample)
      save <- list(
        statistic = observed_mean,
        p.value = mean(abs(means) <= abs(observed_mean))
      )
      bootstrapped(save)

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

    output$hypothesis <- renderPrint({
      boot <- bootstrapped()

      glue(
        "H0: mu = {control$H0}\n",
        "T: {boot$statistic}\n",
        "p-val.: {boot$p.value}\n"
      )
    }) |>
      bindEvent(control$go)

    output$stats <- renderPrint({
      pop <- sud$population()

      errorI <- sapply(
        X = seq_len(control$K),
        FUN = \(i) {
          set.seed(control$seed + i)
          if (is.null(pop$x2)) {
            sample <- pop$x1$rand(control$n)
          } else {
            sample <- pop$x1$rand(control$n) - pop$x2$rand(control$n)
          }
          # Centrování
          observed_mean <- mean(sample)
          centered_sample <- sample - observed_mean + control$H0

          means <- sapply(
            X = seq_len(control$B),
            FUN = \(i) {
              set.seed(control$seed + i)
              mean(sample(x = centered_sample,
                          size = length(centered_sample), replace = TRUE))
            }
          )

          p.value <- mean(abs(means) >= abs(observed_mean))

          p.value <= control$alpha
        }
      ) |> mean()

      errorII <- sapply(
        X = seq_len(control$K),
        FUN = \(i) {
          set.seed(control$seed + i)
          if (is.null(pop$x2)) {
            sample <- pop$x1$rand(control$n)
          } else {
            sample <- pop$x1$rand(control$n) - pop$x2$rand(control$n)
          }
          # Centrování
          observed_mean <- mean(sample)
          centered_sample <- sample - observed_mean + control$H0

          means <- sapply(
            X = seq_len(control$B),
            FUN = \(i) {
              set.seed(control$seed + i)
              mean(sample(x = centered_sample,
                          size = length(centered_sample), replace = TRUE))
            }
          )

          p.value <- mean(abs(means) >= abs(observed_mean))
          p.value > control$alpha
        }
      ) |> mean()

      glue(
        "Chyba I. typu: {errorI}\n",
        "Chyba II. typu: {errorII}\n",
        "Síla testu: {1 - errorII}\n",
      )
    }) |>
      bindEvent(control$go)
  })
}
