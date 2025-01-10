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

    # Paralelizace bootstrapu
    BootstrappedTask <- ExtendedTask$new(function(sam, control) {
      future_promise({
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

        list(
          means = means,
          observed_mean = mean(sample),
          p.value = mean(abs(means) <= abs(mean(sample)))
        )
      }, seed = TRUE)
    })

    # Paralelizace simulací
    SimulationTask <- ExtendedTask$new(function(pop, control) {
      future_promise({
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

        list(
          errorI = errorI,
          errorII = errorII
        )
      }, seed = TRUE)
    })

    # Zahájení simulace pro bootstrap
    observe({
      control_list <- reactiveValuesToList(control)
      BootstrappedTask$invoke(sam = sud$sampleData,
                              control = control_list)
    }) |>
      bindEvent(sud$go())

    # Bootstrap histogram
    output$distribution <- renderPlotly({
      boot <- BootstrappedTask$result()
      plot_ly(type = 'histogram') |>
        add_trace(x = ~boot$means) |>
        layout(
          title = "Neparametrický bootstrap",
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE
          ),
          showlegend = FALSE
        )
    })

    # Charakteristika bootstrapu
    output$hypothesis <- renderPrint({
      boot <- BootstrappedTask$result()
      glue(
        "H0: mu = {control$H0}\n",
        "T: {boot$observed_mean}\n",
        "p-val.: {boot$p.value}\n"
      )
    })

    # Zahájení simulace pro bootstrap
    observe({
      control_list <- reactiveValuesToList(control)
      SimulationTask$invoke(pop = sud$population,
                            control = control_list)
      # TODO: Proč to nefunguje?
      # runjs(glue(
      #   r'($("#{session$ns("stats")}").empty().prepend("\n\n\n");)'
      # ))
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
