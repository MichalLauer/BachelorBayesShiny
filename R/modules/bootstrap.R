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
        if (control$use.seed) {
          set.seed(control$seed)
        }

        smpl <- sam$x1 - (sam$x2 %||% 0)
        observed_mean <- mean(smpl)

        means <- sapply(
          X = seq_len(control$B),
          FUN = \(i) {
            mean(sample(x = smpl, size = length(smpl), replace = TRUE))
          }
        )

        list(
          means = means,
          observed_mean = observed_mean,
          p.value = mean(abs(means - mean(means)) >= abs(observed_mean - mean(means)))
        )
      }, seed = TRUE)
    })

    # Paralelizace simulací
    SimulationTask <- ExtendedTask$new(function(pop, control) {
      future_promise({
        if (control$use.seed) {
          set.seed(control$seed)
        }

        errorI <- sapply(
          X = seq_len(control$K),
          FUN = \(i) {
            smpl <- pop$x1$rand(control$n)
            if (!is.null(pop$x2)) {
              smpl <- smpl - pop$x2$rand(control$n)
            }

            # Centrování
            observed_mean <- mean(smpl)
            centered_sample <- smpl - observed_mean + control$H0

            means <- sapply(
              X = seq_len(control$B),
              FUN = \(i) {
                mean(sample(x = centered_sample,
                            size = length(centered_sample), replace = TRUE))
              }
            )

            # H0 díky úpravě dat platí. Zkoumám pravděpodobnost, že zamítneme H0,
            # která ve skutečnosti platí.
            p.value <- mean(abs(means - mean(means)) >= abs(observed_mean - mean(means)))
            p.value <= control$alpha
          }
        ) |> mean()

        errorII <- sapply(
          X = seq_len(control$K),
          FUN = \(i) {
            smpl <- pop$x1$rand(control$n)
            if (!is.null(pop$x2)) {
              smpl <- smpl - pop$x2$rand(control$n)
            }

            means <- sapply(
              X = seq_len(control$B),
              FUN = \(i) {
                mean(sample(x = smpl,
                            size = length(smpl), replace = TRUE))
              }
            )

            # H1 v populaci platí. Zkoumám pravděpodobnost, že za platnosti H1
            # nezamítnu H0.
            p.value <- mean(abs(means - mean(means)) >= abs(control$H0 - mean(means)))
            p.value >= control$alpha
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
