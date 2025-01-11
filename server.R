server <- function(input, output, session) {

  # Definice pravidel pro vstupní data
  iv <- InputValidator$new()
  iv$add_rule("distr1", is_distr)
  iv$add_rule("distr2", is_distr)
  iv$add_rule("n", sv_required("n je povinné."))
  iv$add_rule("n", sv_gte(2, "n musí být >= {rhs}."))
  iv$add_rule("n", sv_integer("n musí být celé číslo."))
  iv$add_rule("alpha", sv_required("alpha je povinná."))
  iv$add_rule("alpha", sv_between(0, 1, message_fmt = "alpha musí být v <{left}, {right})."))
  iv$add_rule("alpha", sv_between(0, 1, message_fmt = "alpha musí být v <{left}, {right})."))
  iv$add_rule("H0", sv_required("H0 je povinná."))
  iv$add_rule("H1", sv_required("H1 je povinná."))
  iv$add_rule("K", sv_required("K je povinné."))
  iv$add_rule("K", sv_gte(1, "K musí být >= {rhs}."))
  iv$add_rule("K", sv_integer("K musí být celé číslo."))
  iv$add_rule("B", sv_required("B je povinné."))
  iv$add_rule("B", sv_gte(1, "B musí být >= {rhs}."))
  iv$add_rule("B", sv_integer("B musí být celé číslo."))
  iv$add_rule("seed", \(x) if (input$use.seed & !is.numeric(x)) "Je nutné zadat číslo.")
  iv$enable()

  # Uložení uživatelských rozdělení
  sud <- session$userData
  sud$population <- list()
  sud$sampleData <- list()
  sud$go <- reactiveVal(NULL)
  observe({
    req(iv$is_valid())

    pop <- list()
    sam <- list()
    pop$x1 <- dparse(input$distr1)
    sam$x1 <- get_sample(d = pop$x1, c = input)
    if (input$distr2 != "") {
      pop$x2 <- dparse(input$distr2)
      sam$x2 <- get_sample(d = pop$x2, c = input)
    }

    sud$population <- pop
    sud$sampleData <- sam
    sud$go((sud$go() %||% 0) + runif(1))
  }) |>
    bindEvent(input$go)

  # Pro první běh nastav výšku
  observe({
    runjs(glue(
      r'($("#parametric-stats"}").empty().prepend("\n\n\n");)',
      r'($("#nonparametric-stats"}").empty().prepend("\n\n\n");)',
      r'($("#bootstrap-stats"}").empty().prepend("\n\n\n");)'
    ))
  }) |>
    bindEvent(sud$go(), once = TRUE)

  # Vypnutí/zapnutí seed vstupu
  observe({
    if (input$use.seed) {
      enable(id = "seed")
    } else {
      disable(id = "seed")
    }
  }) |>
    bindEvent(input$use.seed)

  # Moduly
  populationServer("population", control = input)
  parametricServer("parametric", control = input)
  nonparametricServer("nonparametric", control = input)
  bootstrapServer("bootstrap", control = input)
}
