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
    sam$x1 <- get_sample(d = pop$x1, c = input)
    if (input$distr2 != "") {
      pop$x2 <- dparse(input$distr2)
      sam$x2 <- get_sample(d = pop$x2, c = input)
    } else {
      pop$x2 <- NULL
    }

    sud$population(NULL)
    sud$population(pop)
    sud$sampleData(NULL)
    sud$sampleData(sam)
  }) |>
    bindEvent(input$go)

  observe({
    if (input$use.seed) {
      enable(id = "seed")
    } else {
      disable(id = "seed")
    }
  }) |>
    bindEvent(input$use.seed)

  populationServer("population", control = input)
  parametricServer("parametric", control = input)
  nonparametricServer("nonparametric", control = input)
  bootstrapServer("bootstrap", control = input)
}
