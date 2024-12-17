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
  bootstrapServer("bootstrap", control = input)
}
