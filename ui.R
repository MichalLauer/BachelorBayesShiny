ui <- page_sidebar(
  title = "Porovnání parametrického a neparametrického testování hypotéz",
  sidebar = sidebar(
    open = NA,
    accordion(
      accordion_panel(
        title = "Nastavení dat",
        textInput(
          inputId = "distr1",
          label = "Rozdělení 1. populace",
          value = "N(0, 1)",
          placeholder = "N(0, 1)"
        ),
        textInput(
          inputId = "distr2",
          label = "Rozdělení 2. populace",
          placeholder = "LN(5, 10)"
        )
      ),
      accordion_panel(
        title = "Nastavení hypotéz",
        numericInput(
          inputId = "n",
          label = "Velikost vzorku",
          min = 1, max = 1000, value = 30,
          step = 1
        ),
        numericInput(
          inputId = "H0",
          label = "Nulová hypotéza:",
          value = 0
        ),
        numericInput(
          inputId = "alpha",
          label = "Hladina významnosti",
          min = 0, max = 1, step = 0.05,
          value = 0.05
        ),
        numericInput(
          inputId = "B",
          label = "Počet bootstrapů",
          min = 1, max = 10000, step = 1,
          value = 1000
        ),
        numericInput(
          inputId = "K",
          label = "Počet simulací",
          min = 1, max = 10000, step = 1,
          value = 1000
        )
      )
    ),
    numericInput(
      inputId = "seed",
      label = "Seed",
      value = runif(1)
    ),
    actionButton(
      inputId = "go",
      label = "Zapínáme pásy!"
    )
  ),
  navset_card_underline(
    title = "Zobrazené rozdělení",
    nav_panel("Populace", populationUI("population")),
    nav_panel("Parametrický test",
              layout_columns(
                plotlyOutput("distrib_parametric")
              ),
              layout_columns(
                verbatimTextOutput("distrib_parametric_test"),
                verbatimTextOutput("distrib_parametric_stats")
              )
    ),
    nav_panel("Neparametrický test", plotlyOutput("distrib_nonparametric")),
    nav_panel("Bootstrap test", plotlyOutput("distrib_bootstrap"))
  )
)