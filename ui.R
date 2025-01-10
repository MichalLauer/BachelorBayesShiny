ui <- page_sidebar(
  title = "Porovnání parametrického a neparametrického testování hypotéz",
  theme = bs_theme(
    version = 5
  ) |>
    bs_add_rules(sass::sass_file("./src/css/style.scss")),
  sidebar = sidebar(
    open = NA,
    accordion(
      class = "main-controls",
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
        title = "Testování hypotéz",
        layout_columns(
          numericInput(
            inputId = "n",
            label = tooltip(
              trigger = list(
                withMathJax("$$n$$"),
                icon("circle-question")
              ),
              "Velikost vzorku, který se vezme z populace",
              placement = "right"
            ),
            min = 1, max = 1000, value = 30,
            step = 1
          ),
          numericInput(
            inputId = "alpha",
            label = tooltip(
              trigger = list(
                withMathJax("$$\\alpha$$"),
                icon("circle-question")
              ),
              "Hladina významnosti",
              placement = "right"
            ),
            min = 0, max = 1, step = 0.05,
            value = 0.05
          )
        ),
        numericInput(
          inputId = "H0",
          label = tooltip(
            trigger = list(
              withMathJax("$$H_0$$"),
              icon("circle-question")
            ),
            "Nulová hypotéza, která je testována",
            placement = "right"
          ),
          value = 0,
          step = 0.1
        ),
        checkboxInput(
          inputId = "paired",
          label = "Jsou data závislá?"
        ),
        checkboxInput(
          inputId = "var.equal",
          label = "Je stejný rozptyl?"
        )
      ),
      accordion_panel(
        title = "Simulace",
        numericInput(
          inputId = "H1",
          label = tooltip(
            trigger = list(
              withMathJax("$$H_1$$"),
              icon("circle-question")
            ),
            "Alternativní hypotéza sloužící k výpočtu chyby II. typu",
            placement = "right"
          ),
          value = 0.5,
          step = 1
        ),
        layout_columns(
          numericInput(
            inputId = "K",
            label = tooltip(
              trigger = list(
                withMathJax("$$K$$"),
                icon("circle-question")
              ),
              "Počet vzorků pro zjištění statistik hypotéz (chyba I. druhu atp)",
              placement = "right"
            ),
            min = 1, max = 10000, step = 1,
            value = 100
          ),
          numericInput(
            inputId = "B",
            label = tooltip(
              trigger = list(
                withMathJax("$$B$$"),
                icon("circle-question")
              ),
              "Počet bootstrapových vzorků",
              placement = "right"
            ),
            min = 1, max = 10000, step = 1,
            value = 1000
          )
        )
      ),
      accordion_panel(
        title = "Replikovatelnost",
        numericInput(
          inputId = "seed",
          label = "Seed",
          value = runif(1)
        ),
        checkboxInput(
          inputId = "use.seed",
          label = "Použít seed?"
        )
      )
    ),
    actionButton(
      inputId = "go",
      label = "Zapínáme pásy!"
    )
  ),
  useShinyjs(),
  autoWaiter(
    id = c("parametric-stats", "nonparametric-stats", "bootstrap-stats"),
    html = spin_dots()
  ),
  navset_card_underline(
    title = "Zobrazené rozdělení",
    nav_panel("Populace", populationUI("population")),
    nav_panel("Parametrický test", parametricUI("parametric")),
    nav_panel("Neparametrický test", nonparametricUI("nonparametric")),
    nav_panel("Bootstrap test", bootstrapUI("bootstrap"))
  )
)