populationUI <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      height = "100%",
      plotlyOutput(ns("distribution"))
    ),
    layout_columns(
      verbatimTextOutput(ns("distrib1")),
      verbatimTextOutput(ns("distrib2"))
    )
  )
}

populationServer <- function(id, control) {
  moduleServer(id, function(input, output, session) {
    sud <- session$userData

    output$distribution <- renderPlotly({
      pop <- sud$population

      xy <- get_xy(pop$x1)
      plot <-
        plot_ly(type = 'scatter', mode = 'lines', fill = 'tozeroy') |>
        add_trace(x = ~xy$x, y = ~xy$y, name = xy$n,
                  hoverinfo = 'text',
                  text = ~ glue("{xy$n}<br>",
                                "f({xy$x}) = {xy$y}")) |>
        layout(
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = paste("Hustota pro", xy$n)
          ),
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5
          )
        )

      if (!is.null(pop$x2)) {
        xy2 <- get_xy(pop$x2)
        plot <-
          plot |>
          add_trace(x = ~xy2$x, y = ~xy2$y, name = xy2$n, yaxis = "y2",
                    hoverinfo = 'text',
                    text = ~ glue("{xy2$n}<br>",
                                  "f({xy2$x}) = {xy2$y}")) |>
          layout(
            yaxis2 = list(
              side = "right",
              overlaying = "y",
              automargin = TRUE,
              title = paste("Hustota pro", xy2$n)
            )
          )
      }

      plot
    }) |>
      bindEvent(sud$go())

    output$distrib1 <- renderPrint({
      d <- sud$population$x1
      glue("
            První rozdělení: {d$strprint()}
            E(X): {d$mean()}
            D(X): {d$variance()}
           ")
    }) |>
      bindEvent(sud$go())

    output$distrib2 <- renderPrint({
      d <- sud$population$x2
      if (is.null(d)) {
        return("Druhé rozdělení: <nedefinováno>")
      }

      glue("
            Druhé rozdělení: {d$strprint()}
            E(X): {d$mean()}
            D(X): {d$variance()}
           ")
    }) |>
      bindEvent(sud$go())
  })
}
