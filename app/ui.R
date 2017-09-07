source("functions.R")
source("packages.R")

ui <- function() {
  shinyUI(
    semanticPage(
      title = "rfoils!",
      includeCSS("www/app.css"),
      div(class = "ui container", style = "width: 90%; margin-top: 20px",
          render_hello(),
          render_dropdowns_ui(),
          p(),
          render_plot_ui(),
          render_table_ui(),
          render_author(),
          p()
      )
    )
  )
}
