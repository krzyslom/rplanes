source("ui.R")

options(warn = -1)

profile_geometry <- read_data_from_db("profile_geometry")
geometry <- read_data_from_db("geometry")

server <- function(input, output, session) {
  output$airfoil_series <- render_airfoil_series_dropdown(geometry$name, input)
  output$dropdowns <- render_both_dropdowns()
  output$geometry_plot <- render_profile_geometry_plot(profile_geometry, input)
  output$geometry_table <- render_geometry_table(geometry, input)
}
