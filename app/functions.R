read_data_from_db <- function(table_name) {
  connection <- dbConnect(SQLite(), dbname = "data/airfoil.db")
  table_data <- dbReadTable(connection, table_name)
  dbDisconnect(connection)
  table_data
}

ribbon_ui <- function(label) {
  div(class = "ui blue ribbon label",
      label
  )
}

render_hello <- function() {
  div(class = "ui raised segment",
      ribbon_ui("Hello!"),
      div(class = "ui center aligned grid",
          div(class = "column",
              h1("Welcome to the airfoil data application!"),
              h3("Choose an airfoil you want to inspect.")
          )
      )
  )
}

render_dropdown <- function(name, label, choices, default_text) {
  div(class = "column",
      div(class = "ui pointing below blue label",
          label
      ),
      dropdown(name = name, choices = choices, default_text = default_text)
  )
}

render_first_letter_dropdown <- function() {
  render_dropdown("first_letter", "Airfoil first letter", LETTERS,
                  "Choose first letter of the series…")
}

render_airfoil_series_dropdown <- function(series, input) {
  airfoil_choices <- eventReactive(input$first_letter, {
    if (!is.null(input$first_letter)) {
      series %>%
        str_subset(str_c("^", input$first_letter))
    } else {
      series
    }
  })

  default_text <- reactive({
    if (length(airfoil_choices()) != 0) {
      "Choose airfoil series…"
    } else {
      "No airfoils that begin with that letter."
    }
  })

  renderUI({
    render_dropdown("airfoil_series", "Airfoil series", airfoil_choices(),
                    default_text())
  })
}

render_both_dropdowns <- function() {
  renderUI({
    div(class = "ui center aligned two column grid",
        render_first_letter_dropdown(),
        div(class = "column",
            uiOutput("airfoil_series")
        )
    )
  })
}

render_dropdowns_ui <- function() {
  div(class = "ui raised segment",
      ribbon_ui("Airfoils"),
      uiOutput("dropdowns")
  )
}

run_validation <- function(data, error_class) {
  validate(
    need(nrow(data) != 0,
         "No airfoil series chosen"),
    errorClass = error_class
  )
}

filter_data <- function(data, input, error_class) {
  eventReactive(input$airfoil_series, {
    filtered_data <- data %>%
      filter(name == input$airfoil_series)

    run_validation(filtered_data, error_class)

    filtered_data
  })
}

render_profile_geometry_plot <- function(data, input) {
  filtered_data <- filter_data(data, input, "render plot")

  renderPlotly({
    plot_ly(filtered_data(), x = ~ x_coord, y = ~ y_coord,
            type = "scatter", mode = "lines", color = I("blue"),
            name = "Airfoil contour") %>%
      add_markers(showlegend = FALSE) %>%
      add_lines(y = ~ fitted(loess(y_coord ~ x_coord)),
                line = list(color = "rgba(0, 0, 255, .5)", width = 2),
                name = "Camber line") %>%
      layout(title = ~ name,
             xaxis = list(title = "x scale"),
             yaxis = list(title = "y scale"))
  })
}

render_segment_ui <- function(ribbon_label, output_ui) {
  div(class = "ui raised segment",
      ribbon_ui(ribbon_label),
      output_ui %>%
        withSpinner()
  )
}

render_plot_ui <- function() {
  render_segment_ui("Airfoil geometry", plotlyOutput("geometry_plot"))
}

render_geometry_table <- function(data, input) {
  render_row <- function(attribute, value, icon) {
    tags$tr(
      tags$td(
        h4(class = "ui image header",
           uiicon(icon),
           div(class = "content",
               attribute
           )
        )
      ),
      tags$td(
        value
      )
    )
  }

  filtered_data <- filter_data(data, input, "render")

  gathered_data <- reactive({
    filtered_data() %>%
      gather(attribute, value) %>%
      mutate(icon = case_when(attribute == "name" ~ "plane",
                              attribute %in% c("thickness", "camber",
                                               "lower_flatness",
                                               "leading_edge_radius") ~
                                "percent",
                              attribute %>%
                                str_detect("angle") ~ "radio",
                              TRUE ~ "minus")) %>%
      mutate(attribute = c("Series", "Camber", "Efficiency",
                           "Leading edge radius", "Lower flatness",
                           str_c("Max C", tags$sub("L"), c("", " angle")),
                           str_c("Max L/D", c("", " angle")),
                           str_c("Max L/D C", tags$sub("L")), "Stall angle",
                           "Thickness", "Trailing edge angle",
                           "Zero lift angle") %>%
               map(HTML))
  })

  renderUI({
    div(class = "ui centered grid",
        tags$table(class = "ui very basic collapsing celled table",
                   tags$body(
                     list(gathered_data()$attribute, gathered_data()$value,
                          gathered_data()$icon) %>%
                       pmap(render_row)
                   )
        )
    )
  })
}

render_table_ui <- function() {
  render_segment_ui("Airfoil parameters", tableOutput("geometry_table"))
}

render_author <- function() {
  render_icon_link <- function(href, icon) {
    a(href = href,
      target = "_blank",
      rel = "noopener noreferrer",
      uiicon(str_c("huge", icon, sep = " "))
    )
  }

  div(class = "ui raised segment",
      ribbon_ui("Author"),
      div(class = "ui centered grid",
          div(class = "ui breadcrumb",
              render_icon_link("https://github.com/krzyslom", "github"),
              render_icon_link(
                "https://www.linkedin.com/in/krzysztof-slomczynski/",
                "linkedin square"
              )
          )
      )
  )
}
