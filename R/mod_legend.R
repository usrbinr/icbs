# Module: Legend Block Configuration
# Handles inline text legend settings

#' Legend Module UI
#' @param id Module namespace ID
#' @noRd
mod_legend_ui <- function(id) {
    ns <- shiny::NS(id)

    bslib::accordion_panel(
        title = shiny::span(shiny::span(class = "badge bg-purple me-2", " "), "Legend Block"),
        value = "Legend Block",
        icon = shiny::icon("tags"),

        shiny::checkboxInput(ns("enabled"), "Enable inline legend", value = FALSE),
        shiny::conditionalPanel(
            condition = paste0("input['", ns("enabled"), "']"),
            shiny::textInput(ns("labels"), "Labels (comma-separated)",
                value = "Category A, Category B, Category C", width = "100%"),
            shiny::uiOutput(ns("color_inputs")),
            bslib::layout_column_wrap(
                width = 1/2,
                shiny::selectInput(ns("position"), "Position", width = "100%",
                    choices = c("Above chart" = "above", "Below chart" = "below",
                                "Left of chart" = "left", "Right of chart" = "right")),
                shiny::selectInput(ns("halign"), "Align", width = "100%",
                    choices = c("Left" = "left", "Center" = "center", "Right" = "right"),
                    selected = "right")
            ),
            shiny::textInput(ns("sep"), "Separator", value = " | ", width = "100%"),
            bslib::layout_column_wrap(
                width = 1/2,
                shiny::sliderInput(ns("size"), "Size", min = 7, max = 14, value = 10, step = 1),
                shiny::sliderInput(ns("width"), "Width", min = 0.05, max = 0.25, value = 0.12, step = 0.01)
            ),
            shiny::div(
                class = "d-flex gap-3",
                shiny::checkboxInput(ns("bold"), "Bold", value = TRUE),
                shiny::checkboxInput(ns("uppercase"), "UPPERCASE", value = FALSE)
            ),
            shiny::numericInput(ns("lineheight"), "Line height (vertical)", value = 1.6, min = 1, max = 3, step = 0.1, width = "100%"),
            shiny::sliderInput(ns("wrap"), "Wrap at chars (0=off)", min = 0, max = 60, value = 0, step = 5)
        )
    )
}

#' Legend Module Server
#' @param id Module namespace ID
#' @return List of reactives for legend configuration
#' @noRd
mod_legend_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        default_colors <- default_legend_colors

        # Dynamic color inputs based on labels
        output$color_inputs <- shiny::renderUI({
            labels <- trimws(strsplit(input$labels %||% "", ",")[[1]])
            labels <- labels[labels != ""]
            n <- length(labels)

            if (n == 0) return(NULL)

            color_inputs <- purrr::map(seq_len(n), function(i) {
                input_id <- ns(paste0("color_", i))
                current_val <- input[[paste0("color_", i)]]
                default_val <- if (is.null(current_val)) {
                    default_colors[((i - 1) %% length(default_colors)) + 1]
                } else current_val

                shiny::div(
                    class = "mb-2",
                    shiny::fluidRow(
                        shiny::column(6, shiny::tags$small(labels[i])),
                        shiny::column(6, shiny::textInput(input_id, NULL, value = default_val, width = "100%"))
                    )
                )
            })

            shiny::tagList(color_inputs)
        })

        # Get legend colors as named vector
        legend_colors <- shiny::reactive({
            shiny::req(input$enabled)

            labels <- trimws(strsplit(input$labels %||% "", ",")[[1]])
            labels <- labels[labels != ""]
            n <- length(labels)

            if (n == 0) return(NULL)

            colors <- purrr::map_chr(seq_len(n), function(i) {
                input[[paste0("color_", i)]] %||% default_colors[((i - 1) %% length(default_colors)) + 1]
            }) |> stats::setNames(labels)
            colors
        })

        # Create legend block plot
        legend_plot <- shiny::reactive({
            if (!(input$enabled %||% FALSE)) return(NULL)

            colors <- legend_colors()
            if (is.null(colors) || length(colors) == 0) return(NULL)

            pos <- input$position %||% "above"
            orientation <- get_legend_orientation(pos)
            h_align <- if (pos == "left") "left"
                       else if (pos == "right") "right"
                       else (input$halign %||% "right")

            legend_block(
                colors,
                halign = h_align,
                valign = "top",
                orientation = orientation,
                sep = input$sep %||% " | ",
                size = input$size %||% 10,
                bold = input$bold %||% TRUE,
                uppercase = input$uppercase %||% FALSE,
                lineheight = input$lineheight %||% 1.6,
                width = 0.95,
                wrap_width = if ((input$wrap %||% 0) > 0) input$wrap else NULL
            )
        })

        list(
            enabled = shiny::reactive(input$enabled %||% FALSE),
            plot = legend_plot,
            colors = legend_colors,
            position = shiny::reactive(input$position %||% "above"),
            width = shiny::reactive(input$width %||% 0.12),
            halign = shiny::reactive(input$halign %||% "right"),
            sep = shiny::reactive(input$sep %||% " | "),
            size = shiny::reactive(input$size %||% 10),
            bold = shiny::reactive(input$bold %||% TRUE),
            uppercase = shiny::reactive(input$uppercase %||% FALSE),
            lineheight = shiny::reactive(input$lineheight %||% 1.6),
            wrap = shiny::reactive(input$wrap %||% 0),
            labels = shiny::reactive(input$labels %||% "")
        )
    })
}
