# UI helper functions for story_designer
# These reduce boilerplate without requiring full module namespacing

#' Create a text block accordion panel
#' @noRd
text_panel <- function(id, label, badge_color, icon_name, default_text,
                       rows = 2, tooltip = "**bold** | *italic* | {#hex color}",
                       show_margin = TRUE, size_min = 8, size_max = 16, size_default = 11,
                       extra_controls = NULL) {
    # Use textInput for single-line, textAreaInput for multi-line
    text_input <- if (rows <= 1) {
        shiny::textInput(paste0(id, "_text"), NULL, value = default_text, width = "100%")
    } else {
        shiny::textAreaInput(paste0(id, "_text"), NULL, value = default_text,
                             rows = rows, width = "100%",
                             placeholder = "Use **bold** or {#E69F00 color}")
    }

    controls <- shiny::tagList(
        shiny::div(
            class = "d-flex align-items-center gap-2 mb-1",
            shiny::span("Text", class = "small"),
            bslib::tooltip(shiny::icon("circle-info", class = "text-muted"), tooltip)
        ),
        text_input,
        shiny::sliderInput(paste0(id, "_size"), "Font size",
                           min = size_min, max = size_max, value = size_default, step = 1)
    )

    if (show_margin) {
        controls <- shiny::tagList(
            controls,
            shiny::sliderInput(paste0(id, "_margin_bottom"), "Space below (pt)",
                               min = 0, max = 30, value = 5, step = 1)
        )
    }

    if (!is.null(extra_controls)) {
        controls <- shiny::tagList(controls, extra_controls)
    }

    bslib::accordion_panel(
        title = shiny::span(shiny::span(class = paste0("badge bg-", badge_color, " me-2"), " "), label),
        value = label,
        icon = shiny::icon(icon_name),
        controls
    )
}

#' Create the narrative panel with position controls
#' @noRd
narrative_panel <- function(default_text) {
    bslib::accordion_panel(
        title = shiny::span(shiny::span(class = "badge bg-success me-2", " "), "Narrative"),
        value = "Narrative",
        icon = shiny::icon("align-left"),
        shiny::div(
            class = "d-flex align-items-center gap-2 mb-1",
            shiny::span("Text", class = "small"),
            bslib::tooltip(shiny::icon("circle-info", class = "text-muted"),
                           "**bold** | *italic* | {#hex color} | Use blank lines for paragraphs")
        ),
        shiny::textAreaInput("narrative_text", NULL, value = default_text, rows = 4, width = "100%"),
        shiny::selectInput("narrative_position", "Layout",
            choices = c("Chart LEFT | Narrative RIGHT" = "right",
                        "Narrative LEFT | Chart RIGHT" = "left",
                        "Chart TOP | Narrative BOTTOM" = "bottom",
                        "Narrative TOP | Chart BOTTOM" = "top"),
            selected = "right"),
        shiny::uiOutput("width_display"),
        shiny::sliderInput("narrative_width", NULL, min = 0.15, max = 0.50, value = 0.35, step = 0.05),
        shiny::sliderInput("narrative_size", "Font size", min = 8, max = 14, value = 10, step = 1)
    )
}

#' Create the legend panel
#' @noRd
legend_panel <- function() {
    bslib::accordion_panel(
        title = shiny::span(shiny::span(class = "badge bg-danger me-2", " "), "Legend"),
        value = "Legend",
        icon = shiny::icon("palette"),
        shiny::checkboxInput("legend_enabled", "Enable text legend", value = FALSE),
        shiny::conditionalPanel(
            condition = "input.legend_enabled",
            shiny::selectInput("legend_position", "Position", width = "100%",
                choices = c("Above chart" = "above", "Below chart" = "below",
                            "Right of chart (vertical)" = "right", "Left of chart (vertical)" = "left")),
            shiny::textInput("legend_labels", "Categories (comma-separated)",
                             value = "Category A, Category B, Category C", width = "100%"),
            shiny::div(
                class = "d-flex align-items-center gap-2 mb-1 mt-2",
                shiny::span("Colors", class = "small"),
                bslib::tooltip(shiny::icon("circle-info", class = "text-muted"),
                               "Use hex (#808080) or color names (red, steelblue)")
            ),
            shiny::uiOutput("legend_color_inputs"),
            shiny::selectInput("legend_sep", "Separator", width = "100%",
                choices = c("Pipe ( | )" = " | ", "Bullet" = " \u2022 ",
                            "Dash ( - )" = " - ", "None" = "  ")),
            shiny::selectInput("legend_halign", "Text alignment", width = "100%",
                choices = c("Right" = "right", "Center" = "center", "Left" = "left")),
            shiny::sliderInput("legend_size", "Font size", min = 8, max = 14, value = 10, step = 1),
            shiny::sliderInput("legend_wrap", "Wrap at chars (0=off)", min = 0, max = 20, value = 0, step = 1),
            shiny::conditionalPanel(
                condition = "input.legend_position == 'left' || input.legend_position == 'right'",
                shiny::sliderInput("legend_width", "Legend width", min = 0.08, max = 0.25, value = 0.12, step = 0.02),
                shiny::sliderInput("legend_lineheight", "Line spacing", min = 1.0, max = 3.0, value = 1.6, step = 0.2)
            ),
            shiny::checkboxInput("legend_bold", "Bold", value = TRUE),
            shiny::checkboxInput("legend_uppercase", "Uppercase", value = FALSE)
        )
    )
}

#' Create the plot theme panel
#' @noRd
plot_panel <- function() {
    bslib::accordion_panel(
        title = shiny::span(shiny::span(class = "badge bg-warning me-2", " "), "Plot"),
        value = "Plot",
        icon = shiny::icon("chart-bar"),
        shiny::selectInput("plot_theme", "Theme", width = "100%",
            choices = c("STWD" = "stwd", "Void" = "void")),
        shiny::selectInput("plot_legend_pos", "Legend", width = "100%",
            choices = c("Right" = "right", "Bottom" = "bottom",
                        "Top" = "top", "Left" = "left", "None" = "none"))
    )
}

#' Create the color palette panel
#' @noRd
palette_panel <- function() {
    bslib::accordion_panel(
        title = shiny::span(shiny::span(class = "badge bg-purple me-2", " "), "Color Palette"),
        value = "Color Palette",
        icon = shiny::icon("swatchbook"),
        shiny::selectInput("palette_package", "Package", width = "100%",
            choices = c("None" = "none",
                        "ggsci (Scientific)" = "ggsci",
                        "MetBrewer (Art)" = "MetBrewer",
                        "nord (Arctic)" = "nord",
                        "PNWColors (Pacific NW)" = "PNWColors",
                        "rcartocolor (Carto)" = "rcartocolor",
                        "RColorBrewer (Classic)" = "RColorBrewer",
                        "scico (Scientific)" = "scico",
                        "viridis (Colorblind)" = "viridis",
                        "wesanderson (Films)" = "wesanderson")),
        shiny::conditionalPanel(
            condition = "input.palette_package != 'none'",
            shiny::uiOutput("palette_choices"),
            shiny::div(
                class = "d-flex gap-1 mb-2",
                shiny::actionButton("palette_prev", "", icon = shiny::icon("chevron-left"), class = "btn-sm btn-outline-secondary"),
                shiny::actionButton("palette_next", "", icon = shiny::icon("chevron-right"), class = "btn-sm btn-outline-secondary"),
                shiny::actionButton("palette_random", "Random", icon = shiny::icon("shuffle"), class = "btn-sm btn-outline-secondary")
            ),
            shiny::uiOutput("palette_preview"),
            shiny::uiOutput("palette_warning"),
            shiny::radioButtons("palette_apply", "Apply to:",
                choices = c("Fill" = "fill", "Color" = "color", "Both" = "both"),
                selected = "fill", inline = TRUE),
            shiny::radioButtons("palette_scale", "Scale type:",
                choices = c("Discrete" = "discrete", "Continuous" = "continuous"),
                selected = "discrete", inline = TRUE)
        )
    )
}

#' Create the manual colors panel
#' @noRd
manual_colors_panel <- function() {
    bslib::accordion_panel(
        title = shiny::span(shiny::span(class = "badge bg-dark me-2", " "), "Manual Colors"),
        value = "Manual Colors",
        icon = shiny::icon("eye-dropper"),
        shiny::checkboxInput("manual_colors_enabled", "Enable manual coloring", value = FALSE),
        shiny::conditionalPanel(
            condition = "input.manual_colors_enabled",
            shiny::div(
                class = "mb-2",
                shiny::textInput("default_color", "Default color (unassigned)", value = "#808080", width = "100%"),
                bslib::tooltip(shiny::icon("circle-info", class = "text-muted"),
                               "Categories without a specific color will use this")
            ),
            shiny::radioButtons("manual_apply", "Apply to:",
                choices = c("Fill" = "fill", "Color" = "color", "Both" = "both"),
                selected = "fill", inline = TRUE),
            shiny::hr(class = "my-2"),
            shiny::div(
                class = "d-flex justify-content-between align-items-center mb-2",
                shiny::tags$small("Assign colors by:", class = "text-muted"),
                shiny::radioButtons("assign_mode", NULL,
                    choices = c("#" = "number", "Name" = "name"),
                    selected = "number", inline = TRUE)
            ),
            shiny::uiOutput("category_assignments")
        )
    )
}

#' Create the axis & grid panel (consolidated)
#' @noRd
axis_grid_panel <- function() {
    bslib::accordion_panel(
        title = shiny::span(shiny::span(class = "badge bg-warning me-2", " "), "Axis & Grid"),
        value = "Axis & Grid",
        icon = shiny::icon("sliders-h"),

        # X-Axis Title
        shiny::div(class = "d-flex justify-content-between align-items-center",
            shiny::strong(class = "small", "X-Axis Title"),
            shiny::checkboxInput("axis_title_x_bold", "Bold", value = FALSE, width = "auto")
        ),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::sliderInput("axis_title_x_size", "Size", min = 8, max = 16, value = 11, step = 1),
            shiny::selectInput("axis_title_x_align", "Align", width = "100%",
                choices = c("Center" = "0.5", "Left" = "0", "Right" = "1"))
        ),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::selectInput("axis_title_x_angle", "Rotate", width = "100%",
                choices = c("0\u00B0" = "0", "90\u00B0" = "90")),
            shiny::textInput("axis_title_x_color", "Color", value = "#333333", width = "100%")
        ),
        shiny::sliderInput("axis_title_x_margin", "Spacing", min = 0, max = 20, value = 5, step = 1),

        shiny::hr(class = "my-2"),

        # Y-Axis Title
        shiny::div(class = "d-flex justify-content-between align-items-center",
            shiny::strong(class = "small", "Y-Axis Title"),
            shiny::checkboxInput("axis_title_y_bold", "Bold", value = FALSE, width = "auto")
        ),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::sliderInput("axis_title_y_size", "Size", min = 8, max = 16, value = 11, step = 1),
            shiny::selectInput("axis_title_y_align", "Align", width = "100%",
                choices = c("Center" = "0.5", "Bottom" = "0", "Top" = "1"))
        ),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::selectInput("axis_title_y_angle", "Rotate", width = "100%",
                choices = c("90\u00B0" = "90", "0\u00B0" = "0")),
            shiny::textInput("axis_title_y_color", "Color", value = "#333333", width = "100%")
        ),
        shiny::sliderInput("axis_title_y_margin", "Spacing", min = 0, max = 20, value = 5, step = 1),

        shiny::hr(class = "my-2"),

        # Axis Text
        shiny::strong(class = "small", "Axis Text"),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::sliderInput("axis_text_size", "Size", min = 7, max = 14, value = 10, step = 1),
            shiny::textInput("axis_text_color", "Color", value = "#666666", width = "100%")
        ),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::checkboxInput("show_axis_line", "Axis line", value = FALSE),
            shiny::checkboxInput("show_ticks", "Tick marks", value = FALSE)
        ),
        shiny::textInput("axis_line_color", "Line/tick color", value = "#333333", width = "100%"),

        shiny::hr(class = "my-2"),

        # Grid Lines
        shiny::div(class = "d-flex justify-content-between align-items-center",
            shiny::strong(class = "small", "Grid Lines"),
            shiny::checkboxInput("grid_remove_all", "Remove all", value = FALSE, width = "auto")
        ),
        shiny::conditionalPanel(
            condition = "!input.grid_remove_all",
            bslib::layout_column_wrap(
                width = 1/2,
                shiny::selectInput("grid_major", "Major", width = "100%",
                    choices = c("Both" = "both", "H only" = "h", "V only" = "v", "None" = "none")),
                shiny::selectInput("grid_minor", "Minor", width = "100%",
                    choices = c("None" = "none", "Both" = "both", "H only" = "h", "V only" = "v"))
            ),
            shiny::textInput("grid_color", "Grid color", value = "#E5E5E5", width = "100%")
        )
    )
}
