# Story Designer Defaults and State Management
# Default values and reset functions for the Shiny app

#' Default input values for story_designer
#' @noRd
default_input_values <- list(
    # Text content
    title_text = "**Your {#E69F00 title} here**",
    subtitle_text = "Supporting context for your visualization",
    narrative_text = "**KEY INSIGHT:**\nYour narrative text here.\n\n**ACTION:**\nWhat should the audience do?",
    caption_text = "SOURCE: Your data source",
    # Sizes
    title_size = 12, subtitle_size = 11, narrative_size = 10, caption_size = 9,
    # Margins
    title_margin_bottom = 5, subtitle_margin_bottom = 5,
    # Layout
    narrative_width = 0.35, title_height = 0.08, subtitle_height = 0.06, caption_height = 0.05,
    # Alignment
    narrative_position = "right", title_align = "left", subtitle_align = "left",
    narrative_halign = "left", narrative_valign = "top", caption_position = "full_left",
    # Line heights
    title_lineheight = 1.1, subtitle_lineheight = 1.2, narrative_lineheight = 1.4,
    narrative_padding = 10, caption_color = "#808080",
    # Plot
    plot_theme = "stwd", plot_legend_pos = "right", palette_package = "none",
    # X-Axis
    axis_title_x_size = 11, axis_title_x_bold = FALSE, axis_title_x_align = "0.5",
    axis_title_x_angle = "0", axis_title_x_margin = 5, axis_title_x_color = "#333333",
    # Y-Axis
    axis_title_y_size = 11, axis_title_y_bold = FALSE, axis_title_y_align = "0.5",
    axis_title_y_angle = "90", axis_title_y_margin = 5, axis_title_y_color = "#333333",
    # Axis text & lines
    axis_text_size = 10, axis_text_color = "#666666",
    show_axis_line = FALSE, show_ticks = FALSE, axis_line_color = "#333333",
    # Grid
    grid_remove_all = FALSE, grid_major = "both", grid_minor = "none", grid_color = "#E5E5E5"
)

#' Reset all inputs to defaults
#' @noRd
reset_all_inputs <- function(session, defaults = default_input_values) {
    # Text inputs
    shiny::updateTextAreaInput(session, "title_text", value = defaults$title_text)
    shiny::updateTextAreaInput(session, "subtitle_text", value = defaults$subtitle_text)
    shiny::updateTextAreaInput(session, "narrative_text", value = defaults$narrative_text)
    shiny::updateTextInput(session, "caption_text", value = defaults$caption_text)

    # Sliders
    purrr::walk(
        c("title_size", "subtitle_size", "narrative_size", "caption_size",
          "title_margin_bottom", "subtitle_margin_bottom", "narrative_width",
          "title_height", "subtitle_height", "caption_height",
          "axis_title_x_size", "axis_title_x_margin", "axis_title_y_size",
          "axis_title_y_margin", "axis_text_size"),
        ~ shiny::updateSliderInput(session, .x, value = defaults[[.x]])
    )

    # Numeric inputs
    purrr::walk(
        c("title_lineheight", "subtitle_lineheight", "narrative_lineheight", "narrative_padding"),
        ~ shiny::updateNumericInput(session, .x, value = defaults[[.x]])
    )

    # Select inputs
    purrr::walk(
        c("narrative_position", "title_align", "subtitle_align", "narrative_halign",
          "narrative_valign", "caption_position", "plot_theme", "plot_legend_pos",
          "palette_package", "axis_title_x_align", "axis_title_x_angle",
          "axis_title_y_align", "axis_title_y_angle", "grid_major", "grid_minor"),
        ~ shiny::updateSelectInput(session, .x, selected = defaults[[.x]])
    )

    # Checkbox inputs
    purrr::walk(
        c("axis_title_x_bold", "axis_title_y_bold", "show_axis_line",
          "show_ticks", "grid_remove_all"),
        ~ shiny::updateCheckboxInput(session, .x, value = defaults[[.x]])
    )

    # Text inputs (colors)
    purrr::walk(
        c("caption_color", "axis_title_x_color", "axis_title_y_color",
          "axis_text_color", "axis_line_color", "grid_color"),
        ~ shiny::updateTextInput(session, .x, value = defaults[[.x]])
    )
}

#' Build theme modifications from input values
#' @noRd
build_theme_mods <- function(input) {
    is_void <- (input$plot_theme %||% "stwd") == "void"

    if (is_void) {
        return(ggplot2::theme(legend.position = input$plot_legend_pos %||% "right"))
    }

    # Axis title styling
    x_face <- if (input$axis_title_x_bold %||% FALSE) "bold" else "plain"
    y_face <- if (input$axis_title_y_bold %||% FALSE) "bold" else "plain"
    y_angle <- as.numeric(input$axis_title_y_angle %||% "90")
    y_hjust <- as.numeric(input$axis_title_y_align %||% "0.5")

    theme_mods <- ggplot2::theme(
        axis.title.x = ggplot2::element_text(
            size = input$axis_title_x_size %||% 11, face = x_face,
            hjust = as.numeric(input$axis_title_x_align %||% "0.5"),
            angle = as.numeric(input$axis_title_x_angle %||% "0"),
            color = input$axis_title_x_color %||% "#333333",
            margin = ggplot2::margin(t = input$axis_title_x_margin %||% 5)
        ),
        axis.title.y = ggplot2::element_text(
            size = input$axis_title_y_size %||% 11, face = y_face,
            hjust = if (y_angle == 0) 0.5 else y_hjust,
            vjust = if (y_angle == 0) y_hjust else 0.5,
            angle = y_angle,
            color = input$axis_title_y_color %||% "#333333",
            margin = ggplot2::margin(r = input$axis_title_y_margin %||% 5)
        ),
        axis.text = ggplot2::element_text(
            size = input$axis_text_size %||% 10,
            color = input$axis_text_color %||% "#666666"
        ),
        legend.position = input$plot_legend_pos %||% "right"
    )

    # Axis lines and ticks
    line_color <- input$axis_line_color %||% "#333333"
    if (input$show_axis_line %||% FALSE) {
        theme_mods <- theme_mods + ggplot2::theme(axis.line = ggplot2::element_line(color = line_color))
    }
    theme_mods <- theme_mods + ggplot2::theme(
        axis.ticks = if (input$show_ticks %||% FALSE) {
            ggplot2::element_line(color = line_color)
        } else ggplot2::element_blank()
    )

    # Grid lines
    if (input$grid_remove_all %||% FALSE) {
        theme_mods <- theme_mods + ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
        )
    } else {
        grid_color <- input$grid_color %||% "#E5E5E5"
        grid_major <- input$grid_major %||% "both"
        grid_minor <- input$grid_minor %||% "none"

        major_h <- if (grid_major %in% c("both", "h")) ggplot2::element_line(color = grid_color, linewidth = 0.5) else ggplot2::element_blank()
        major_v <- if (grid_major %in% c("both", "v")) ggplot2::element_line(color = grid_color, linewidth = 0.5) else ggplot2::element_blank()
        minor_h <- if (grid_minor %in% c("both", "h")) ggplot2::element_line(color = grid_color, linewidth = 0.25) else ggplot2::element_blank()
        minor_v <- if (grid_minor %in% c("both", "v")) ggplot2::element_line(color = grid_color, linewidth = 0.25) else ggplot2::element_blank()

        theme_mods <- theme_mods + ggplot2::theme(
            panel.grid.major.y = major_h, panel.grid.major.x = major_v,
            panel.grid.minor.y = minor_h, panel.grid.minor.x = minor_v
        )
    }

    theme_mods
}
