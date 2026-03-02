# Code Generation Functions
# Generate R code strings for story_designer's Code tab

#' Generate theme code string
#' @noRd
generate_theme_code <- function(input, `%||%`) {
    theme_name <- input$plot_theme %||% "stwd"
    theme_fn <- switch(theme_name,
        "stwd" = "theme_stwd()",
        "void" = "theme_void()",
        "theme_stwd()"
    )

    # Axis titles
    x_title_face <- if (input$axis_title_x_bold %||% FALSE) "bold" else "plain"
    x_title_angle <- input$axis_title_x_angle %||% "0"
    x_title_margin <- input$axis_title_x_margin %||% 5
    y_title_face <- if (input$axis_title_y_bold %||% FALSE) "bold" else "plain"
    y_title_angle <- input$axis_title_y_angle %||% "90"
    y_title_margin <- input$axis_title_y_margin %||% 5

    # Axis line and ticks
    axis_line_code <- ""
    if (input$show_axis_line %||% FALSE) {
        axis_line_code <- paste0(axis_line_code, ',\n        axis.line = element_line(color = "',
                                  input$axis_line_color %||% "#333333", '")')
    }
    if (input$show_ticks %||% FALSE) {
        axis_line_code <- paste0(axis_line_code, ',\n        axis.ticks = element_line(color = "',
                                  input$axis_line_color %||% "#333333", '")')
    } else {
        axis_line_code <- paste0(axis_line_code, ',\n        axis.ticks = element_blank()')
    }

    # Grid lines
    grid_code <- generate_grid_code(input, `%||%`)

    paste0(
        'styled_plot <- my_plot +\n',
        '    ', theme_fn, ' +\n',
        '    theme(\n',
        '        axis.title.x = element_text(\n',
        '            size = ', input$axis_title_x_size %||% 11, ', face = "', x_title_face, '",\n',
        '            hjust = ', input$axis_title_x_align %||% "0.5", ', angle = ', x_title_angle, ',\n',
        '            color = "', input$axis_title_x_color %||% "#333333", '",\n',
        '            margin = margin(t = ', x_title_margin, ')\n',
        '        ),\n',
        '        axis.title.y = element_text(\n',
        '            size = ', input$axis_title_y_size %||% 11, ', face = "', y_title_face, '",\n',
        '            hjust = ', input$axis_title_y_align %||% "0.5", ', angle = ', y_title_angle, ',\n',
        '            color = "', input$axis_title_y_color %||% "#333333", '",\n',
        '            margin = margin(r = ', y_title_margin, ')\n',
        '        ),\n',
        '        axis.text = element_text(size = ', input$axis_text_size %||% 10,
        ', color = "', input$axis_text_color %||% "#666666", '"),\n',
        '        legend.position = "', input$plot_legend_pos %||% "right", '"',
        axis_line_code, grid_code, '\n',
        '    )'
    )
}

#' Generate grid code string
#' @noRd
generate_grid_code <- function(input, `%||%`) {
    if (input$grid_remove_all %||% FALSE) {
        return(',\n        panel.grid.major = element_blank(),\n        panel.grid.minor = element_blank()')
    }

    grid_major <- input$grid_major %||% "both"
    grid_minor <- input$grid_minor %||% "none"
    grid_color <- input$grid_color %||% "#E5E5E5"
    grid_lines <- c()

    # Major grid
    if (grid_major == "none") {
        grid_lines <- c(grid_lines, 'panel.grid.major = element_blank()')
    } else if (grid_major == "h") {
        grid_lines <- c(grid_lines,
                        paste0('panel.grid.major.y = element_line(color = "', grid_color, '")'),
                        'panel.grid.major.x = element_blank()')
    } else if (grid_major == "v") {
        grid_lines <- c(grid_lines,
                        'panel.grid.major.y = element_blank()',
                        paste0('panel.grid.major.x = element_line(color = "', grid_color, '")'))
    }

    # Minor grid
    if (grid_minor == "none") {
        grid_lines <- c(grid_lines, 'panel.grid.minor = element_blank()')
    } else if (grid_minor == "h") {
        grid_lines <- c(grid_lines,
                        paste0('panel.grid.minor.y = element_line(color = "', grid_color, '")'),
                        'panel.grid.minor.x = element_blank()')
    } else if (grid_minor == "v") {
        grid_lines <- c(grid_lines,
                        'panel.grid.minor.y = element_blank()',
                        paste0('panel.grid.minor.x = element_line(color = "', grid_color, '")'))
    } else if (grid_minor == "both") {
        grid_lines <- c(grid_lines,
                        paste0('panel.grid.minor = element_line(color = "', grid_color, '")'))
    }

    if (length(grid_lines) > 0) {
        paste0(',\n        ', paste(grid_lines, collapse = ',\n        '))
    } else ""
}

#' Generate palette scale code
#' @noRd
generate_palette_code <- function(pkg, pal_name, apply_to, scale_type) {
    if (pkg == "none") return("")

    pal_fn <- switch(pkg,
        "ggsci" = paste0('ggsci::pal_', pal_name, '()(8)'),
        "MetBrewer" = paste0('MetBrewer::met.brewer("', pal_name, '")'),
        "nord" = paste0('nord::nord("', pal_name, '", 8)'),
        "PNWColors" = paste0('PNWColors::pnw_palette("', pal_name, '", 8)'),
        "rcartocolor" = paste0('rcartocolor::carto_pal(8, "', pal_name, '")'),
        "RColorBrewer" = paste0('RColorBrewer::brewer.pal(8, "', pal_name, '")'),
        "scico" = paste0('scico::scico(8, palette = "', pal_name, '")'),
        "viridis" = paste0('viridis::viridis(8, option = "', pal_name, '")'),
        "wesanderson" = paste0('wesanderson::wes_palette("', pal_name, '", 8, type = "continuous")'),
        'viridis::viridis(8)'
    )

    scale_code <- ""
    if (scale_type == "discrete") {
        if (apply_to %in% c("fill", "both")) {
            scale_code <- paste0(scale_code, ' +\n    scale_fill_manual(values = ', pal_fn, ')')
        }
        if (apply_to %in% c("color", "both")) {
            scale_code <- paste0(scale_code, ' +\n    scale_color_manual(values = ', pal_fn, ')')
        }
    } else {
        if (apply_to %in% c("fill", "both")) {
            scale_code <- paste0(scale_code, ' +\n    scale_fill_gradientn(colors = ', pal_fn, ')')
        }
        if (apply_to %in% c("color", "both")) {
            scale_code <- paste0(scale_code, ' +\n    scale_color_gradientn(colors = ', pal_fn, ')')
        }
    }
    scale_code
}

#' Generate block code for a text component
#' @noRd
generate_block_code <- function(type, text, size, halign, lineheight,
                                 margin_bottom = NULL, wrap_width = 0,
                                 valign = NULL, padding = NULL, color = NULL) {
    text_escaped <- gsub('"', '\\"', text)
    if (type == "narrative") {
        text_escaped <- gsub('\n', '\\n', text_escaped)
    }

    fn_name <- switch(type,
        "title" = "title_block",
        "subtitle" = "subtitle_block",
        "narrative" = "text_narrative",
        "caption" = "caption_block"
    )

    size_arg <- switch(type,
        "title" = "title_size",
        "subtitle" = "subtitle_size",
        "narrative" = "size",
        "caption" = "caption_size"
    )

    code <- paste0(type, '_plot <- ', fn_name, '(\n',
                   '    "', text_escaped, '",\n',
                   '    ', size_arg, ' = ', size)

    if (!is.null(halign)) {
        code <- paste0(code, ',\n    halign = "', halign, '"')
    }
    if (!is.null(valign)) {
        code <- paste0(code, ',\n    valign = "', valign, '"')
    }
    if (!is.null(lineheight)) {
        code <- paste0(code, ',\n    lineheight = ', lineheight)
    }
    if (!is.null(margin_bottom)) {
        code <- paste0(code, ',\n    margin_bottom = ', margin_bottom)
    }
    if (!is.null(padding)) {
        code <- paste0(code, ',\n    padding = ', padding)
    }
    if (!is.null(color)) {
        code <- paste0(code, ',\n    color = "', color, '"')
    }
    if (wrap_width > 0) {
        code <- paste0(code, ',\n    wrap_width = ', wrap_width)
    }

    paste0(code, '\n)\n')
}

#' Generate legend block code
#' @noRd
generate_legend_code <- function(labels, colors, halign, orientation, sep,
                                  size, bold, uppercase, lineheight = NULL, wrap_width = 0) {
    color_vec <- paste0('c(', paste0('"', labels, '" = "', colors, '"', collapse = ', '), ')')

    code <- paste0(
        'legend_plot <- legend_block(\n',
        '    ', color_vec, ',\n',
        '    halign = "', halign, '",\n',
        '    orientation = "', orientation, '",\n',
        '    sep = "', sep, '",\n',
        '    size = ', size, ',\n',
        '    bold = ', if (bold) "TRUE" else "FALSE", ',\n',
        '    uppercase = ', if (uppercase) "TRUE" else "FALSE"
    )

    if (!is.null(lineheight) && orientation == "vertical") {
        code <- paste0(code, ',\n    lineheight = ', lineheight)
    }
    if (wrap_width > 0) {
        code <- paste0(code, ',\n    wrap_width = ', wrap_width)
    }

    paste0(code, '\n)\n')
}

#' Generate final patchwork composition code
#' @noRd
generate_composition_code <- function(heights, narrative_width, legend_enabled = FALSE,
                                       legend_pos = "above", legend_width = 0.12) {
    h <- heights

    code <- paste0(
        '# Combine styled plot + narrative\n',
        'content <- styled_plot + narrative_plot +\n',
        '    plot_layout(widths = c(', round(1 - narrative_width, 2), ', ', narrative_width, '))\n\n',
        '# Stack everything\n'
    )

    if (legend_enabled) {
        content_h <- round(1 - h$title - h$subtitle -
                           (if (legend_pos %in% c("above", "below")) h$legend else 0) - h$caption, 3)

        if (legend_pos == "above") {
            code <- paste0(code,
                'final <- title_plot / subtitle_plot / legend_plot / content / caption_plot +\n',
                '    plot_layout(heights = c(', h$title, ', ', h$subtitle, ', ', h$legend, ', ',
                content_h, ', ', h$caption, '))')
        } else if (legend_pos == "below") {
            code <- paste0(code,
                'final <- title_plot / subtitle_plot / content / legend_plot / caption_plot +\n',
                '    plot_layout(heights = c(', h$title, ', ', h$subtitle, ', ', content_h, ', ',
                h$legend, ', ', h$caption, '))')
        } else if (legend_pos == "right") {
            code <- paste0(code,
                'content_with_legend <- patchwork::wrap_plots(content, legend_plot, widths = c(',
                round(1 - legend_width, 2), ', ', legend_width, '))\n',
                'final <- title_plot / subtitle_plot / content_with_legend / caption_plot +\n',
                '    plot_layout(heights = c(', h$title, ', ', h$subtitle, ', ', content_h, ', ', h$caption, '))')
        } else {
            code <- paste0(code,
                'content_with_legend <- patchwork::wrap_plots(legend_plot, content, widths = c(',
                legend_width, ', ', round(1 - legend_width, 2), '))\n',
                'final <- title_plot / subtitle_plot / content_with_legend / caption_plot +\n',
                '    plot_layout(heights = c(', h$title, ', ', h$subtitle, ', ', content_h, ', ', h$caption, '))')
        }
    } else {
        code <- paste0(code,
            'final <- title_plot / subtitle_plot / content / caption_plot +\n',
            '    plot_layout(heights = c(', h$title, ', ', h$subtitle, ', ',
            round(1 - h$title - h$subtitle - h$caption, 3), ', ', h$caption, '))')
    }

    code
}
