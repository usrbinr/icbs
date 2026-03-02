# Helper functions for story_designer
# These are internal utilities used by the Shiny app

#' Convert named colors to hex in marquee syntax
#' @noRd
convert_named_colors <- function(text) {

    if (is.null(text) || text == "") return(text)
    pattern <- "\\{([a-zA-Z][a-zA-Z0-9]*)\\s+([^}]+)\\}"
    matches <- gregexpr(pattern, text, perl = TRUE)
    if (matches[[1]][1] == -1) return(text)
    result <- text
    all_matches <- regmatches(text, matches)[[1]]
    for (match in all_matches) {
        parts <- regmatches(match, regexec(pattern, match, perl = TRUE))[[1]]
        color_name <- parts[2]
        content <- parts[3]
        tryCatch({
            rgb_vals <- grDevices::col2rgb(color_name)
            hex_code <- sprintf("#%02X%02X%02X", rgb_vals[1], rgb_vals[2], rgb_vals[3])
            replacement <- paste0("{", hex_code, " ", content, "}")
            result <- sub(match, replacement, result, fixed = TRUE)
        }, error = function(e) { })
    }
    result
}

#' Get palette names for a given package
#' @noRd
get_palette_names <- function(pkg) {
    switch(pkg,
        "ggsci" = if (requireNamespace("ggsci", quietly = TRUE)) {
            c("npg", "aaas", "nejm", "lancet", "jama", "jco", "ucscgb",
              "d3", "locuszoom", "igv", "uchicago", "startrek", "tron",
              "futurama", "rickandmorty", "simpsons", "gsea", "material")
        } else character(0),
        "MetBrewer" = if (requireNamespace("MetBrewer", quietly = TRUE)) {
            names(MetBrewer::MetPalettes)
        } else character(0),
        "nord" = if (requireNamespace("nord", quietly = TRUE)) {
            names(nord::nord_palettes)
        } else character(0),
        "PNWColors" = if (requireNamespace("PNWColors", quietly = TRUE)) {
            names(PNWColors::pnw_palettes)
        } else character(0),
        "rcartocolor" = if (requireNamespace("rcartocolor", quietly = TRUE)) {
            rcartocolor::cartocolors$Name
        } else character(0),
        "RColorBrewer" = if (requireNamespace("RColorBrewer", quietly = TRUE)) {
            rownames(RColorBrewer::brewer.pal.info)
        } else character(0),
        "scico" = if (requireNamespace("scico", quietly = TRUE)) {
            scico::scico_palette_names()
        } else character(0),
        "viridis" = c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo"),
        "wesanderson" = if (requireNamespace("wesanderson", quietly = TRUE)) {
            names(wesanderson::wes_palettes)
        } else character(0),
        character(0)
    )
}

#' Get colors from a palette
#' @noRd
get_palette_colors <- function(pkg, name, n = 8) {
    tryCatch({
        switch(pkg,
            "ggsci" = {
                pal_fn <- get(paste0("pal_", name), envir = asNamespace("ggsci"))
                pal_fn()(n)
            },
            "MetBrewer" = MetBrewer::met.brewer(name, n),
            "nord" = nord::nord(name, n),
            "PNWColors" = PNWColors::pnw_palette(name, n),
            "rcartocolor" = rcartocolor::carto_pal(n, name),
            "RColorBrewer" = {
                max_n <- RColorBrewer::brewer.pal.info[name, "maxcolors"]
                RColorBrewer::brewer.pal(min(n, max_n), name)
            },
            "scico" = scico::scico(n, palette = name),
            "viridis" = if (requireNamespace("viridis", quietly = TRUE)) {
                viridis::viridis(n, option = name)
            } else grDevices::hcl.colors(n, name),
            "wesanderson" = wesanderson::wes_palette(name, n, type = "continuous"),
            grDevices::hcl.colors(n)
        )
    }, error = function(e) grDevices::hcl.colors(n))
}

#' Default colors for legend
#' @noRd
default_legend_colors <- c("#808080", "#B0B0B0", "#E69F00", "#56B4E9", "#009E73",
                           "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

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
    for (id in c("title_size", "subtitle_size", "narrative_size", "caption_size",
                 "title_margin_bottom", "subtitle_margin_bottom", "narrative_width",
                 "title_height", "subtitle_height", "caption_height",
                 "axis_title_x_size", "axis_title_x_margin", "axis_title_y_size",
                 "axis_title_y_margin", "axis_text_size")) {
        shiny::updateSliderInput(session, id, value = defaults[[id]])
    }

    # Numeric inputs
    for (id in c("title_lineheight", "subtitle_lineheight", "narrative_lineheight", "narrative_padding")) {
        shiny::updateNumericInput(session, id, value = defaults[[id]])
    }

    # Select inputs
    for (id in c("narrative_position", "title_align", "subtitle_align", "narrative_halign",
                 "narrative_valign", "caption_position", "plot_theme", "plot_legend_pos",
                 "palette_package", "axis_title_x_align", "axis_title_x_angle",
                 "axis_title_y_align", "axis_title_y_angle", "grid_major", "grid_minor")) {
        shiny::updateSelectInput(session, id, selected = defaults[[id]])
    }

    # Checkbox inputs
    for (id in c("axis_title_x_bold", "axis_title_y_bold", "show_axis_line",
                 "show_ticks", "grid_remove_all")) {
        shiny::updateCheckboxInput(session, id, value = defaults[[id]])
    }

    # Text inputs (colors)
    for (id in c("caption_color", "axis_title_x_color", "axis_title_y_color",
                 "axis_text_color", "axis_line_color", "grid_color")) {
        shiny::updateTextInput(session, id, value = defaults[[id]])
    }
}

#' Build theme modifications from input values
#' @noRd
build_theme_mods <- function(input, `%||%`) {
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

#' Apply color scales to a plot
#' @noRd
apply_color_scales <- function(p, colors, apply_to, scale_type) {
    if (is.null(colors) || length(colors) == 0) return(p)

    tryCatch({
        p_new <- p
        if (scale_type == "discrete") {
            if (apply_to %in% c("fill", "both")) {
                p_new <- p_new + ggplot2::scale_fill_manual(values = colors)
            }
            if (apply_to %in% c("color", "both")) {
                p_new <- p_new + ggplot2::scale_color_manual(values = colors)
            }
        } else if (length(colors) >= 2) {
            if (apply_to %in% c("fill", "both")) {
                p_new <- p_new + ggplot2::scale_fill_gradientn(colors = colors)
            }
            if (apply_to %in% c("color", "both")) {
                p_new <- p_new + ggplot2::scale_color_gradientn(colors = colors)
            }
        }
        ggplot2::ggplot_build(p_new)  # Test if valid
        p_new
    }, error = function(e) p)
}
