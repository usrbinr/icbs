# Shiny modules for story_designer
# These modularize repeated UI patterns for better maintainability

#' Text Block Module UI
#' @noRd
textBlockUI <- function(id, label, badge_color, icon_name, default_text, rows = 2,
                        tooltip_text = "**bold** | *italic* | {#hex color}",
                        show_margin = TRUE, size_range = c(6, 24), size_default = 12) {
    ns <- shiny::NS(id)

    controls <- shiny::tagList(
        shiny::div(
            class = "d-flex align-items-center gap-2 mb-1",
            shiny::span("Text", class = "small"),
            bslib::tooltip(
                shiny::icon("circle-info", class = "text-muted"),
                tooltip_text
            )
        ),
        shiny::textAreaInput(ns("text"), NULL, value = default_text, rows = rows, width = "100%",
                             placeholder = "Use **bold** or {#E69F00 color}"),
        shiny::sliderInput(ns("size"), "Font size", min = size_range[1], max = size_range[2],
                           value = size_default, step = 1)
    )

    if (show_margin) {
        controls <- shiny::tagList(
            controls,
            shiny::sliderInput(ns("margin_bottom"), "Space below (pt)", min = 0, max = 30, value = 5, step = 1)
        )
    }

    bslib::accordion_panel(
        title = shiny::span(shiny::span(class = paste0("badge bg-", badge_color, " me-2"), " "), label),
        value = label,
        icon = shiny::icon(icon_name),
        controls
    )
}

#' Text Block Module Server
#' @noRd
textBlockServer <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        list(
            text = shiny::reactive(input$text),
            size = shiny::reactive(input$size),
            margin_bottom = shiny::reactive(input$margin_bottom)
        )
    })
}

#' Plot Styling Module UI - Consolidates axis and grid controls
#' @noRd
plotStylingUI <- function(id) {
    ns <- shiny::NS(id)

    bslib::accordion_panel(
        title = shiny::span(shiny::span(class = "badge bg-warning me-2", " "), "Plot Styling"),
        value = "Plot Styling",
        icon = shiny::icon("sliders-h"),

        # Theme selection
        shiny::selectInput(ns("theme"), "Theme", width = "100%",
            choices = c("STWD" = "stwd", "Void" = "void")),
        shiny::selectInput(ns("legend_pos"), "Legend Position", width = "100%",
            choices = c("Right" = "right", "Bottom" = "bottom",
                        "Top" = "top", "Left" = "left", "None" = "none")),

        shiny::hr(class = "my-2"),

        # X-Axis Title
        shiny::div(
            class = "d-flex justify-content-between align-items-center",
            shiny::strong(class = "small", "X-Axis Title"),
            bslib::tooltip(
                shiny::icon("circle-info", class = "text-muted"),
                "Style the x-axis label"
            )
        ),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::sliderInput(ns("x_size"), "Size", min = 8, max = 16, value = 11, step = 1),
            shiny::selectInput(ns("x_align"), "Align", width = "100%",
                choices = c("Center" = "0.5", "Left" = "0", "Right" = "1"))
        ),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::checkboxInput(ns("x_bold"), "Bold", value = FALSE),
            shiny::selectInput(ns("x_angle"), "Rotation", width = "100%",
                choices = c("0\u00B0" = "0", "90\u00B0" = "90"))
        ),
        shiny::textInput(ns("x_color"), "Color", value = "#333333", width = "100%"),

        shiny::hr(class = "my-2"),

        # Y-Axis Title
        shiny::div(
            class = "d-flex justify-content-between align-items-center",
            shiny::strong(class = "small", "Y-Axis Title"),
            bslib::tooltip(
                shiny::icon("circle-info", class = "text-muted"),
                "Style the y-axis label"
            )
        ),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::sliderInput(ns("y_size"), "Size", min = 8, max = 16, value = 11, step = 1),
            shiny::selectInput(ns("y_align"), "Align", width = "100%",
                choices = c("Center" = "0.5", "Bottom" = "0", "Top" = "1"))
        ),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::checkboxInput(ns("y_bold"), "Bold", value = FALSE),
            shiny::selectInput(ns("y_angle"), "Rotation", width = "100%",
                choices = c("90\u00B0" = "90", "0\u00B0" = "0"))
        ),
        shiny::textInput(ns("y_color"), "Color", value = "#333333", width = "100%"),

        shiny::hr(class = "my-2"),

        # Axis Text & Lines
        shiny::strong(class = "small", "Axis Text"),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::sliderInput(ns("text_size"), "Size", min = 7, max = 14, value = 10, step = 1),
            shiny::textInput(ns("text_color"), "Color", value = "#666666", width = "100%")
        ),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::checkboxInput(ns("show_axis_line"), "Show axis line", value = FALSE),
            shiny::checkboxInput(ns("show_ticks"), "Show ticks", value = FALSE)
        ),
        shiny::textInput(ns("line_color"), "Line/tick color", value = "#333333", width = "100%"),

        shiny::hr(class = "my-2"),

        # Grid Lines
        shiny::strong(class = "small", "Grid Lines"),
        shiny::checkboxInput(ns("remove_all_grid"), "Remove all grid lines", value = FALSE),
        shiny::conditionalPanel(
            condition = paste0("!input['", ns("remove_all_grid"), "']"),
            ns = ns,
            bslib::layout_column_wrap(
                width = 1/2,
                shiny::selectInput(ns("major_grid"), "Major", width = "100%",
                    choices = c("Both" = "both", "H only" = "h", "V only" = "v", "None" = "none")),
                shiny::selectInput(ns("minor_grid"), "Minor", width = "100%",
                    choices = c("None" = "none", "Both" = "both", "H only" = "h", "V only" = "v"))
            ),
            shiny::textInput(ns("grid_color"), "Grid color", value = "#E5E5E5", width = "100%")
        )
    )
}

#' Plot Styling Module Server
#' @noRd
plotStylingServer <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        list(
            # Theme
            theme = shiny::reactive(input$theme),
            legend_pos = shiny::reactive(input$legend_pos),
            # X-Axis
            x_size = shiny::reactive(input$x_size),
            x_align = shiny::reactive(input$x_align),
            x_bold = shiny::reactive(input$x_bold),
            x_angle = shiny::reactive(input$x_angle),
            x_color = shiny::reactive(input$x_color),
            # Y-Axis
            y_size = shiny::reactive(input$y_size),
            y_align = shiny::reactive(input$y_align),
            y_bold = shiny::reactive(input$y_bold),
            y_angle = shiny::reactive(input$y_angle),
            y_color = shiny::reactive(input$y_color),
            # Text & Lines
            text_size = shiny::reactive(input$text_size),
            text_color = shiny::reactive(input$text_color),
            show_axis_line = shiny::reactive(input$show_axis_line),
            show_ticks = shiny::reactive(input$show_ticks),
            line_color = shiny::reactive(input$line_color),
            # Grid
            remove_all_grid = shiny::reactive(input$remove_all_grid),
            major_grid = shiny::reactive(input$major_grid),
            minor_grid = shiny::reactive(input$minor_grid),
            grid_color = shiny::reactive(input$grid_color)
        )
    })
}

#' Export Module UI
#' @noRd
exportUI <- function(id) {
    ns <- shiny::NS(id)

    bslib::card_body(
        class = "p-3",
        shiny::div(
            class = "row g-3 mb-3",
            shiny::div(
                class = "col-auto",
                shiny::selectInput(ns("format"), "Format", width = "100px",
                    choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                    selected = "png")
            ),
            shiny::div(
                class = "col-auto",
                shiny::numericInput(ns("width"), "Width (in)", value = 12, min = 4, max = 24, step = 0.5, width = "100px")
            ),
            shiny::div(
                class = "col-auto",
                shiny::numericInput(ns("height"), "Height (in)", value = 9, min = 3, max = 18, step = 0.5, width = "100px")
            ),
            shiny::div(
                class = "col-auto",
                shiny::numericInput(ns("dpi"), "DPI", value = 150, min = 72, max = 600, step = 10, width = "80px")
            )
        ),
        shiny::div(
            class = "d-flex gap-2 mb-3",
            shiny::actionButton(ns("validate"), "Validate Actual Size",
                class = "btn-success", icon = shiny::icon("search-plus")),
            shiny::actionButton(ns("popout"), "Pop Out",
                class = "btn-outline-secondary", icon = shiny::icon("external-link-alt")),
            shiny::downloadButton(ns("download"), "Download", class = "btn-primary")
        ),
        shiny::uiOutput(ns("info")),
        shiny::uiOutput(ns("preview"))
    )
}

#' Export Module Server
#' @noRd
exportServer <- function(id, build_layout) {
    shiny::moduleServer(id, function(input, output, session) {
        actual_size_data <- shiny::reactiveVal(NULL)

        # Export info display
        output$info <- shiny::renderUI({
            w <- input$width %||% 12
            h <- input$height %||% 9
            dpi <- input$dpi %||% 150
            fmt <- toupper(input$format %||% "png")
            shiny::div(
                class = "text-muted small mb-2",
                shiny::strong(fmt), " output: ", w, '" x ', h, '" @ ', dpi, ' DPI = ',
                shiny::strong(round(w * dpi), " x ", round(h * dpi), " px")
            )
        })

        # Validate actual size
        shiny::observeEvent(input$validate, {
            shiny::withProgress(message = "Rendering at actual size...", {
                w <- input$width %||% 12
                h <- input$height %||% 9
                dpi <- input$dpi %||% 150
                temp_file <- tempfile(fileext = ".png")
                ggplot2::ggsave(temp_file, build_layout(),
                    width = w, height = h, dpi = dpi, bg = "white")
                img_data <- base64enc::base64encode(temp_file)
                actual_size_data(list(
                    data = img_data,
                    px_width = w * dpi,
                    px_height = h * dpi
                ))
                unlink(temp_file)
            })
        })

        # Actual size preview
        output$preview <- shiny::renderUI({
            img_info <- actual_size_data()
            if (is.null(img_info)) {
                return(shiny::div(
                    class = "mt-3 p-4 text-center text-muted border rounded bg-light",
                    shiny::icon("search-plus", class = "fa-2x mb-2"),
                    shiny::p("Click 'Validate Actual Size' to preview at true export dimensions")
                ))
            }

            shiny::div(
                class = "mt-3 border rounded",
                shiny::div(
                    class = "bg-success text-white p-2 d-flex justify-content-between align-items-center",
                    shiny::span(
                        shiny::icon("check-circle"), " Actual Size ",
                        shiny::span(class = "badge bg-light text-dark",
                            paste0(img_info$px_width, "x", img_info$px_height, "px"))
                    ),
                    shiny::actionButton(session$ns("close"), "Close", class = "btn-sm btn-light")
                ),
                shiny::div(
                    class = "p-1 text-center small text-muted bg-light",
                    "This is EXACTLY what your export will look like. Scroll to inspect."
                ),
                shiny::div(
                    style = "max-height: 600px; overflow: auto; border: 2px solid #28a745;",
                    shiny::tags$img(src = paste0("data:image/png;base64,", img_info$data),
                        style = "max-width: none;")
                )
            )
        })

        shiny::observeEvent(input$close, { actual_size_data(NULL) })

        # Pop out preview
        shiny::observeEvent(input$popout, {
            img_info <- actual_size_data()
            if (is.null(img_info)) {
                shiny::withProgress(message = "Rendering at actual size...", {
                    temp_file <- tempfile(fileext = ".png")
                    ggplot2::ggsave(temp_file, build_layout(),
                        width = input$width %||% 12,
                        height = input$height %||% 9,
                        dpi = input$dpi %||% 150, bg = "white")
                    img_data <- base64enc::base64encode(temp_file)
                    actual_size_data(list(
                        data = img_data,
                        px_width = (input$width %||% 12) * (input$dpi %||% 150),
                        px_height = (input$height %||% 9) * (input$dpi %||% 150)
                    ))
                    unlink(temp_file)
                })
                img_info <- actual_size_data()
            }
            shiny::showModal(shiny::modalDialog(
                title = paste0("Actual Size Preview (", img_info$px_width, "x", img_info$px_height, "px)"),
                size = "xl",
                easyClose = TRUE,
                shiny::div(
                    style = "overflow: auto; max-height: 70vh;",
                    shiny::tags$img(src = paste0("data:image/png;base64,", img_info$data),
                        style = "max-width: none;")
                ),
                footer = shiny::modalButton("Close")
            ))
        })

        # Download handler
        output$download <- shiny::downloadHandler(
            filename = function() {
                fmt <- input$format %||% "png"
                paste0("story_layout_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt)
            },
            content = function(file) {
                w <- input$width %||% 12
                h <- input$height %||% 9
                dpi <- input$dpi %||% 150
                fmt <- input$format %||% "png"
                if (fmt == "pdf") {
                    ggplot2::ggsave(file, build_layout(), width = w, height = h, device = "pdf", bg = "white")
                } else if (fmt == "svg") {
                    ggplot2::ggsave(file, build_layout(), width = w, height = h, device = "svg", bg = "white")
                } else {
                    ggplot2::ggsave(file, build_layout(), width = w, height = h, dpi = dpi, bg = "white")
                }
            }
        )

        # Return reactive values for use in main app
        list(
            format = shiny::reactive(input$format),
            width = shiny::reactive(input$width),
            height = shiny::reactive(input$height),
            dpi = shiny::reactive(input$dpi)
        )
    })
}

#' Color Palette Module UI
#' @noRd
colorPaletteUI <- function(id) {
    ns <- shiny::NS(id)

    bslib::accordion_panel(
        title = shiny::span(shiny::span(class = "badge bg-purple me-2", " "), "Color Palette"),
        value = "Color Palette",
        icon = shiny::icon("swatchbook"),
        shiny::selectInput(ns("package"), "Package", width = "100%",
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
            condition = paste0("input['", ns("package"), "'] != 'none'"),
            shiny::uiOutput(ns("palette_choices")),
            shiny::div(
                class = "d-flex gap-1 mb-2",
                shiny::actionButton(ns("prev"), "", icon = shiny::icon("chevron-left"), class = "btn-sm btn-outline-secondary"),
                shiny::actionButton(ns("next"), "", icon = shiny::icon("chevron-right"), class = "btn-sm btn-outline-secondary"),
                shiny::actionButton(ns("random"), "Random", icon = shiny::icon("shuffle"), class = "btn-sm btn-outline-secondary")
            ),
            shiny::uiOutput(ns("preview")),
            shiny::uiOutput(ns("warning")),
            shiny::radioButtons(ns("apply_to"), "Apply to:",
                choices = c("Fill" = "fill", "Color" = "color", "Both" = "both"),
                selected = "fill", inline = TRUE),
            shiny::radioButtons(ns("scale_type"), "Scale type:",
                choices = c("Discrete" = "discrete", "Continuous" = "continuous"),
                selected = "discrete", inline = TRUE)
        )
    )
}

#' Color Palette Module Server
#' @noRd
colorPaletteServer <- function(id, plot_categories) {
    shiny::moduleServer(id, function(input, output, session) {
        `%||%` <- function(x, y) if (is.null(x)) y else x

        palette_idx <- shiny::reactiveVal(1)
        selected_colors <- shiny::reactiveVal(integer(0))

        # Available palettes for selected package
        available_palettes <- shiny::reactive({
            get_palette_names(input$package %||% "none")
        })

        # Reset on package change
        shiny::observeEvent(input$package, {
            palette_idx(1)
            selected_colors(integer(0))
        })

        # Reset selection on palette change
        shiny::observeEvent(input$name, {
            selected_colors(integer(0))
        })

        # Navigation
        shiny::observeEvent(input$prev, {
            palettes <- available_palettes()
            if (length(palettes) > 0) {
                idx <- palette_idx()
                palette_idx(if (idx <= 1) length(palettes) else idx - 1)
            }
        })

        shiny::observeEvent(input$`next`, {
            palettes <- available_palettes()
            if (length(palettes) > 0) {
                idx <- palette_idx()
                palette_idx(if (idx >= length(palettes)) 1 else idx + 1)
            }
        })

        shiny::observeEvent(input$random, {
            palettes <- available_palettes()
            if (length(palettes) > 0) {
                palette_idx(sample(length(palettes), 1))
            }
        })

        # Sync dropdown with index
        shiny::observeEvent(input$name, {
            palettes <- available_palettes()
            idx <- match(input$name, palettes)
            if (!is.na(idx)) palette_idx(idx)
        })

        # Render palette dropdown
        output$palette_choices <- shiny::renderUI({
            pkg <- input$package %||% "none"
            if (pkg == "none") return(NULL)
            palettes <- available_palettes()
            if (length(palettes) == 0) {
                return(shiny::div(
                    class = "alert alert-warning py-1 px-2 small",
                    shiny::icon("exclamation-triangle"),
                    paste0(" Package '", pkg, "' not installed. Run: install.packages('", pkg, "')")
                ))
            }
            idx <- min(palette_idx(), length(palettes))
            shiny::selectInput(session$ns("name"), "Palette", width = "100%",
                choices = palettes, selected = palettes[idx])
        })

        # Handle swatch clicks
        shiny::observeEvent(input$swatch_click, {
            clicked_idx <- input$swatch_click
            current <- selected_colors()
            if (clicked_idx %in% current) {
                selected_colors(setdiff(current, clicked_idx))
            } else {
                selected_colors(c(current, clicked_idx))
            }
        })

        # Render palette preview
        output$preview <- shiny::renderUI({
            pkg <- input$package %||% "none"
            if (pkg == "none") return(NULL)
            palettes <- available_palettes()
            if (length(palettes) == 0) return(NULL)
            idx <- min(palette_idx(), length(palettes))
            colors <- get_palette_colors(pkg, palettes[idx], 8)
            sel <- selected_colors()

            swatches <- lapply(seq_along(colors), function(i) {
                col <- colors[i]
                is_selected <- i %in% sel
                badge <- if (is_selected) {
                    pos <- which(sel == i)
                    shiny::span(
                        style = "position:absolute;top:-6px;right:-6px;background:#0d6efd;color:white;border-radius:50%;width:14px;height:14px;font-size:9px;display:flex;align-items:center;justify-content:center;",
                        pos
                    )
                } else NULL

                bslib::tooltip(
                    shiny::div(
                        style = "position:relative;display:inline-block;margin-right:4px;",
                        shiny::span(
                            style = paste0(
                                "display:inline-block;width:28px;height:28px;background:", col,
                                ";border:", if (is_selected) "3px solid #0d6efd" else "1px solid #ccc",
                                ";border-radius:4px;cursor:pointer;transition:all 0.15s;"
                            ),
                            onclick = paste0("Shiny.setInputValue('", session$ns("swatch_click"), "', ", i, ", {priority: 'event'});")
                        ),
                        badge
                    ),
                    paste0(col, if (is_selected) " (selected)" else " (click to select)")
                )
            })

            selected_hex <- if (length(sel) > 0) {
                sel_colors <- colors[sel]
                shiny::div(
                    class = "mt-2 small",
                    shiny::tags$code(
                        style = "cursor:pointer;",
                        onclick = paste0("navigator.clipboard.writeText('", paste(sel_colors, collapse = ", "), "');"),
                        paste(sel_colors, collapse = ", ")
                    ),
                    shiny::span(class = "text-muted ms-1", "(click to copy)")
                )
            } else {
                shiny::div(class = "mt-1 small text-muted", "Click swatches to select colors")
            }

            shiny::div(swatches, selected_hex)
        })

        # Current palette colors
        current_palette <- shiny::reactive({
            pkg <- input$package %||% "none"
            if (pkg == "none") return(NULL)
            palettes <- available_palettes()
            if (length(palettes) == 0) return(NULL)
            idx <- min(palette_idx(), length(palettes))
            all_colors <- get_palette_colors(pkg, palettes[idx], 8)
            sel <- selected_colors()
            if (length(sel) > 0) {
                all_colors[sel]
            } else {
                all_colors
            }
        })

        # Warning when categories > colors
        output$warning <- shiny::renderUI({
            pkg <- input$package %||% "none"
            if (pkg == "none") return(NULL)

            cats <- plot_categories()
            palette_colors <- current_palette()
            n_colors <- length(palette_colors)

            apply_to <- input$apply_to %||% "fill"
            n_cats <- if (apply_to == "fill") cats$n_fill else if (apply_to == "color") cats$n_color else max(cats$n_fill, cats$n_color)

            if (n_cats > 0 && n_cats > n_colors) {
                shiny::div(
                    class = "alert alert-warning py-1 px-2 small mb-2",
                    shiny::icon("exclamation-triangle"),
                    paste0(" ", n_cats, " categories but only ", n_colors, " colors. Consider Manual Colors section.")
                )
            } else NULL
        })

        # Return values
        list(
            package = shiny::reactive(input$package),
            name = shiny::reactive({
                palettes <- available_palettes()
                if (length(palettes) == 0) return(NULL)
                palettes[min(palette_idx(), length(palettes))]
            }),
            apply_to = shiny::reactive(input$apply_to),
            scale_type = shiny::reactive(input$scale_type),
            colors = current_palette,
            palette_idx = palette_idx
        )
    })
}
