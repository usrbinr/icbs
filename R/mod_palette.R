# Module: Color Palette Selection
# Handles palette package/name selection, swatch display, and manual color assignment

#' Palette Module UI
#' @param id Module namespace ID
#' @noRd
mod_palette_ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
        # Palette selection panel (from story_designer_ui.R palette_panel)
        bslib::accordion_panel(
            title = shiny::span(shiny::span(class = "badge bg-warning me-2", " "), "Color Palette"),
            value = "Color Palette",
            icon = shiny::icon("palette"),

            shiny::selectInput(ns("palette_package"), "Package", width = "100%",
                choices = c("None" = "none", "ggsci", "MetBrewer", "nord", "PNWColors",
                            "rcartocolor", "RColorBrewer", "scico", "viridis", "wesanderson"),
                selected = "none"),

            shiny::conditionalPanel(
                condition = paste0("input['", ns("palette_package"), "'] != 'none'"),
                shiny::uiOutput(ns("palette_choices")),
                shiny::div(
                    class = "d-flex gap-1 mb-2",
                    shiny::actionButton(ns("palette_prev"), "", icon = shiny::icon("chevron-left"),
                        class = "btn-sm btn-outline-secondary"),
                    shiny::actionButton(ns("palette_next"), "", icon = shiny::icon("chevron-right"),
                        class = "btn-sm btn-outline-secondary"),
                    shiny::actionButton(ns("palette_random"), "", icon = shiny::icon("random"),
                        class = "btn-sm btn-outline-secondary")
                ),
                shiny::uiOutput(ns("palette_preview")),
                shiny::uiOutput(ns("palette_warning")),
                bslib::layout_column_wrap(
                    width = 1/2,
                    shiny::selectInput(ns("palette_apply"), "Apply to", width = "100%",
                        choices = c("Fill" = "fill", "Color" = "color", "Both" = "both")),
                    shiny::selectInput(ns("palette_scale"), "Scale", width = "100%",
                        choices = c("Discrete" = "discrete", "Continuous" = "continuous"))
                )
            )
        ),

        # Manual colors panel
        bslib::accordion_panel(
            title = shiny::span(shiny::span(class = "badge bg-dark me-2", " "), "Manual Colors"),
            value = "Manual Colors",
            icon = shiny::icon("paint-brush"),

            shiny::checkboxInput(ns("manual_enabled"), "Enable manual color assignment", value = FALSE),
            shiny::conditionalPanel(
                condition = paste0("input['", ns("manual_enabled"), "']"),
                shiny::div(
                    class = "mb-2",
                    shiny::textInput(ns("default_color"), "Default color (unassigned)", value = "#808080", width = "100%"),
                    shiny::tags$small(class = "text-muted", "Categories without assigned colors get this color")
                ),
                bslib::layout_column_wrap(
                    width = 1/2,
                    shiny::selectInput(ns("manual_apply"), "Apply to", width = "100%",
                        choices = c("Fill" = "fill", "Color" = "color", "Both" = "both")),
                    shiny::selectInput(ns("assign_mode"), "Show as", width = "100%",
                        choices = c("Element #" = "number", "Name" = "name"))
                ),
                shiny::uiOutput(ns("category_assignments"))
            )
        )
    )
}

#' Palette Module Server
#' @param id Module namespace ID
#' @param plot_categories Reactive returning list with fill_levels, color_levels, n_fill, n_color
#' @return List of reactives: current_palette, manual_colors, palette_apply, manual_apply, manual_enabled
#' @noRd
mod_palette_server <- function(id, plot_categories) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        `%||%` <- function(x, y) if (is.null(x)) y else x

        # Default colors
        default_colors <- default_legend_colors

        # --- Palette State ---
        palette_idx <- shiny::reactiveVal(1)
        selected_colors <- shiny::reactiveVal(integer(0))
        manual_color_values <- shiny::reactiveVal(list())

        # Available palettes for selected package
        available_palettes <- shiny::reactive({
            get_palette_names(input$palette_package %||% "none")
        })

        # Reset on package change
        shiny::observeEvent(input$palette_package, {
            palette_idx(1)
            selected_colors(integer(0))
        })

        # Reset selection on palette change
        shiny::observeEvent(input$palette_name, {
            selected_colors(integer(0))
        })

        # Navigation buttons
        shiny::observeEvent(input$palette_prev, {
            palettes <- available_palettes()
            if (length(palettes) > 0) {
                idx <- palette_idx()
                palette_idx(if (idx <= 1) length(palettes) else idx - 1)
            }
        })

        shiny::observeEvent(input$palette_next, {
            palettes <- available_palettes()
            if (length(palettes) > 0) {
                idx <- palette_idx()
                palette_idx(if (idx >= length(palettes)) 1 else idx + 1)
            }
        })

        shiny::observeEvent(input$palette_random, {
            palettes <- available_palettes()
            if (length(palettes) > 0) {
                palette_idx(sample(length(palettes), 1))
            }
        })

        # Sync dropdown with index
        shiny::observeEvent(input$palette_name, {
            palettes <- available_palettes()
            idx <- match(input$palette_name, palettes)
            if (!is.na(idx)) palette_idx(idx)
        })

        # Swatch click handler
        shiny::observeEvent(input$swatch_click, {
            clicked_idx <- input$swatch_click
            current <- selected_colors()
            if (clicked_idx %in% current) {
                selected_colors(setdiff(current, clicked_idx))
            } else {
                selected_colors(c(current, clicked_idx))
            }
        })

        # --- UI Outputs ---
        output$palette_choices <- shiny::renderUI({
            pkg <- input$palette_package %||% "none"
            if (pkg == "none") return(NULL)

            palettes <- available_palettes()
            if (length(palettes) == 0) {
                return(shiny::div(
                    class = "alert alert-warning py-1 px-2 small",
                    shiny::icon("exclamation-triangle"),
                    paste0(" Package '", pkg, "' not installed.")
                ))
            }

            idx <- min(palette_idx(), length(palettes))
            shiny::selectInput(ns("palette_name"), "Palette", width = "100%",
                choices = palettes, selected = palettes[idx])
        })

        output$palette_preview <- shiny::renderUI({
            pkg <- input$palette_package %||% "none"
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
                            onclick = paste0("Shiny.setInputValue('", ns("swatch_click"), "', ", i, ", {priority: 'event'});")
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

        output$palette_warning <- shiny::renderUI({
            pkg <- input$palette_package %||% "none"
            if (pkg == "none") return(NULL)

            cats <- plot_categories()
            palette_colors <- current_palette()
            n_colors <- length(palette_colors)

            apply_to <- input$palette_apply %||% "fill"
            n_cats <- if (apply_to == "fill") cats$n_fill
                      else if (apply_to == "color") cats$n_color
                      else max(cats$n_fill, cats$n_color)

            if (n_cats > 0 && n_cats > n_colors) {
                shiny::div(
                    class = "alert alert-warning py-1 px-2 small mb-2",
                    shiny::icon("exclamation-triangle"),
                    paste0(" ", n_cats, " categories but only ", n_colors, " colors.")
                )
            } else NULL
        })

        # Manual color category assignments
        output$category_assignments <- shiny::renderUI({
            cats <- plot_categories()
            apply_to <- input$manual_apply %||% "fill"

            levels_to_use <- if (apply_to == "fill") cats$fill_levels
                             else if (apply_to == "color") cats$color_levels
                             else if (!is.null(cats$fill_levels)) cats$fill_levels
                             else cats$color_levels

            if (is.null(levels_to_use) || length(levels_to_use) == 0) {
                return(shiny::div(
                    class = "text-muted small",
                    shiny::icon("info-circle"), " No discrete categories detected"
                ))
            }

            palette_colors <- current_palette()
            if (is.null(palette_colors) || length(palette_colors) == 0) {
                palette_colors <- default_colors
            }

            color_choices <- c("Default" = "default")
            for (i in seq_along(palette_colors)) {
                color_choices[paste0("#", i, " (", palette_colors[i], ")")] <- palette_colors[i]
            }

            assign_mode <- input$assign_mode %||% "number"

            assignment_ui <- lapply(seq_along(levels_to_use), function(i) {
                level_name <- levels_to_use[i]
                input_id <- ns(paste0("cat_color_", i))

                if (assign_mode == "number") {
                    label_text <- paste0("#", i)
                } else {
                    label_text <- if (nchar(level_name) > 12) paste0(substr(level_name, 1, 10), "..") else level_name
                }

                shiny::div(
                    class = "d-flex align-items-center gap-2 mb-1",
                    shiny::span(label_text, class = "small", style = "width:70px;display:inline-block;"),
                    shiny::selectInput(input_id, NULL, width = "100%",
                        choices = color_choices, selected = "default")
                )
            })

            shiny::div(
                shiny::div(class = "small text-muted mb-2",
                    paste0(length(levels_to_use), " categories detected")),
                assignment_ui
            )
        })

        # Collect manual color values
        shiny::observe({
            cats <- plot_categories()
            apply_to <- input$manual_apply %||% "fill"

            levels_to_use <- if (apply_to == "fill") cats$fill_levels
                             else if (apply_to == "color") cats$color_levels
                             else if (!is.null(cats$fill_levels)) cats$fill_levels
                             else cats$color_levels

            if (is.null(levels_to_use)) return()

            color_map <- list()
            for (i in seq_along(levels_to_use)) {
                val <- input[[paste0("cat_color_", i)]]
                if (!is.null(val) && val != "default") {
                    color_map[[levels_to_use[i]]] <- val
                }
            }
            manual_color_values(color_map)
        })

        # --- Return Values ---
        current_palette <- shiny::reactive({
            pkg <- input$palette_package %||% "none"
            if (pkg == "none") return(NULL)

            palettes <- available_palettes()
            if (length(palettes) == 0) return(NULL)

            idx <- min(palette_idx(), length(palettes))
            all_colors <- get_palette_colors(pkg, palettes[idx], 8)
            sel <- selected_colors()

            if (length(sel) > 0) all_colors[sel] else all_colors
        })

        list(
            current_palette = current_palette,
            manual_colors = shiny::reactive(manual_color_values()),
            palette_apply = shiny::reactive(input$palette_apply %||% "fill"),
            palette_scale = shiny::reactive(input$palette_scale %||% "discrete"),
            manual_apply = shiny::reactive(input$manual_apply %||% "fill"),
            manual_enabled = shiny::reactive(input$manual_enabled %||% FALSE),
            default_color = shiny::reactive(input$default_color %||% "#808080"),
            palette_package = shiny::reactive(input$palette_package %||% "none"),
            palette_name = shiny::reactive({
                palettes <- available_palettes()
                if (length(palettes) > 0) palettes[min(palette_idx(), length(palettes))] else NULL
            })
        )
    })
}
