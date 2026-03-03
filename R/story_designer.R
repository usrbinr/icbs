#' Interactive Story Layout Designer
#'
#' @description
#' Launches a Shiny app to interactively design story layouts with
#' real-time preview. Provides controls for text blocks, color palettes,
#' themes, and exports ready-to-use patchwork code.
#'
#' @details
#' The Story Designer is the primary interface for creating SWD-style
#' visualizations. It provides a WYSIWYG editor with live preview,
#' allowing you to adjust text, colors, and layout before copying the
#' generated patchwork code into your Quarto document or R script.
#'
#' The workflow is: (1) create your base ggplot, (2) pass it to
#' `story_designer()`, (3) customize using the sidebar controls,
#' (4) copy the code from the Code tab, (5) paste into your document.
#'
#' Text inputs support marquee formatting syntax: `**bold**` for bold,
#' `*italic*` for italics, and `{#HEXCOLOR text}` for colored text.
#'
#' @param plot Optional ggplot object to include in the preview.
#'   If NULL, a placeholder chart is used.
#' @param title Initial title text. Supports marquee formatting.
#' @param subtitle Initial subtitle text.
#' @param narrative Initial narrative text for the side panel.
#' @param caption Initial caption/source text.
#'
#' @returns Launches a Shiny app. Returns NULL invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(factor(cyl), mpg, fill = factor(cyl))) +
#'   geom_col()
#' story_designer(plot = p, title = "**Fuel Economy** by {#E69F00 Cylinders}")
#' }
#'
story_designer <- function(plot = NULL,
                           title = "**Your {#E69F00 title} here**",
                           subtitle = "Supporting context for your visualization",
                           narrative = "**KEY INSIGHT:**\nYour narrative here.",
                           caption = "SOURCE: Your data source") {

    # Check required packages
    for (pkg in c("shiny", "bslib", "marquee")) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            cli::cli_abort(
                "Package {.pkg {pkg}} is required for {.fn story_designer}.",
                "i" = "Install it with: {.code install.packages('{pkg}')}"
            )
        }
    }

    # Default placeholder plot if none provided
    user_plot <- plot %||% ggplot2::ggplot(
        data.frame(x = c("A", "B", "C", "D"), y = c(4, 7, 3, 8)),
        ggplot2::aes(x = x, y = y)
    ) +
        ggplot2::geom_col(fill = "#E69F00") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = NULL, y = NULL, title = NULL)

    # Register CSS resource path
    css_path <- system.file("www", package = "stwd")
    if (nzchar(css_path)) {
        shiny::addResourcePath("stwd-assets", css_path)
    }

    # ============ UI ============
    ui <- bslib::page_sidebar(
        title = "Story Layout Designer",
        theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
        shiny::tags$head(
            shiny::tags$link(rel = "stylesheet", type = "text/css",
                href = "stwd-assets/story_designer.css")
        ),

        sidebar = bslib::sidebar(
            id = "main_sidebar",
            width = 320,
            open = TRUE,

            # Output dimensions
            shiny::div(
                class = "mb-3 p-2 bg-light rounded",
                shiny::div(
                    class = "d-flex justify-content-between align-items-center mb-2",
                    shiny::strong("Output Dimensions"),
                    shiny::div(
                        class = "d-flex align-items-center gap-1",
                        shiny::tags$small("Sidebar:", class = "text-muted"),
                        shiny::selectInput("sidebar_width", NULL, width = "70px",
                            choices = c("280", "320", "380", "450", "520"),
                            selected = "320")
                    )
                ),
                bslib::layout_column_wrap(
                    width = 1/2,
                    shiny::numericInput("output_width", "W (in)", value = 12, min = 6, max = 20, step = 1),
                    shiny::numericInput("output_height", "H (in)", value = 9, min = 4, max = 16, step = 1)
                )
            ),

            # Accordion panels
            bslib::accordion(
                id = "inputs_accordion",
                open = FALSE,
                text_panel("title", "Title", "primary", "heading", title,
                    size_min = 6, size_max = 24, size_default = 12),
                text_panel("subtitle", "Subtitle", "info", "font", subtitle,
                    size_min = 8, size_max = 16, size_default = 11),
                narrative_panel(narrative),
                text_panel("caption", "Caption", "secondary", "quote-right", caption,
                    rows = 1, show_margin = FALSE, size_min = 7, size_max = 12, size_default = 9),
                mod_legend_ui("legend"),
                plot_panel(),
                mod_palette_ui("palette"),
                axis_grid_panel()
            ),

            # Reset button
            shiny::div(class = "mt-3",
                shiny::actionButton("reset_defaults", "Reset to Defaults",
                    class = "btn-outline-secondary btn-sm w-100", icon = shiny::icon("undo"))
            )
        ),

        # Main content tabs
        bslib::navset_card_tab(
            id = "main_tabs",
            full_screen = TRUE,

            # Preview tab
            bslib::nav_panel(
                title = shiny::span(shiny::icon("eye"), " Preview"),
                bslib::card_body(
                    class = "p-2",
                    shiny::div(
                        class = "d-flex justify-content-between align-items-center mb-2",
                        shiny::span(
                            shiny::span(class = "badge bg-warning text-dark me-2", "Scaled"),
                            shiny::textOutput("dimensions_label", inline = TRUE)
                        )
                    ),
                    shiny::uiOutput("preview_container")
                )
            ),

            # Sections tab
            bslib::nav_panel(
                title = shiny::span(shiny::icon("th-large"), " Sections"),
                bslib::layout_column_wrap(
                    width = 1/2,
                    bslib::card(
                        bslib::card_header(class = "bg-light py-2", "Section Heights"),
                        bslib::card_body(
                            shiny::sliderInput("title_height", "Title",
                                min = 0.05, max = 0.40, value = 0.08, step = 0.01),
                            shiny::sliderInput("subtitle_height", "Subtitle",
                                min = 0.03, max = 0.20, value = 0.06, step = 0.01),
                            shiny::sliderInput("caption_height", "Caption",
                                min = 0.02, max = 0.10, value = 0.05, step = 0.01)
                        )
                    ),
                    bslib::card(
                        bslib::card_header(class = "bg-light py-2", "Layout Preview"),
                        bslib::card_body(
                            shiny::plotOutput("height_diagram", height = "180px")
                        )
                    )
                )
            ),

            # Fine Tune tab
            bslib::nav_panel(
                title = shiny::span(shiny::icon("sliders-h"), " Fine Tune"),
                fine_tune_accordion()
            ),

            # Validate & Export tab
            bslib::nav_panel(
                title = shiny::span(shiny::icon("search-plus"), " Validate & Export"),
                mod_export_ui("export")
            ),

            # Code tab
            bslib::nav_panel(
                title = shiny::span(shiny::icon("code"), " Code"),
                bslib::card_body(
                    shiny::div(
                        class = "mb-2",
                        shiny::actionButton("copy_code", "Copy Code",
                            class = "btn-sm btn-primary me-1", icon = shiny::icon("copy")),
                        shiny::downloadButton("download_plot", "Download PNG",
                            class = "btn-sm btn-outline-secondary")
                    ),
                    shiny::uiOutput("generated_code")
                )
            ),

            # Components tab
            bslib::nav_panel(
                title = shiny::span(shiny::icon("layer-group"), " Components"),
                bslib::navset_card_underline(
                    bslib::nav_panel("Plot", shiny::plotOutput("preview_chart", height = "250px")),
                    bslib::nav_panel("Title", shiny::plotOutput("preview_title", height = "100px")),
                    bslib::nav_panel("Subtitle", shiny::plotOutput("preview_subtitle", height = "80px")),
                    bslib::nav_panel("Narrative", shiny::plotOutput("preview_narrative", height = "150px")),
                    bslib::nav_panel("Caption", shiny::plotOutput("preview_caption", height = "80px"))
                )
            )
        )
    )

    # ============ SERVER ============
    server <- function(input, output, session) {

        # --- Debounced Text Inputs ---
        title_text_d <- shiny::debounce(shiny::reactive(input$title_text), 500)
        subtitle_text_d <- shiny::debounce(shiny::reactive(input$subtitle_text), 500)
        narrative_text_d <- shiny::debounce(shiny::reactive(input$narrative_text), 500)
        caption_text_d <- shiny::debounce(shiny::reactive(input$caption_text), 500)

        # --- Detect Plot Categories ---
        plot_categories <- shiny::reactive({
            tryCatch(
                detect_plot_categories(user_plot),
                error = function(e) list(fill_levels = NULL, color_levels = NULL, n_fill = 0, n_color = 0)
            )
        })

        # --- Module: Palette ---
        palette <- mod_palette_server("palette", plot_categories)

        # --- Module: Legend ---
        legend <- mod_legend_server("legend")

        # --- Sidebar Width ---
        shiny::observeEvent(input$sidebar_width, {
            shiny::insertUI(
                selector = "head", where = "beforeEnd", immediate = TRUE,
                ui = shiny::tags$style(shiny::HTML(paste0(
                    ".bslib-sidebar-layout > .sidebar { width: ", input$sidebar_width, "px !important; }"
                )))
            )
        })

        # --- Current Heights ---
        current_heights <- shiny::reactive({
            legend_h <- if (legend$enabled()) 0.04 else 0
            list(
                title = input$title_height %||% 0.08,
                subtitle = input$subtitle_height %||% 0.06,
                legend = legend_h,
                caption = input$caption_height %||% 0.05
            )
        })

        # --- Styled Plot ---
        styled_plot <- shiny::reactive({
            base_theme <- switch(input$plot_theme %||% "stwd",
                "stwd" = theme_stwd(),
                "void" = ggplot2::theme_void(),
                theme_stwd()
            )

            theme_mods <- build_theme_mods(input)
            p <- user_plot + base_theme + theme_mods

            # Apply palette colors
            pal_colors <- palette$current_palette()
            if (!is.null(pal_colors)) {
                p <- apply_color_scales(p, pal_colors,
                    palette$palette_apply(), palette$palette_scale())
            }

            # Apply manual colors if enabled
            if (palette$manual_enabled()) {
                color_map <- palette$manual_colors()
                default_col <- palette$default_color()
                manual_apply <- palette$manual_apply()

                if (length(color_map) > 0 || default_col != "#808080") {
                    cats <- plot_categories()
                    levels_to_use <- if (manual_apply == "fill") cats$fill_levels
                        else if (manual_apply == "color") cats$color_levels
                        else if (!is.null(cats$fill_levels)) cats$fill_levels
                        else cats$color_levels

                    if (!is.null(levels_to_use) && length(levels_to_use) > 0) {
                        final_colors <- purrr::map_chr(levels_to_use, function(lvl) {
                            if (lvl %in% names(color_map)) color_map[[lvl]] else default_col
                        }) |> stats::setNames(levels_to_use)
                        p <- apply_color_scales(p, final_colors, manual_apply, "discrete")
                    }
                }
            }

            p
        })

        # --- Build Layout ---
        build_layout <- shiny::reactive({
            h <- current_heights()

            # Get text values
            title_txt <- convert_named_colors(title_text_d())
            subtitle_txt <- convert_named_colors(subtitle_text_d())
            narrative_txt <- convert_named_colors(narrative_text_d())
            caption_txt <- convert_named_colors(caption_text_d())

            # Validate inputs with user-friendly messages
            shiny::validate(
                shiny::need(
                    nchar(title_txt) > 0 || nchar(subtitle_txt) > 0,
                    "Enter a title or subtitle to see the preview"
                )
            )

            # Create blocks
            title_plot <- title_block(
                title_txt,
                title_size = input$title_size,
                halign = input$title_align %||% "left",
                lineheight = input$title_lineheight %||% 1.1,
                margin_left = 5, margin_right = 5,
                margin_bottom = input$title_margin_bottom,
                wrap_width = if ((input$title_wrap %||% 0) > 0) input$title_wrap else NULL
            )

            subtitle_plot <- subtitle_block(
                subtitle_txt,
                subtitle_size = input$subtitle_size,
                halign = input$subtitle_align %||% "left",
                lineheight = input$subtitle_lineheight %||% 1.2,
                margin_left = 5, margin_right = 5,
                margin_bottom = input$subtitle_margin_bottom,
                wrap_width = if ((input$subtitle_wrap %||% 0) > 0) input$subtitle_wrap else NULL
            )

            narrative_plot <- text_narrative(
                narrative_txt,
                size = input$narrative_size,
                halign = input$narrative_halign %||% "left",
                valign = input$narrative_valign %||% "top",
                padding = input$narrative_padding %||% 10,
                lineheight = input$narrative_lineheight %||% 1.4,
                wrap_width = if ((input$narrative_wrap %||% 0) > 0) input$narrative_wrap else NULL
            )

            caption_halign <- switch(input$caption_position %||% "full_left",
                "full_left" = "left", "full_center" = "center",
                "full_right" = "right", "under_chart" = "left", "left")

            caption_plot <- caption_block(
                caption_txt,
                caption_size = input$caption_size,
                halign = caption_halign,
                color = input$caption_color %||% "#808080",
                wrap_width = if ((input$caption_wrap %||% 0) > 0) input$caption_wrap else NULL
            )

            # Get legend
            legend_plot <- legend$plot()
            legend_pos <- legend$position()

            # Build content (plot + narrative)
            content <- styled_plot()
            plot_width <- 1 - input$narrative_width

            if (input$narrative_position == "right") {
                content <- content + narrative_plot +
                    patchwork::plot_layout(widths = c(plot_width, input$narrative_width))
            } else if (input$narrative_position == "left") {
                content <- narrative_plot + content +
                    patchwork::plot_layout(widths = c(input$narrative_width, plot_width))
            } else if (input$narrative_position == "top") {
                content <- narrative_plot / content +
                    patchwork::plot_layout(heights = c(input$narrative_width, plot_width))
            } else {
                content <- content / narrative_plot +
                    patchwork::plot_layout(heights = c(plot_width, input$narrative_width))
            }

            # Calculate content height
            legend_h_above <- if (!is.null(legend_plot) && legend_pos %in% c("above", "below")) h$legend else 0
            content_height <- 1 - h$title - h$subtitle - legend_h_above - h$caption

            # Compose final layout
            compose_layout(
                title_plot, subtitle_plot, content, caption_plot,
                legend_plot, legend_pos, legend$width(),
                h$title, h$subtitle, content_height, h$legend, h$caption
            )
        })

        # --- Module: Export ---
        export <- mod_export_server("export", build_layout)

        # --- Outputs ---
        output$dimensions_label <- shiny::renderText({
            paste0(input$output_width, '" x ', input$output_height, '"')
        })

        output$preview_container <- shiny::renderUI({
            aspect_pct <- (input$output_height / input$output_width) * 100
            shiny::div(
                style = paste0("position: relative; width: 100%; padding-top: ", aspect_pct,
                    "%; background: white; border: 1px solid #dee2e6;"),
                shiny::div(
                    style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0;",
                    shiny::plotOutput("preview_plot", height = "100%", width = "100%")
                )
            )
        })

        output$preview_plot <- shiny::renderPlot({
            build_layout()
        }, res = 96, bg = "white") |>
            shiny::bindCache(
                title_text_d(), subtitle_text_d(), narrative_text_d(), caption_text_d(),
                input$title_size, input$subtitle_size, input$narrative_size, input$caption_size,
                input$title_height, input$subtitle_height, input$caption_height,
                input$narrative_width, input$narrative_position,
                input$plot_theme, legend$enabled(), legend$position(),
                palette$current_palette(), palette$manual_enabled()
            )

        output$height_diagram <- shiny::renderPlot({
            h <- current_heights()
            content_height <- 1 - h$title - h$subtitle - h$caption

            df <- data.frame(
                component = factor(c("Title", "Subtitle", "Content", "Caption"),
                    levels = c("Title", "Subtitle", "Content", "Caption")),
                height = c(h$title, h$subtitle, content_height, h$caption),
                label = paste0(round(c(h$title, h$subtitle, content_height, h$caption) * 100), "%")
            )
            df$ymax <- cumsum(df$height)
            df$ymin <- c(0, head(df$ymax, -1))
            df$ymid <- (df$ymin + df$ymax) / 2

            ggplot2::ggplot(df) +
                ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, fill = component),
                    color = "white", linewidth = 0.5) +
                ggplot2::geom_text(ggplot2::aes(x = 0.5, y = ymid, label = label),
                    size = 3, color = "white", fontface = "bold") +
                ggplot2::scale_y_reverse() +
                ggplot2::scale_fill_manual(values = c(
                    "Title" = "#3498db", "Subtitle" = "#5dade2",
                    "Content" = "#27ae60", "Caption" = "#95a5a6"
                )) +
                ggplot2::theme_void() +
                ggplot2::theme(legend.position = "none")
        }, bg = "transparent")

        # Component previews
        output$preview_chart <- shiny::renderPlot({ styled_plot() }, res = 96, bg = "white")

        output$preview_title <- shiny::renderPlot({
            title_block(convert_named_colors(title_text_d()),
                title_size = input$title_size,
                margin_bottom = input$title_margin_bottom)
        }, res = 96, bg = "white")

        output$preview_subtitle <- shiny::renderPlot({
            subtitle_block(convert_named_colors(subtitle_text_d()),
                subtitle_size = input$subtitle_size,
                margin_bottom = input$subtitle_margin_bottom)
        }, res = 96, bg = "white")

        output$preview_narrative <- shiny::renderPlot({
            text_narrative(convert_named_colors(narrative_text_d()),
                size = input$narrative_size,
                halign = input$narrative_halign %||% "left",
                lineheight = input$narrative_lineheight %||% 1.4)
        }, res = 96, bg = "white")

        output$preview_caption <- shiny::renderPlot({
            caption_halign <- switch(input$caption_position %||% "full_left",
                "full_left" = "left", "full_center" = "center",
                "full_right" = "right", "under_chart" = "left", "left")
            caption_block(convert_named_colors(caption_text_d()),
                caption_size = input$caption_size, halign = caption_halign,
                color = input$caption_color %||% "#808080",
                margin_top = 2, margin_bottom = 2)
        }, res = 96, bg = "white")

        # Reset defaults
        shiny::observeEvent(input$reset_defaults, {
            reset_all_inputs(session)
        })

        # Code generation
        code_to_copy <- shiny::reactive({
            h <- current_heights()
            caption_halign <- switch(input$caption_position %||% "full_left",
                "full_left" = "left", "full_center" = "center",
                "full_right" = "right", "under_chart" = "left", "left")

            pkg <- palette$palette_package()
            pal_name <- palette$palette_name()

            legend_code <- ""
            if (legend$enabled()) {
                labels <- trimws(strsplit(legend$labels(), ",")[[1]])
                labels <- labels[labels != ""]
                if (length(labels) > 0) {
                    colors <- legend$colors()
                    legend_pos <- legend$position()
                    orientation <- if (legend_pos %in% c("left", "right")) "vertical" else "horizontal"
                    h_align <- if (legend_pos == "left") "left"
                        else if (legend_pos == "right") "right"
                        else legend$halign()

                    legend_code <- generate_legend_code(
                        labels, unname(colors), h_align, orientation,
                        legend$sep(), legend$size(), legend$bold(),
                        legend$uppercase(),
                        if (orientation == "vertical") legend$lineheight() else NULL,
                        legend$wrap()
                    )
                }
            }

            paste0(
                'library(patchwork)\n\n',
                generate_theme_code(input),
                generate_palette_code(pkg, pal_name, palette$palette_apply(), palette$palette_scale()),
                '\n\n',
                generate_block_code("title", input$title_text, input$title_size,
                    input$title_align %||% "left", input$title_lineheight %||% 1.1,
                    input$title_margin_bottom, input$title_wrap %||% 0),
                generate_block_code("subtitle", input$subtitle_text, input$subtitle_size,
                    input$subtitle_align %||% "left", input$subtitle_lineheight %||% 1.2,
                    input$subtitle_margin_bottom, input$subtitle_wrap %||% 0),
                generate_block_code("narrative", input$narrative_text, input$narrative_size,
                    input$narrative_halign %||% "left", input$narrative_lineheight %||% 1.4,
                    wrap_width = input$narrative_wrap %||% 0,
                    valign = input$narrative_valign %||% "top",
                    padding = input$narrative_padding %||% 10),
                generate_block_code("caption", input$caption_text, input$caption_size,
                    caption_halign, lineheight = NULL, wrap_width = input$caption_wrap %||% 0,
                    color = input$caption_color %||% "#808080"),
                legend_code,
                generate_composition_code(h, input$narrative_width,
                    legend$enabled(), legend$position(), legend$width())
            )
        })

        output$generated_code <- shiny::renderUI({
            shiny::pre(class = "bg-dark text-light p-3 rounded",
                style = "font-size: 12px;", code_to_copy())
        })

        shiny::observeEvent(input$copy_code, {
            if (requireNamespace("clipr", quietly = TRUE)) {
                clipr::write_clip(code_to_copy())
                shiny::showNotification("Copied!", type = "message", duration = 2)
            } else {
                shiny::showNotification("Install 'clipr' for clipboard", type = "warning")
            }
        })

        output$download_plot <- shiny::downloadHandler(
            filename = function() {
                paste0("story_layout_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
            },
            content = function(file) {
                ggplot2::ggsave(file, build_layout(),
                    width = input$output_width, height = input$output_height,
                    dpi = 150, bg = "white")
            }
        )
    }

    shiny::shinyApp(ui = ui, server = server)
}

#' Compose final patchwork layout
#' @noRd
compose_layout <- function(title_plot, subtitle_plot, content, caption_plot,
                           legend_plot, legend_pos, legend_width,
                           h_title, h_subtitle, content_height, h_legend, h_caption) {

    if (!is.null(legend_plot)) {
        if (legend_pos == "above") {
            title_plot / subtitle_plot / legend_plot / content / caption_plot +
                patchwork::plot_layout(heights = c(h_title, h_subtitle, h_legend, content_height, h_caption))
        } else if (legend_pos == "below") {
            title_plot / subtitle_plot / content / legend_plot / caption_plot +
                patchwork::plot_layout(heights = c(h_title, h_subtitle, content_height, h_legend, h_caption))
        } else if (legend_pos == "right") {
            content_with_legend <- patchwork::wrap_plots(content, legend_plot,
                widths = c(1 - legend_width, legend_width))
            title_plot / subtitle_plot / content_with_legend / caption_plot +
                patchwork::plot_layout(heights = c(h_title, h_subtitle, content_height, h_caption))
        } else if (legend_pos == "left") {
            content_with_legend <- patchwork::wrap_plots(legend_plot, content,
                widths = c(legend_width, 1 - legend_width))
            title_plot / subtitle_plot / content_with_legend / caption_plot +
                patchwork::plot_layout(heights = c(h_title, h_subtitle, content_height, h_caption))
        } else {
            title_plot / subtitle_plot / content / caption_plot +
                patchwork::plot_layout(heights = c(h_title, h_subtitle, content_height, h_caption))
        }
    } else {
        title_plot / subtitle_plot / content / caption_plot +
            patchwork::plot_layout(heights = c(h_title, h_subtitle, content_height, h_caption))
    }
}

#' Fine tune accordion UI
#' @noRd
fine_tune_accordion <- function() {
    bslib::accordion(
        id = "finetune_accordion",
        open = FALSE,
        bslib::accordion_panel(
            title = shiny::span(shiny::span(class = "badge bg-primary me-2", " "), "Title"),
            value = "Title",
            shiny::selectInput("title_align", "Alignment", width = "100%",
                choices = c("Left" = "left", "Center" = "center", "Right" = "right")),
            shiny::numericInput("title_lineheight", "Line height",
                value = 1.1, min = 0.8, max = 2, step = 0.1, width = "100%"),
            shiny::sliderInput("title_wrap", "Wrap at chars (0=off)",
                min = 0, max = 80, value = 0, step = 5)
        ),
        bslib::accordion_panel(
            title = shiny::span(shiny::span(class = "badge bg-info me-2", " "), "Subtitle"),
            value = "Subtitle",
            shiny::selectInput("subtitle_align", "Alignment", width = "100%",
                choices = c("Left" = "left", "Center" = "center", "Right" = "right")),
            shiny::numericInput("subtitle_lineheight", "Line height",
                value = 1.2, min = 0.8, max = 2, step = 0.1, width = "100%"),
            shiny::sliderInput("subtitle_wrap", "Wrap at chars (0=off)",
                min = 0, max = 80, value = 0, step = 5)
        ),
        bslib::accordion_panel(
            title = shiny::span(shiny::span(class = "badge bg-success me-2", " "), "Narrative"),
            value = "Narrative",
            shiny::selectInput("narrative_halign", "Horizontal align", width = "100%",
                choices = c("Left" = "left", "Center" = "center", "Right" = "right")),
            shiny::selectInput("narrative_valign", "Vertical align", width = "100%",
                choices = c("Top" = "top", "Center" = "center", "Bottom" = "bottom")),
            shiny::numericInput("narrative_lineheight", "Line height",
                value = 1.4, min = 0.8, max = 3, step = 0.1, width = "100%"),
            shiny::numericInput("narrative_padding", "Padding (pt)",
                value = 10, min = 0, max = 30, step = 1, width = "100%"),
            shiny::sliderInput("narrative_wrap", "Wrap at chars (0=off)",
                min = 0, max = 80, value = 0, step = 5)
        ),
        bslib::accordion_panel(
            title = shiny::span(shiny::span(class = "badge bg-secondary me-2", " "), "Caption"),
            value = "Caption",
            shiny::selectInput("caption_position", "Position", width = "100%",
                choices = c("Full width (left)" = "full_left", "Full width (center)" = "full_center",
                    "Full width (right)" = "full_right", "Under chart only" = "under_chart")),
            shiny::textInput("caption_color", "Color", value = "#808080", width = "100%"),
            shiny::sliderInput("caption_wrap", "Wrap at chars (0=off)",
                min = 0, max = 100, value = 0, step = 5)
        )
    )
}
