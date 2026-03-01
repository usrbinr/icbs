#' Interactive Story Layout Designer
#'
#' Launches a Shiny app to interactively design story layouts with
#' real-time clipping detection. Helps you find the right height
#' allocations before exporting.
#'
#' @param plot Optional ggplot object to include in the preview.
#'   If NULL, a placeholder chart is used.
#' @param title Initial title text.
#' @param subtitle Initial subtitle text.
#' @param narrative Initial narrative text.
#' @param caption Initial caption text.
#'
#' @returns Launches a Shiny app. When you click "Copy Code", the
#'   generated `story_layout()` call is copied to your clipboard.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch with defaults
#' story_designer()
#'
#' # Launch with your plot
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_col()
#' story_designer(plot = p, title = "My Title")
#' }
#'
story_designer <- function(plot = NULL,
                           title = "**Your {#E69F00 title} here**",
                           subtitle = "Supporting context for your visualization",
                           narrative = "**KEY INSIGHT:**\nYour narrative here.",
                           caption = "SOURCE: Your data source") {

    if (!requireNamespace("shiny", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg shiny} is required for {.fn story_designer}.",
            "i" = "Install it with: {.code install.packages('shiny')}"
        )
    }

    if (!requireNamespace("bslib", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg bslib} is required for {.fn story_designer}.",
            "i" = "Install it with: {.code install.packages('bslib')}"
        )
    }

    if (!requireNamespace("marquee", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg marquee} is required for {.fn story_designer}.",
            "i" = "Install it with: {.code install.packages('marquee')}"
        )
    }

    # Store the user's plot
    user_plot <- plot

    # Default placeholder plot if none provided
    if (is.null(user_plot)) {
        user_plot <- ggplot2::ggplot(
            data.frame(x = c("A", "B", "C", "D"), y = c(4, 7, 3, 8)),
            ggplot2::aes(x = x, y = y)
        ) +
            ggplot2::geom_col(fill = "#E69F00") +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = NULL, y = NULL, title = NULL)
    }

    # UI
    ui <- bslib::page_sidebar(
        title = "Story Layout Designer",
        theme = bslib::bs_theme(
            version = 5,
            bootswatch = "flatly",
            primary = "#2c3e50"
        ),

        # Sidebar with inputs
        sidebar = bslib::sidebar(
            width = 320,
            open = TRUE,

            # Output dimensions - compact
            shiny::div(
                class = "mb-3 p-2 bg-light rounded",
                shiny::strong("Output Dimensions"),
                bslib::layout_column_wrap(
                    width = 1/2,
                    shiny::numericInput("output_width", "W (in)", value = 12, min = 6, max = 20, step = 1),
                    shiny::numericInput("output_height", "H (in)", value = 9, min = 4, max = 16, step = 1)
                )
            ),

            # Accordion for text components - no nested cards
            bslib::accordion(
                id = "inputs_accordion",
                open = FALSE,

                bslib::accordion_panel(
                    title = shiny::span(shiny::span(class = "badge bg-primary me-2", " "), "Title"),
                    value = "Title",
                    icon = shiny::icon("heading"),
                    shiny::div(
                        class = "d-flex align-items-center gap-2 mb-1",
                        shiny::span("Text", class = "small"),
                        bslib::tooltip(
                            shiny::icon("circle-info", class = "text-muted"),
                            "**bold** | *italic* | {#E69F00 colored text} | {red named color}"
                        )
                    ),
                    shiny::textAreaInput("title_text", NULL, value = title, rows = 2, width = "100%",
                                         placeholder = "Use **bold** or {#E69F00 color}"),
                    shiny::sliderInput("title_size", "Font size", min = 10, max = 24, value = 16, step = 1),
                    shiny::sliderInput("title_margin_bottom", "Space below (pt)", min = 0, max = 30, value = 5, step = 1),
                    shiny::sliderInput("title_wrap", "Wrap at chars (0=off)", min = 0, max = 80, value = 0, step = 5),
                    shiny::uiOutput("title_metrics")
                ),

                bslib::accordion_panel(
                    title = shiny::span(shiny::span(class = "badge bg-info me-2", " "), "Subtitle"),
                    value = "Subtitle",
                    icon = shiny::icon("font"),
                    shiny::div(
                        class = "d-flex align-items-center gap-2 mb-1",
                        shiny::span("Text", class = "small"),
                        bslib::tooltip(
                            shiny::icon("circle-info", class = "text-muted"),
                            "**bold** | *italic* | {#hex color} | {colorname text}"
                        )
                    ),
                    shiny::textAreaInput("subtitle_text", NULL, value = subtitle, rows = 2, width = "100%"),
                    shiny::sliderInput("subtitle_size", "Font size", min = 8, max = 16, value = 11, step = 1),
                    shiny::sliderInput("subtitle_margin_bottom", "Space below (pt)", min = 0, max = 30, value = 5, step = 1),
                    shiny::sliderInput("subtitle_wrap", "Wrap at chars (0=off)", min = 0, max = 80, value = 0, step = 5),
                    shiny::uiOutput("subtitle_metrics")
                ),

                bslib::accordion_panel(
                    title = shiny::span(shiny::span(class = "badge bg-success me-2", " "), "Narrative"),
                    value = "Narrative",
                    icon = shiny::icon("align-left"),
                    shiny::div(
                        class = "d-flex align-items-center gap-2 mb-1",
                        shiny::span("Text", class = "small"),
                        bslib::tooltip(
                            shiny::icon("circle-info", class = "text-muted"),
                            "**bold** | *italic* | {#hex color} | Use blank lines for paragraphs"
                        )
                    ),
                    shiny::textAreaInput("narrative_text", NULL, value = narrative, rows = 4, width = "100%"),
                    shiny::selectInput("narrative_position", "Layout",
                                       choices = c("Chart LEFT | Narrative RIGHT" = "right",
                                                   "Narrative LEFT | Chart RIGHT" = "left",
                                                   "Chart TOP | Narrative BOTTOM" = "bottom",
                                                   "Narrative TOP | Chart BOTTOM" = "top"),
                                       selected = "right"),
                    shiny::uiOutput("width_display"),
                    shiny::sliderInput("narrative_width", NULL, min = 0.15, max = 0.50, value = 0.35, step = 0.05),
                    shiny::sliderInput("narrative_size", "Font size", min = 8, max = 14, value = 10, step = 1)
                ),

                bslib::accordion_panel(
                    title = shiny::span(shiny::span(class = "badge bg-secondary me-2", " "), "Caption"),
                    value = "Caption",
                    icon = shiny::icon("quote-right"),
                    shiny::div(
                        class = "d-flex align-items-center gap-2 mb-1",
                        shiny::span("Text", class = "small"),
                        bslib::tooltip(
                            shiny::icon("circle-info", class = "text-muted"),
                            "Source attribution (e.g., SOURCE: Company Database)"
                        )
                    ),
                    shiny::textInput("caption_text", NULL, value = caption, width = "100%"),
                    shiny::sliderInput("caption_size", "Font size", min = 7, max = 12, value = 9, step = 1),
                    shiny::sliderInput("caption_wrap", "Wrap at chars (0=off)", min = 0, max = 100, value = 0, step = 5)
                ),

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
                            bslib::tooltip(
                                shiny::icon("circle-info", class = "text-muted"),
                                "Use hex (#808080) or color names (red, steelblue)"
                            )
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
                ),

                bslib::accordion_panel(
                    title = shiny::span(shiny::span(class = "badge bg-warning me-2", " "), "Plot"),
                    value = "Plot",
                    icon = shiny::icon("chart-bar"),
                    shiny::selectInput("plot_theme", "Theme", width = "100%",
                        choices = c("Minimal" = "minimal", "STWD" = "stwd", "Void" = "void")),
                    shiny::selectInput("plot_legend_pos", "Legend", width = "100%",
                        choices = c("Right" = "right", "Bottom" = "bottom",
                                    "Top" = "top", "Left" = "left", "None" = "none")),
                    shiny::hr(),
                    shiny::tags$label("Color Palette", class = "form-label"),
                    shiny::selectInput("palette_package", "Package", width = "100%",
                        choices = c("None" = "none", "MetBrewer" = "MetBrewer",
                                    "PNWColors" = "PNWColors", "RColorBrewer" = "RColorBrewer",
                                    "viridis" = "viridis")),
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
                        shiny::radioButtons("palette_apply", "Apply to:",
                            choices = c("Fill" = "fill", "Color" = "color", "Both" = "both"),
                            selected = "fill", inline = TRUE),
                        shiny::radioButtons("palette_scale", "Scale type:",
                            choices = c("Discrete" = "discrete", "Continuous" = "continuous"),
                            selected = "discrete", inline = TRUE)
                    )
                ),

                bslib::accordion_panel(
                    title = shiny::span(shiny::span(class = "badge bg-warning me-2", " "), "X-Axis Label"),
                    value = "X-Axis Label",
                    icon = shiny::icon("arrows-left-right"),
                    shiny::sliderInput("axis_title_x_size", "Size", min = 8, max = 16, value = 11, step = 1),
                    shiny::checkboxInput("axis_title_x_bold", "Bold", value = FALSE),
                    shiny::selectInput("axis_title_x_align", "Alignment", width = "100%",
                        choices = c("Center" = "0.5", "Left" = "0", "Right" = "1")),
                    shiny::selectInput("axis_title_x_angle", "Rotation", width = "100%",
                        choices = c("Horizontal (0)" = "0", "Vertical (90)" = "90")),
                    shiny::sliderInput("axis_title_x_margin", "Spacing from axis", min = 0, max = 20, value = 5, step = 1),
                    shiny::textInput("axis_title_x_color", "Color", value = "#333333", width = "100%")
                ),

                bslib::accordion_panel(
                    title = shiny::span(shiny::span(class = "badge bg-warning me-2", " "), "Y-Axis Label"),
                    value = "Y-Axis Label",
                    icon = shiny::icon("arrows-up-down"),
                    shiny::sliderInput("axis_title_y_size", "Size", min = 8, max = 16, value = 11, step = 1),
                    shiny::checkboxInput("axis_title_y_bold", "Bold", value = FALSE),
                    shiny::selectInput("axis_title_y_align", "Alignment", width = "100%",
                        choices = c("Center" = "0.5", "Bottom" = "0", "Top" = "1")),
                    shiny::selectInput("axis_title_y_angle", "Rotation", width = "100%",
                        choices = c("Vertical (90)" = "90", "Horizontal (0)" = "0")),
                    shiny::sliderInput("axis_title_y_margin", "Spacing from axis", min = 0, max = 20, value = 5, step = 1),
                    shiny::textInput("axis_title_y_color", "Color", value = "#333333", width = "100%")
                ),

                bslib::accordion_panel(
                    title = shiny::span(shiny::span(class = "badge bg-warning me-2", " "), "Axis Text & Lines"),
                    value = "Axis Text & Lines",
                    icon = shiny::icon("font"),
                    shiny::strong(class = "small", "Axis Text"),
                    shiny::sliderInput("axis_text_size", "Size", min = 7, max = 14, value = 10, step = 1),
                    shiny::textInput("axis_text_color", "Color", value = "#666666", width = "100%"),
                    shiny::hr(class = "my-2"),
                    shiny::strong(class = "small", "Axis Line & Ticks"),
                    shiny::checkboxInput("show_axis_line", "Show axis line", value = FALSE),
                    shiny::checkboxInput("show_ticks", "Show tick marks", value = FALSE),
                    shiny::textInput("axis_line_color", "Line/tick color", value = "#333333", width = "100%")
                ),

                bslib::accordion_panel(
                    title = shiny::span(shiny::span(class = "badge bg-warning me-2", " "), "Grid Lines"),
                    value = "Grid Lines",
                    icon = shiny::icon("border-all"),
                    shiny::checkboxInput("grid_remove_all", "Remove all grid lines", value = FALSE),
                    shiny::selectInput("grid_major", "Major grid", width = "100%",
                        choices = c("Both" = "both", "Horizontal only" = "h", "Vertical only" = "v", "None" = "none")),
                    shiny::selectInput("grid_minor", "Minor grid", width = "100%",
                        choices = c("None" = "none", "Both" = "both", "Horizontal only" = "h", "Vertical only" = "v")),
                    shiny::textInput("grid_color", "Grid color", value = "#E5E5E5", width = "100%")
                )
            ),
            # Reset button
            shiny::div(class = "mt-3",
                shiny::actionButton("reset_defaults", "Reset to Defaults",
                    class = "btn-outline-secondary btn-sm w-100", icon = shiny::icon("undo"))
            )
        ),

        # Main content - use navset for different views
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
                        ),
                        shiny::actionButton("validate_actual", "Validate Actual Size",
                                            class = "btn-sm btn-success",
                                            icon = shiny::icon("search-plus"))
                    ),
                    shiny::uiOutput("preview_container"),
                    shiny::uiOutput("actual_size_preview")
                )
            ),

            # Sections tab
            bslib::nav_panel(
                title = shiny::span(shiny::icon("th-large"), " Sections"),
                bslib::layout_column_wrap(
                    width = 1/2,
                    # Controls on left
                    bslib::card(
                        bslib::card_header(class = "bg-light py-2", "Section Heights"),
                        bslib::card_body(
                            shiny::sliderInput("title_height", "Title", min = 0.05, max = 0.40, value = 0.12, step = 0.01),
                            shiny::sliderInput("subtitle_height", "Subtitle", min = 0.03, max = 0.20, value = 0.08, step = 0.01),
                            shiny::sliderInput("caption_height", "Caption", min = 0.02, max = 0.10, value = 0.05, step = 0.01)
                        )
                    ),
                    # Diagram on right
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
                bslib::accordion(
                    id = "finetune_accordion",
                    open = FALSE,
                    # Title settings
                    bslib::accordion_panel(
                        title = shiny::span(shiny::span(class = "badge bg-primary me-2", " "), "Title"),
                        value = "Title",
                        shiny::selectInput("title_align", "Alignment", width = "100%",
                            choices = c("Left" = "left", "Center" = "center", "Right" = "right")),
                        shiny::div(
                            class = "d-flex align-items-center gap-2",
                            shiny::numericInput("title_lineheight", "Line height", value = 1.1, min = 0.8, max = 2, step = 0.1, width = "80%"),
                            bslib::tooltip(shiny::icon("circle-info", class = "text-muted mt-4"), "1.0 = single, 1.5 = 1.5x, 2.0 = double")
                        )
                    ),
                    # Subtitle settings
                    bslib::accordion_panel(
                        title = shiny::span(shiny::span(class = "badge bg-info me-2", " "), "Subtitle"),
                        value = "Subtitle",
                        shiny::selectInput("subtitle_align", "Alignment", width = "100%",
                            choices = c("Left" = "left", "Center" = "center", "Right" = "right")),
                        shiny::div(
                            class = "d-flex align-items-center gap-2",
                            shiny::numericInput("subtitle_lineheight", "Line height", value = 1.2, min = 0.8, max = 2, step = 0.1, width = "80%"),
                            bslib::tooltip(shiny::icon("circle-info", class = "text-muted mt-4"), "1.0 = single, 1.5 = 1.5x, 2.0 = double")
                        )
                    ),
                    # Narrative settings
                    bslib::accordion_panel(
                        title = shiny::span(shiny::span(class = "badge bg-success me-2", " "), "Narrative"),
                        value = "Narrative",
                        shiny::selectInput("narrative_halign", "Horizontal align", width = "100%",
                            choices = c("Left" = "left", "Center" = "center", "Right" = "right")),
                        shiny::selectInput("narrative_valign", "Vertical align", width = "100%",
                            choices = c("Top" = "top", "Center" = "center", "Bottom" = "bottom")),
                        shiny::div(
                            class = "d-flex align-items-center gap-2",
                            shiny::numericInput("narrative_lineheight", "Line height", value = 1.4, min = 0.8, max = 3, step = 0.1, width = "80%"),
                            bslib::tooltip(shiny::icon("circle-info", class = "text-muted mt-4"), "1.0 = single, 2.0 = double")
                        ),
                        shiny::numericInput("narrative_padding", "Padding (pt)", value = 10, min = 0, max = 30, step = 1, width = "100%")
                    ),
                    # Caption settings
                    bslib::accordion_panel(
                        title = shiny::span(shiny::span(class = "badge bg-secondary me-2", " "), "Caption"),
                        value = "Caption",
                        shiny::selectInput("caption_position", "Position", width = "100%",
                            choices = c("Full width (left)" = "full_left",
                                        "Full width (center)" = "full_center",
                                        "Full width (right)" = "full_right",
                                        "Under chart only" = "under_chart")),
                        shiny::div(
                            class = "d-flex align-items-center gap-2",
                            shiny::textInput("caption_color", "Color", value = "#808080", width = "80%"),
                            bslib::tooltip(shiny::icon("circle-info", class = "text-muted mt-4"), "Hex (#808080) or name (gray)")
                        )
                    )
                )
            ),

            # Export tab
            bslib::nav_panel(
                title = shiny::span(shiny::icon("download"), " Export"),
                bslib::card_body(
                    class = "p-3",
                    shiny::div(
                        class = "row g-3 mb-3",
                        shiny::div(
                            class = "col-auto",
                            shiny::selectInput("export_format", "Format", width = "100px",
                                               choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                               selected = "png")
                        ),
                        shiny::div(
                            class = "col-auto",
                            shiny::numericInput("export_width", "Width (in)", value = 12, min = 4, max = 24, step = 0.5, width = "100px")
                        ),
                        shiny::div(
                            class = "col-auto",
                            shiny::numericInput("export_height", "Height (in)", value = 9, min = 3, max = 18, step = 0.5, width = "100px")
                        ),
                        shiny::div(
                            class = "col-auto",
                            shiny::numericInput("export_dpi", "DPI", value = 150, min = 72, max = 600, step = 10, width = "80px")
                        )
                    ),
                    shiny::div(
                        class = "d-flex gap-2 mb-3",
                        shiny::actionButton("render_export", "Preview Export",
                                            class = "btn-success", icon = shiny::icon("eye")),
                        shiny::downloadButton("download_export", "Download", class = "btn-primary")
                    ),
                    shiny::uiOutput("export_info"),
                    shiny::uiOutput("export_preview")
                )
            ),

            # Code tab
            bslib::nav_panel(
                title = shiny::span(shiny::icon("code"), " Code"),
                bslib::card_body(
                    shiny::div(
                        class = "mb-2",
                        shiny::actionButton("copy_code", "Copy Code", class = "btn-sm btn-primary me-1",
                                            icon = shiny::icon("copy")),
                        shiny::downloadButton("download_plot", "Download PNG", class = "btn-sm btn-outline-secondary")
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

    # Server
    server <- function(input, output, session) {

        # Define %||% locally for NULL handling
        `%||%` <- function(x, y) if (is.null(x)) y else x

        # Debounced text inputs - wait 500ms after typing stops before updating
        title_text_d <- shiny::debounce(shiny::reactive(input$title_text), 500)
        subtitle_text_d <- shiny::debounce(shiny::reactive(input$subtitle_text), 500)
        narrative_text_d <- shiny::debounce(shiny::reactive(input$narrative_text), 500)
        caption_text_d <- shiny::debounce(shiny::reactive(input$caption_text), 500)

        # Convert named colors to hex in marquee syntax
        # Supports {colorname text} in addition to {#hex text}
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

        # Default color palette for legend
        default_colors <- c("#808080", "#B0B0B0", "#E69F00", "#56B4E9", "#009E73",
                            "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

        # --- Color Palette Functions ---
        get_palette_names <- function(pkg) {
            switch(pkg,
                "MetBrewer" = if (requireNamespace("MetBrewer", quietly = TRUE)) {
                    names(MetBrewer::MetPalettes)
                } else character(0),
                "PNWColors" = if (requireNamespace("PNWColors", quietly = TRUE)) {
                    names(PNWColors::pnw_palettes)
                } else character(0),
                "RColorBrewer" = if (requireNamespace("RColorBrewer", quietly = TRUE)) {
                    rownames(RColorBrewer::brewer.pal.info)
                } else character(0),
                "viridis" = c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo"),
                character(0)
            )
        }

        get_palette_colors <- function(pkg, name, n = 8) {
            tryCatch({
                switch(pkg,
                    "MetBrewer" = MetBrewer::met.brewer(name, n),
                    "PNWColors" = PNWColors::pnw_palette(name, n),
                    "RColorBrewer" = {
                        max_n <- RColorBrewer::brewer.pal.info[name, "maxcolors"]
                        RColorBrewer::brewer.pal(min(n, max_n), name)
                    },
                    "viridis" = if (requireNamespace("viridis", quietly = TRUE)) {
                        viridis::viridis(n, option = name)
                    } else grDevices::hcl.colors(n, name),
                    grDevices::hcl.colors(n)
                )
            }, error = function(e) grDevices::hcl.colors(n))
        }

        # Reactive: available palettes for selected package
        available_palettes <- shiny::reactive({
            get_palette_names(input$palette_package %||% "none")
        })

        # Reactive: current palette index
        palette_idx <- shiny::reactiveVal(1)

        # Reset index when package changes
        shiny::observeEvent(input$palette_package, {
            palette_idx(1)
        })

        # Palette navigation buttons
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

        # Sync dropdown selection with index
        shiny::observeEvent(input$palette_name, {
            palettes <- available_palettes()
            idx <- match(input$palette_name, palettes)
            if (!is.na(idx)) palette_idx(idx)
        })

        # Render palette dropdown
        output$palette_choices <- shiny::renderUI({
            pkg <- input$palette_package %||% "none"
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
            shiny::selectInput("palette_name", "Palette", width = "100%",
                choices = palettes, selected = palettes[idx])
        })

        # Render palette preview swatches with hex tooltips and click-to-copy
        output$palette_preview <- shiny::renderUI({
            pkg <- input$palette_package %||% "none"
            if (pkg == "none") return(NULL)
            palettes <- available_palettes()
            if (length(palettes) == 0) return(NULL)
            idx <- min(palette_idx(), length(palettes))
            colors <- get_palette_colors(pkg, palettes[idx], 8)
            swatches <- lapply(seq_along(colors), function(i) {
                col <- colors[i]
                bslib::tooltip(
                    shiny::span(
                        style = paste0(
                            "display:inline-block;width:24px;height:24px;background:", col,
                            ";border:1px solid #ccc;border-radius:3px;margin-right:3px;cursor:pointer;",
                            "transition:transform 0.1s;"
                        ),
                        onclick = paste0(
                            "navigator.clipboard.writeText('", col, "');",
                            "this.style.transform='scale(1.2)';",
                            "setTimeout(() => this.style.transform='scale(1)', 150);"
                        )
                    ),
                    paste0(col, " (click to copy)")
                )
            })
            shiny::div(class = "mb-2", swatches)
        })

        # Get current palette colors
        current_palette <- shiny::reactive({
            pkg <- input$palette_package %||% "none"
            if (pkg == "none") return(NULL)
            palettes <- available_palettes()
            if (length(palettes) == 0) return(NULL)
            idx <- min(palette_idx(), length(palettes))
            get_palette_colors(pkg, palettes[idx], 12)
        })

        # --- End Color Palette Functions ---

        # Dynamic color inputs based on number of categories
        output$legend_color_inputs <- shiny::renderUI({
            labels <- trimws(strsplit(input$legend_labels %||% "", ",")[[1]])
            labels <- labels[labels != ""]
            n <- length(labels)
            if (n == 0) return(NULL)

            # Create a color input for each label
            color_inputs <- lapply(seq_len(n), function(i) {
                input_id <- paste0("legend_color_", i)
                # Get existing value or use default
                current_val <- input[[input_id]]
                default_val <- if (is.null(current_val)) default_colors[((i - 1) %% length(default_colors)) + 1] else current_val
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

        # Calculate text metrics - simplified for better defaults
        calc_text_metrics <- function(text, font_size, output_width, is_narrative = FALSE) {
            if (is.null(text) || text == "") {
                return(list(chars = 0, est_lines = 1, required_height = 0.08))
            }

            # Clean text for counting
            clean_text <- gsub("\\{[^}]+\\s+([^}]+)\\}", "\\1", text)
            clean_text <- gsub("\\*+", "", clean_text)
            chars <- nchar(clean_text)

            # Count explicit line breaks
            explicit_lines <- length(strsplit(text, "\n")[[1]])

            # Simple estimate: ~50-70 chars per line at typical widths
            chars_per_line <- 55
            wrapped_lines <- ceiling(chars / chars_per_line)
            est_lines <- max(explicit_lines, wrapped_lines)

            # Height based on font size and lines
            # Title at 16pt needs ~0.10-0.12 per line, subtitle at 11pt needs ~0.06-0.08
            base_height <- if (font_size >= 14) 0.10 else 0.06
            required_height <- est_lines * base_height + 0.02  # padding

            # Clamp to reasonable bounds
            required_height <- max(0.08, min(required_height, 0.35))

            list(chars = chars, est_lines = est_lines, required_height = round(required_height, 2))
        }

        title_metrics <- shiny::reactive({
            calc_text_metrics(title_text_d(), input$title_size, input$output_width)
        })

        subtitle_metrics <- shiny::reactive({
            calc_text_metrics(subtitle_text_d(), input$subtitle_size, input$output_width)
        })

        current_heights <- shiny::reactive({
            legend_h <- if (input$legend_enabled %||% FALSE) 0.04 else 0
            list(title = input$title_height %||% 0.12,
                 subtitle = input$subtitle_height %||% 0.08,
                 legend = legend_h,
                 caption = input$caption_height %||% 0.05)
        })

        # Apply plot theme and axis settings
        styled_plot <- shiny::reactive({
            p <- user_plot

            # Get base theme
            base_theme <- switch(input$plot_theme %||% "minimal",
                "minimal" = ggplot2::theme_minimal(),
                "stwd" = theme_stwd(),
                "void" = ggplot2::theme_void(),
                ggplot2::theme_minimal()
            )

            # X-Axis title styling
            x_title_face <- if (input$axis_title_x_bold %||% FALSE) "bold" else "plain"
            x_title_hjust <- as.numeric(input$axis_title_x_align %||% "0.5")
            x_title_angle <- as.numeric(input$axis_title_x_angle %||% "0")
            x_title_margin <- input$axis_title_x_margin %||% 5
            x_title_color <- input$axis_title_x_color %||% "#333333"

            # Y-Axis title styling
            y_title_face <- if (input$axis_title_y_bold %||% FALSE) "bold" else "plain"
            y_title_hjust <- as.numeric(input$axis_title_y_align %||% "0.5")
            y_title_angle <- as.numeric(input$axis_title_y_angle %||% "90")
            y_title_margin <- input$axis_title_y_margin %||% 5
            y_title_color <- input$axis_title_y_color %||% "#333333"

            # Axis text styling (just size and color)
            text_color <- input$axis_text_color %||% "#666666"
            text_size <- input$axis_text_size %||% 10

            # Grid settings
            grid_color <- input$grid_color %||% "#E5E5E5"
            grid_major <- input$grid_major %||% "both"
            grid_minor <- input$grid_minor %||% "none"

            # Build theme modifications
            theme_mods <- ggplot2::theme(
                axis.title.x = ggplot2::element_text(
                    size = input$axis_title_x_size %||% 11,
                    face = x_title_face,
                    hjust = x_title_hjust,
                    angle = x_title_angle,
                    color = x_title_color,
                    margin = ggplot2::margin(t = x_title_margin)
                ),
                axis.title.y = ggplot2::element_text(
                    size = input$axis_title_y_size %||% 11,
                    face = y_title_face,
                    # When horizontal (angle=0), use vjust for vertical position; when vertical (angle=90), use hjust
                    hjust = if (y_title_angle == 0) 0.5 else y_title_hjust,
                    vjust = if (y_title_angle == 0) y_title_hjust else 0.5,
                    angle = y_title_angle,
                    color = y_title_color,
                    margin = ggplot2::margin(r = y_title_margin)
                ),
                axis.text = ggplot2::element_text(
                    size = text_size,
                    color = text_color
                ),
                legend.position = input$plot_legend_pos %||% "right"
            )

            # Axis line and ticks
            axis_line_color <- input$axis_line_color %||% "#333333"
            if (input$show_axis_line %||% FALSE) {
                theme_mods <- theme_mods + ggplot2::theme(
                    axis.line = ggplot2::element_line(color = axis_line_color)
                )
            }
            if (input$show_ticks %||% FALSE) {
                theme_mods <- theme_mods + ggplot2::theme(
                    axis.ticks = ggplot2::element_line(color = axis_line_color)
                )
            } else {
                theme_mods <- theme_mods + ggplot2::theme(
                    axis.ticks = ggplot2::element_blank()
                )
            }

            # Grid lines - remove all overrides everything
            if (input$grid_remove_all %||% FALSE) {
                theme_mods <- theme_mods + ggplot2::theme(
                    panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank()
                )
            } else {
                # Major grid
                major_h <- if (grid_major %in% c("both", "h")) {
                    ggplot2::element_line(color = grid_color, linewidth = 0.5)
                } else ggplot2::element_blank()
                major_v <- if (grid_major %in% c("both", "v")) {
                    ggplot2::element_line(color = grid_color, linewidth = 0.5)
                } else ggplot2::element_blank()

                # Minor grid
                minor_h <- if (grid_minor %in% c("both", "h")) {
                    ggplot2::element_line(color = grid_color, linewidth = 0.25)
                } else ggplot2::element_blank()
                minor_v <- if (grid_minor %in% c("both", "v")) {
                    ggplot2::element_line(color = grid_color, linewidth = 0.25)
                } else ggplot2::element_blank()

                theme_mods <- theme_mods + ggplot2::theme(
                    panel.grid.major.y = major_h,
                    panel.grid.major.x = major_v,
                    panel.grid.minor.y = minor_h,
                    panel.grid.minor.x = minor_v
                )
            }

            # Apply color palette if selected
            p <- p + base_theme + theme_mods
            palette_colors <- current_palette()
            if (!is.null(palette_colors)) {
                apply_to <- input$palette_apply %||% "fill"
                scale_type <- input$palette_scale %||% "discrete"
                if (scale_type == "discrete") {
                    if (apply_to %in% c("fill", "both")) {
                        p <- p + ggplot2::scale_fill_manual(values = palette_colors)
                    }
                    if (apply_to %in% c("color", "both")) {
                        p <- p + ggplot2::scale_color_manual(values = palette_colors)
                    }
                } else {
                    # Continuous scales
                    if (apply_to %in% c("fill", "both")) {
                        p <- p + ggplot2::scale_fill_gradientn(colors = palette_colors)
                    }
                    if (apply_to %in% c("color", "both")) {
                        p <- p + ggplot2::scale_color_gradientn(colors = palette_colors)
                    }
                }
            }
            p
        })

        # Dimensions label
        output$dimensions_label <- shiny::renderText({
            paste0(input$output_width, '" x ', input$output_height, '"')
        })

        # Width display showing chart vs narrative percentages
        output$width_display <- shiny::renderUI({
            narr_pct <- round(input$narrative_width * 100)
            chart_pct <- 100 - narr_pct
            shiny::div(class = "d-flex justify-content-between small text-muted mb-1",
                shiny::span(paste0("Chart: ", chart_pct, "%")),
                shiny::span(paste0("Narrative: ", narr_pct, "%"))
            )
        })

        # Reset to defaults
        shiny::observeEvent(input$reset_defaults, {
            shiny::updateTextAreaInput(session, "title_text", value = "**Your {#E69F00 title} here**")
            shiny::updateTextAreaInput(session, "subtitle_text", value = "Supporting context for your visualization")
            shiny::updateTextAreaInput(session, "narrative_text", value = "**KEY INSIGHT:**\nYour narrative text here.\n\n**ACTION:**\nWhat should the audience do?")
            shiny::updateTextInput(session, "caption_text", value = "SOURCE: Your data source")
            shiny::updateSliderInput(session, "title_size", value = 16)
            shiny::updateSliderInput(session, "subtitle_size", value = 11)
            shiny::updateSliderInput(session, "narrative_size", value = 10)
            shiny::updateSliderInput(session, "caption_size", value = 9)
            shiny::updateSliderInput(session, "title_margin_bottom", value = 5)
            shiny::updateSliderInput(session, "subtitle_margin_bottom", value = 5)
            shiny::updateSliderInput(session, "narrative_width", value = 0.35)
            shiny::updateSliderInput(session, "title_height", value = 0.12)
            shiny::updateSliderInput(session, "subtitle_height", value = 0.08)
            shiny::updateSliderInput(session, "caption_height", value = 0.05)
            shiny::updateSelectInput(session, "narrative_position", selected = "right")
            shiny::updateSelectInput(session, "title_align", selected = "left")
            shiny::updateSelectInput(session, "subtitle_align", selected = "left")
            shiny::updateSelectInput(session, "narrative_halign", selected = "left")
            shiny::updateSelectInput(session, "narrative_valign", selected = "top")
            shiny::updateSelectInput(session, "caption_position", selected = "full_left")
            shiny::updateNumericInput(session, "title_lineheight", value = 1.1)
            shiny::updateNumericInput(session, "subtitle_lineheight", value = 1.2)
            shiny::updateNumericInput(session, "narrative_lineheight", value = 1.4)
            shiny::updateNumericInput(session, "narrative_padding", value = 10)
            shiny::updateTextInput(session, "caption_color", value = "#808080")
            # Plot settings
            shiny::updateSelectInput(session, "plot_theme", selected = "minimal")
            shiny::updateSelectInput(session, "plot_legend_pos", selected = "right")
            shiny::updateSelectInput(session, "palette_package", selected = "none")
            palette_idx(1)
            # X-Axis title
            shiny::updateSliderInput(session, "axis_title_x_size", value = 11)
            shiny::updateCheckboxInput(session, "axis_title_x_bold", value = FALSE)
            shiny::updateSelectInput(session, "axis_title_x_align", selected = "0.5")
            shiny::updateSelectInput(session, "axis_title_x_angle", selected = "0")
            shiny::updateSliderInput(session, "axis_title_x_margin", value = 5)
            shiny::updateTextInput(session, "axis_title_x_color", value = "#333333")
            # Y-Axis title
            shiny::updateSliderInput(session, "axis_title_y_size", value = 11)
            shiny::updateCheckboxInput(session, "axis_title_y_bold", value = FALSE)
            shiny::updateSelectInput(session, "axis_title_y_align", selected = "0.5")
            shiny::updateSelectInput(session, "axis_title_y_angle", selected = "90")
            shiny::updateSliderInput(session, "axis_title_y_margin", value = 5)
            shiny::updateTextInput(session, "axis_title_y_color", value = "#333333")
            # Axis text & lines
            shiny::updateSliderInput(session, "axis_text_size", value = 10)
            shiny::updateTextInput(session, "axis_text_color", value = "#666666")
            shiny::updateCheckboxInput(session, "show_axis_line", value = FALSE)
            shiny::updateCheckboxInput(session, "show_ticks", value = FALSE)
            shiny::updateTextInput(session, "axis_line_color", value = "#333333")
            shiny::updateSelectInput(session, "axis_text_x_angle", selected = "0")
            shiny::updateSelectInput(session, "axis_text_y_angle", selected = "0")
            # Grid
            shiny::updateCheckboxInput(session, "grid_remove_all", value = FALSE)
            shiny::updateSelectInput(session, "grid_major", selected = "both")
            shiny::updateSelectInput(session, "grid_minor", selected = "none")
            shiny::updateTextInput(session, "grid_color", value = "#E5E5E5")
        })

        # Preview with correct aspect ratio
        output$preview_container <- shiny::renderUI({
            aspect_pct <- (input$output_height / input$output_width) * 100
            shiny::div(
                style = paste0("position: relative; width: 100%; padding-top: ", aspect_pct, "%; background: white; border: 1px solid #dee2e6;"),
                shiny::div(
                    style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0;",
                    shiny::plotOutput("preview_plot", height = "100%", width = "100%")
                )
            )
        })

        # Metrics - simple inline badges (unused but kept for reference)
        output$title_metrics <- shiny::renderUI({
            NULL
        })

        output$subtitle_metrics <- shiny::renderUI({
            NULL
        })


        # Height diagram - vertical (top to bottom)
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

        # Build layout - using block functions directly for margin control
        build_layout <- shiny::reactive({
            h <- current_heights()

            # Convert named colors to hex (using debounced text inputs)
            title_txt <- convert_named_colors(title_text_d())
            subtitle_txt <- convert_named_colors(subtitle_text_d())
            narrative_txt <- convert_named_colors(narrative_text_d())
            caption_txt <- convert_named_colors(caption_text_d())

            # Create title block with fine tune settings
            title_margin <- input$title_margin_lr %||% 5
            title_plot <- title_block(
                title_txt,
                title_size = input$title_size,
                halign = input$title_align %||% "left",
                lineheight = input$title_lineheight %||% 1.1,
                margin_left = title_margin,
                margin_right = title_margin,
                margin_bottom = input$title_margin_bottom,
                wrap_width = if ((input$title_wrap %||% 0) > 0) input$title_wrap else NULL
            )

            # Create subtitle block with fine tune settings
            subtitle_margin <- input$subtitle_margin_lr %||% 5
            subtitle_plot <- subtitle_block(
                subtitle_txt,
                subtitle_size = input$subtitle_size,
                halign = input$subtitle_align %||% "left",
                lineheight = input$subtitle_lineheight %||% 1.2,
                margin_left = subtitle_margin,
                margin_right = subtitle_margin,
                margin_bottom = input$subtitle_margin_bottom,
                wrap_width = if ((input$subtitle_wrap %||% 0) > 0) input$subtitle_wrap else NULL
            )

            # Create narrative with fine tune settings
            narrative_plot <- text_narrative(
                narrative_txt,
                size = input$narrative_size,
                halign = input$narrative_halign %||% "left",
                valign = input$narrative_valign %||% "top",
                padding = input$narrative_padding %||% 10,
                lineheight = input$narrative_lineheight %||% 1.4
            )

            # Create legend block if enabled
            legend_plot <- NULL
            legend_pos <- input$legend_position %||% "above"
            if (input$legend_enabled %||% FALSE) {
                # Parse comma-separated labels
                labels <- trimws(strsplit(input$legend_labels %||% "", ",")[[1]])
                labels <- labels[labels != ""]
                n_labels <- length(labels)
                if (n_labels > 0) {
                    # Get color for each label from dynamic inputs
                    colors <- sapply(seq_len(n_labels), function(i) {
                        input[[paste0("legend_color_", i)]] %||% default_colors[((i - 1) %% length(default_colors)) + 1]
                    })
                    names(colors) <- labels
                    # Determine orientation based on position
                    orientation <- if (legend_pos %in% c("left", "right")) "vertical" else "horizontal"
                    # For vertical legends, align text to left/right based on position
                    h_align <- if (legend_pos == "left") "left" else if (legend_pos == "right") "right" else (input$legend_halign %||% "right")
                    legend_plot <- legend_block(
                        colors,
                        halign = h_align,
                        valign = "top",
                        orientation = orientation,
                        sep = input$legend_sep %||% " | ",
                        size = input$legend_size %||% 10,
                        bold = input$legend_bold %||% TRUE,
                        uppercase = input$legend_uppercase %||% FALSE,
                        lineheight = input$legend_lineheight %||% 1.6,
                        width = 0.95,
                        wrap_width = if ((input$legend_wrap %||% 0) > 0) input$legend_wrap else NULL
                    )
                }
            }

            # Create caption with fine tune settings
            # Map caption_position to halign
            caption_halign <- switch(input$caption_position %||% "full_left",
                "full_left" = "left",
                "full_center" = "center",
                "full_right" = "right",
                "under_chart" = "left",
                "left"
            )
            caption_plot <- caption_block(
                caption_txt,
                caption_size = input$caption_size,
                halign = caption_halign,
                color = input$caption_color %||% "#808080",
                wrap_width = if ((input$caption_wrap %||% 0) > 0) input$caption_wrap else NULL
            )

            # Build content area (plot + narrative)
            content <- styled_plot()
            plot_width <- 1 - input$narrative_width
            caption_under_chart <- (input$caption_position %||% "full_left") == "under_chart"

            # Calculate content height (account for legend if above/below)
            legend_h_above <- if (!is.null(legend_plot) && legend_pos %in% c("above", "below")) h$legend else 0
            content_height <- 1 - h$title - h$subtitle - legend_h_above - h$caption

            # Combine narrative with chart
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
                # bottom
                content <- content / narrative_plot +
                    patchwork::plot_layout(heights = c(plot_width, input$narrative_width))
            }

            # Handle legend positioning
            if (!is.null(legend_plot)) {
                if (legend_pos == "above") {
                    # Legend above chart (between subtitle and content)
                    result <- title_plot / subtitle_plot / legend_plot / content / caption_plot +
                        patchwork::plot_layout(heights = c(h$title, h$subtitle, h$legend, content_height, h$caption))
                } else if (legend_pos == "below") {
                    # Legend below chart (between content and caption)
                    result <- title_plot / subtitle_plot / content / legend_plot / caption_plot +
                        patchwork::plot_layout(heights = c(h$title, h$subtitle, content_height, h$legend, h$caption))
                } else if (legend_pos == "right") {
                    # Legend to right of chart (vertical)
                    # For top/bottom narrative, legend goes beside the whole content block
                    legend_w <- input$legend_width %||% 0.12
                    content_with_legend <- patchwork::wrap_plots(content, legend_plot, widths = c(1 - legend_w, legend_w))
                    result <- title_plot / subtitle_plot / content_with_legend / caption_plot +
                        patchwork::plot_layout(heights = c(h$title, h$subtitle, content_height, h$caption))
                } else if (legend_pos == "left") {
                    # Legend to left of chart (vertical)
                    legend_w <- input$legend_width %||% 0.12
                    content_with_legend <- patchwork::wrap_plots(legend_plot, content, widths = c(legend_w, 1 - legend_w))
                    result <- title_plot / subtitle_plot / content_with_legend / caption_plot +
                        patchwork::plot_layout(heights = c(h$title, h$subtitle, content_height, h$caption))
                }
            } else {
                result <- title_plot / subtitle_plot / content / caption_plot +
                    patchwork::plot_layout(heights = c(h$title, h$subtitle, content_height, h$caption))
            }

            result
        })

        output$preview_plot <- shiny::renderPlot({ build_layout() }, res = 96, bg = "white")

        # Actual size validation
        actual_size_data <- shiny::reactiveVal(NULL)

        shiny::observeEvent(input$validate_actual, {
            shiny::withProgress(message = "Rendering at actual size...", {
                temp_file <- tempfile(fileext = ".png")
                ggplot2::ggsave(temp_file, build_layout(),
                                width = input$output_width, height = input$output_height,
                                dpi = 150, bg = "white")
                img_data <- base64enc::base64encode(temp_file)
                actual_size_data(list(
                    data = img_data,
                    px_width = input$output_width * 150,
                    px_height = input$output_height * 150
                ))
                unlink(temp_file)
            })
        })

        output$actual_size_preview <- shiny::renderUI({
            img_info <- actual_size_data()
            if (is.null(img_info)) return(NULL)

            shiny::div(
                class = "mt-3 border rounded",
                shiny::div(
                    class = "bg-success text-white p-2 d-flex justify-content-between align-items-center",
                    shiny::span(
                        shiny::icon("check-circle"), " Actual Size ",
                        shiny::span(class = "badge bg-light text-dark",
                                    paste0(img_info$px_width, "x", img_info$px_height, "px"))
                    ),
                    shiny::actionButton("close_actual", "Close", class = "btn-sm btn-light")
                ),
                shiny::div(
                    class = "p-1 text-center small text-muted bg-light",
                    "This is EXACTLY what your PNG export will look like. Scroll to inspect."
                ),
                shiny::div(
                    style = "max-height: 400px; overflow: auto; border: 2px solid #28a745;",
                    shiny::tags$img(src = paste0("data:image/png;base64,", img_info$data))
                )
            )
        })

        shiny::observeEvent(input$close_actual, { actual_size_data(NULL) })

        # Export tab handlers
        export_data <- shiny::reactiveVal(NULL)

        output$export_info <- shiny::renderUI({
            w <- input$export_width %||% 12
            h <- input$export_height %||% 9
            dpi <- input$export_dpi %||% 150
            fmt <- toupper(input$export_format %||% "png")
            shiny::div(
                class = "text-muted small mb-2",
                shiny::strong(fmt), " output: ", w, '" x ', h, '" @ ', dpi, ' DPI = ',
                shiny::strong(round(w * dpi), " x ", round(h * dpi), " px")
            )
        })

        shiny::observeEvent(input$render_export, {
            shiny::withProgress(message = "Rendering export preview...", {
                w <- input$export_width %||% 12
                h <- input$export_height %||% 9
                dpi <- input$export_dpi %||% 150
                temp_file <- tempfile(fileext = ".png")
                ggplot2::ggsave(temp_file, build_layout(), width = w, height = h, dpi = dpi, bg = "white")
                img_data <- base64enc::base64encode(temp_file)
                export_data(list(data = img_data, px_width = round(w * dpi), px_height = round(h * dpi)))
                unlink(temp_file)
            })
        })

        output$export_preview <- shiny::renderUI({
            img_info <- export_data()
            if (is.null(img_info)) {
                return(shiny::div(
                    class = "text-center p-4 bg-light rounded",
                    shiny::icon("image", class = "fa-2x text-muted mb-2"), shiny::br(),
                    shiny::span(class = "text-muted", "Click 'Preview Export' to see output")
                ))
            }
            shiny::div(
                shiny::div(
                    class = "bg-success text-white p-2 rounded-top",
                    shiny::icon("check-circle"), " Preview: ",
                    shiny::strong(img_info$px_width, " x ", img_info$px_height, " px")
                ),
                shiny::div(
                    style = "max-height: 400px; overflow: auto; border: 2px solid #28a745; border-top: none;",
                    shiny::tags$img(src = paste0("data:image/png;base64,", img_info$data), style = "display: block;")
                )
            )
        })

        output$download_export <- shiny::downloadHandler(
            filename = function() {
                fmt <- input$export_format %||% "png"
                paste0("story_layout_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt)
            },
            content = function(file) {
                w <- input$export_width %||% 12
                h <- input$export_height %||% 9
                dpi <- input$export_dpi %||% 150
                fmt <- input$export_format %||% "png"
                if (fmt == "pdf") {
                    ggplot2::ggsave(file, build_layout(), width = w, height = h, device = "pdf", bg = "white")
                } else if (fmt == "svg") {
                    ggplot2::ggsave(file, build_layout(), width = w, height = h, device = "svg", bg = "white")
                } else {
                    ggplot2::ggsave(file, build_layout(), width = w, height = h, dpi = dpi, bg = "white")
                }
            }
        )

        # Component previews - with named color conversion
        output$preview_chart <- shiny::renderPlot({
            styled_plot()
        }, res = 96, bg = "white")

        output$preview_title <- shiny::renderPlot({
            title_block(convert_named_colors(title_text_d()), title_size = input$title_size,
                        margin_bottom = input$title_margin_bottom)
        }, res = 96, bg = "white")

        output$preview_subtitle <- shiny::renderPlot({
            subtitle_block(convert_named_colors(subtitle_text_d()), subtitle_size = input$subtitle_size,
                           margin_bottom = input$subtitle_margin_bottom)
        }, res = 96, bg = "white")

        output$preview_narrative <- shiny::renderPlot({
            text_narrative(convert_named_colors(narrative_text_d()), size = input$narrative_size,
                halign = input$narrative_halign %||% "left",
                lineheight = input$narrative_lineheight %||% 1.4)
        }, res = 96, bg = "white")

        output$preview_caption <- shiny::renderPlot({
            caption_halign <- switch(input$caption_position %||% "full_left",
                "full_left" = "left", "full_center" = "center", "full_right" = "right", "under_chart" = "left", "left")
            # Use smaller margins for preview so caption is clearly visible
            caption_block(convert_named_colors(caption_text_d()), caption_size = input$caption_size,
                halign = caption_halign, color = input$caption_color %||% "#808080",
                margin_top = 2, margin_bottom = 2)
        }, res = 96, bg = "white")

        # Generated code
        code_to_copy <- shiny::reactive({
            h <- current_heights()
            caption_halign <- switch(input$caption_position %||% "full_left",
                "full_left" = "left", "full_center" = "center", "full_right" = "right", "under_chart" = "left", "left")

            # Build plot theme code
            theme_name <- input$plot_theme %||% "minimal"
            theme_fn <- switch(theme_name,
                "minimal" = "theme_minimal()",
                "stwd" = "theme_stwd()",
                "void" = "theme_void()",
                "theme_minimal()"
            )
            # X-Axis title
            x_title_face <- if (input$axis_title_x_bold %||% FALSE) "bold" else "plain"
            x_title_angle <- input$axis_title_x_angle %||% "0"
            x_title_margin <- input$axis_title_x_margin %||% 5
            # Y-Axis title
            y_title_face <- if (input$axis_title_y_bold %||% FALSE) "bold" else "plain"
            y_title_angle <- input$axis_title_y_angle %||% "90"
            y_title_margin <- input$axis_title_y_margin %||% 5
            # Axis line and ticks code
            axis_line_code <- ""
            if (input$show_axis_line %||% FALSE) {
                axis_line_code <- paste0(axis_line_code, ',\n        axis.line = element_line(color = "', input$axis_line_color %||% "#333333", '")')
            }
            if (input$show_ticks %||% FALSE) {
                axis_line_code <- paste0(axis_line_code, ',\n        axis.ticks = element_line(color = "', input$axis_line_color %||% "#333333", '")')
            } else {
                axis_line_code <- paste0(axis_line_code, ',\n        axis.ticks = element_blank()')
            }

            # Grid code
            grid_code <- if (input$grid_remove_all %||% FALSE) {
                ',\n        panel.grid.major = element_blank(),\n        panel.grid.minor = element_blank()'
            } else {
                grid_major <- input$grid_major %||% "both"
                grid_minor <- input$grid_minor %||% "none"
                grid_color <- input$grid_color %||% "#E5E5E5"
                grid_lines <- c()
                # Major
                if (grid_major == "none") {
                    grid_lines <- c(grid_lines, 'panel.grid.major = element_blank()')
                } else if (grid_major == "h") {
                    grid_lines <- c(grid_lines, paste0('panel.grid.major.y = element_line(color = "', grid_color, '")'),
                                    'panel.grid.major.x = element_blank()')
                } else if (grid_major == "v") {
                    grid_lines <- c(grid_lines, 'panel.grid.major.y = element_blank()',
                                    paste0('panel.grid.major.x = element_line(color = "', grid_color, '")'))
                }
                # Minor
                if (grid_minor == "none") {
                    grid_lines <- c(grid_lines, 'panel.grid.minor = element_blank()')
                } else if (grid_minor == "h") {
                    grid_lines <- c(grid_lines, paste0('panel.grid.minor.y = element_line(color = "', grid_color, '")'),
                                    'panel.grid.minor.x = element_blank()')
                } else if (grid_minor == "v") {
                    grid_lines <- c(grid_lines, 'panel.grid.minor.y = element_blank()',
                                    paste0('panel.grid.minor.x = element_line(color = "', grid_color, '")'))
                } else if (grid_minor == "both") {
                    grid_lines <- c(grid_lines, paste0('panel.grid.minor = element_line(color = "', grid_color, '")'))
                }
                if (length(grid_lines) > 0) paste0(',\n        ', paste(grid_lines, collapse = ',\n        ')) else ""
            }

            # Always show patchwork code so users can see all settings
            paste0(
                'library(patchwork)\n',
                '# Add ... args to pass extra options to marquee (e.g., family = "Arial")\n\n',
                '# Style the plot\n',
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
                '        legend.position = "', input$plot_legend_pos %||% "right", '"', axis_line_code, grid_code, '\n',
                '    )',
                # Add palette code if selected
                if ((input$palette_package %||% "none") != "none") {
                    pkg <- input$palette_package
                    palettes <- available_palettes()
                    idx <- min(palette_idx(), length(palettes))
                    pal_name <- if (length(palettes) > 0) palettes[idx] else "viridis"
                    apply_to <- input$palette_apply %||% "fill"

                    pal_fn <- switch(pkg,
                        "MetBrewer" = paste0('MetBrewer::met.brewer("', pal_name, '")'),
                        "PNWColors" = paste0('PNWColors::pnw_palette("', pal_name, '", 8)'),
                        "RColorBrewer" = paste0('RColorBrewer::brewer.pal(8, "', pal_name, '")'),
                        "viridis" = paste0('viridis::viridis(8, option = "', pal_name, '")'),
                        'viridis::viridis(8)'
                    )

                    scale_code <- ""
                    scale_type <- input$palette_scale %||% "discrete"
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
                } else "",
                '\n\n',
                'title_plot <- title_block(\n',
                '    "', gsub('"', '\\"', input$title_text), '",\n',
                '    title_size = ', input$title_size, ',\n',
                '    halign = "', input$title_align %||% "left", '",\n',
                '    lineheight = ', input$title_lineheight %||% 1.1, ',\n',
                '    margin_bottom = ', input$title_margin_bottom,
                if ((input$title_wrap %||% 0) > 0) paste0(',\n    wrap_width = ', input$title_wrap) else '', '\n',
                ')\n\n',
                'subtitle_plot <- subtitle_block(\n',
                '    "', gsub('"', '\\"', input$subtitle_text), '",\n',
                '    subtitle_size = ', input$subtitle_size, ',\n',
                '    halign = "', input$subtitle_align %||% "left", '",\n',
                '    lineheight = ', input$subtitle_lineheight %||% 1.2, ',\n',
                '    margin_bottom = ', input$subtitle_margin_bottom,
                if ((input$subtitle_wrap %||% 0) > 0) paste0(',\n    wrap_width = ', input$subtitle_wrap) else '', '\n',
                ')\n\n',
                'narrative_plot <- text_narrative(\n',
                '    "', gsub('\n', '\\n', gsub('"', '\\"', input$narrative_text)), '",\n',
                '    size = ', input$narrative_size, ',\n',
                '    halign = "', input$narrative_halign %||% "left", '",\n',
                '    valign = "', input$narrative_valign %||% "top", '",\n',
                '    lineheight = ', input$narrative_lineheight %||% 1.4, ',\n',
                '    padding = ', input$narrative_padding %||% 10, '\n',
                ')\n\n',
                'caption_plot <- caption_block(\n',
                '    "', gsub('"', '\\"', input$caption_text), '",\n',
                '    caption_size = ', input$caption_size, ',\n',
                '    halign = "', caption_halign, '",\n',
                '    color = "', input$caption_color %||% "#808080", '"',
                if ((input$caption_wrap %||% 0) > 0) paste0(',\n    wrap_width = ', input$caption_wrap) else '', '\n',
                ')\n\n',
                # Legend code if enabled
                if (input$legend_enabled %||% FALSE) {
                    labels <- trimws(strsplit(input$legend_labels %||% "", ",")[[1]])
                    labels <- labels[labels != ""]
                    n_labels <- length(labels)
                    if (n_labels > 0) {
                        colors <- sapply(seq_len(n_labels), function(i) {
                            input[[paste0("legend_color_", i)]] %||% default_colors[((i - 1) %% length(default_colors)) + 1]
                        })
                        color_vec <- paste0('c(', paste0('"', labels, '" = "', colors, '"', collapse = ', '), ')')
                        legend_pos <- input$legend_position %||% "above"
                        orientation <- if (legend_pos %in% c("left", "right")) "vertical" else "horizontal"
                        h_align <- if (legend_pos == "left") "left" else if (legend_pos == "right") "right" else (input$legend_halign %||% "right")
                        lineheight_code <- if (orientation == "vertical") paste0(',\n    lineheight = ', input$legend_lineheight %||% 1.6) else ""
                        wrap_code <- if ((input$legend_wrap %||% 0) > 0) paste0(',\n    wrap_width = ', input$legend_wrap) else ""
                        paste0(
                            'legend_plot <- legend_block(\n',
                            '    ', color_vec, ',\n',
                            '    halign = "', h_align, '",\n',
                            '    orientation = "', orientation, '",\n',
                            '    sep = "', input$legend_sep %||% " | ", '",\n',
                            '    size = ', input$legend_size %||% 10, ',\n',
                            '    bold = ', if (input$legend_bold %||% TRUE) "TRUE" else "FALSE", ',\n',
                            '    uppercase = ', if (input$legend_uppercase %||% FALSE) "TRUE" else "FALSE",
                            lineheight_code, wrap_code, '\n',
                            ')\n\n'
                        )
                    } else ""
                } else "",
                '# Combine styled plot + narrative\n',
                'content <- styled_plot + narrative_plot +\n',
                '    plot_layout(widths = c(', round(1 - input$narrative_width, 2), ', ', input$narrative_width, '))\n\n',
                '# Stack everything\n',
                if (input$legend_enabled %||% FALSE) {
                    legend_pos <- input$legend_position %||% "above"
                    content_h <- round(1 - h$title - h$subtitle - (if (legend_pos %in% c("above", "below")) h$legend else 0) - h$caption, 3)
                    if (legend_pos == "above") {
                        paste0(
                            'final <- title_plot / subtitle_plot / legend_plot / content / caption_plot +\n',
                            '    plot_layout(heights = c(', h$title, ', ', h$subtitle, ', ', h$legend, ', ', content_h, ', ', h$caption, '))'
                        )
                    } else if (legend_pos == "below") {
                        paste0(
                            'final <- title_plot / subtitle_plot / content / legend_plot / caption_plot +\n',
                            '    plot_layout(heights = c(', h$title, ', ', h$subtitle, ', ', content_h, ', ', h$legend, ', ', h$caption, '))'
                        )
                    } else if (legend_pos == "right") {
                        legend_w <- input$legend_width %||% 0.12
                        paste0(
                            'content_with_legend <- patchwork::wrap_plots(content, legend_plot, widths = c(', round(1 - legend_w, 2), ', ', legend_w, '))\n',
                            'final <- title_plot / subtitle_plot / content_with_legend / caption_plot +\n',
                            '    plot_layout(heights = c(', h$title, ', ', h$subtitle, ', ', content_h, ', ', h$caption, '))'
                        )
                    } else {
                        legend_w <- input$legend_width %||% 0.12
                        paste0(
                            'content_with_legend <- patchwork::wrap_plots(legend_plot, content, widths = c(', legend_w, ', ', round(1 - legend_w, 2), '))\n',
                            'final <- title_plot / subtitle_plot / content_with_legend / caption_plot +\n',
                            '    plot_layout(heights = c(', h$title, ', ', h$subtitle, ', ', content_h, ', ', h$caption, '))'
                        )
                    }
                } else {
                    paste0(
                        'final <- title_plot / subtitle_plot / content / caption_plot +\n',
                        '    plot_layout(heights = c(', h$title, ', ', h$subtitle, ', ',
                        round(1 - h$title - h$subtitle - h$caption, 3), ', ', h$caption, '))'
                    )
                }
            )
        })

        output$generated_code <- shiny::renderUI({
            shiny::pre(class = "bg-dark text-light p-3 rounded", style = "font-size: 12px;", code_to_copy())
        })

        # Copy and download
        shiny::observeEvent(input$copy_code, {
            if (requireNamespace("clipr", quietly = TRUE)) {
                clipr::write_clip(code_to_copy())
                shiny::showNotification("Copied!", type = "message", duration = 2)
            } else {
                shiny::showNotification("Install 'clipr' for clipboard", type = "warning")
            }
        })

        output$download_plot <- shiny::downloadHandler(
            filename = function() paste0("story_layout_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
            content = function(file) {
                ggplot2::ggsave(file, build_layout(), width = input$output_width,
                                height = input$output_height, dpi = 150, bg = "white")
            }
        )
    }

    shiny::shinyApp(ui = ui, server = server)
}
