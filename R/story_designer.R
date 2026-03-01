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
                           narrative = "**KEY INSIGHT:**
Your narrative text here.

**ACTION:**
What should the audience do?",
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
                open = c("Title", "Subtitle"),

                bslib::accordion_panel(
                    title = "Title",
                    icon = shiny::icon("heading"),
                    shiny::textAreaInput("title_text", NULL, value = title, rows = 2, width = "100%",
                                         placeholder = "Use **bold** or {#E69F00 color}"),
                    shiny::sliderInput("title_size", "Font size", min = 10, max = 24, value = 16, step = 1),
                    shiny::sliderInput("title_margin_bottom", "Space below (pt)", min = 0, max = 30, value = 5, step = 1),
                    shiny::uiOutput("title_metrics")
                ),

                bslib::accordion_panel(
                    title = "Subtitle",
                    icon = shiny::icon("font"),
                    shiny::textAreaInput("subtitle_text", NULL, value = subtitle, rows = 2, width = "100%"),
                    shiny::sliderInput("subtitle_size", "Font size", min = 8, max = 16, value = 11, step = 1),
                    shiny::sliderInput("subtitle_margin_bottom", "Space below (pt)", min = 0, max = 30, value = 5, step = 1),
                    shiny::uiOutput("subtitle_metrics")
                ),

                bslib::accordion_panel(
                    title = "Narrative",
                    icon = shiny::icon("align-left"),
                    shiny::textAreaInput("narrative_text", NULL, value = narrative, rows = 4, width = "100%"),
                    shiny::selectInput("narrative_position", "Position",
                                       choices = c("right", "left", "bottom"), selected = "right"),
                    shiny::sliderInput("narrative_width", "Width", min = 0.15, max = 0.50, value = 0.35, step = 0.05),
                    shiny::sliderInput("narrative_size", "Font size", min = 8, max = 14, value = 10, step = 1)
                ),

                bslib::accordion_panel(
                    title = "Caption",
                    icon = shiny::icon("quote-right"),
                    shiny::textInput("caption_text", NULL, value = caption, width = "100%"),
                    shiny::sliderInput("caption_size", "Font size", min = 7, max = 12, value = 9, step = 1)
                )
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
                    bslib::nav_panel("Narrative", shiny::plotOutput("preview_narrative", height = "150px"))
                )
            )
        )
    )

    # Server
    server <- function(input, output, session) {

        # Define %||% locally for NULL handling
        `%||%` <- function(x, y) if (is.null(x)) y else x

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
            calc_text_metrics(input$title_text, input$title_size, input$output_width)
        })

        subtitle_metrics <- shiny::reactive({
            calc_text_metrics(input$subtitle_text, input$subtitle_size, input$output_width)
        })

        current_heights <- shiny::reactive({
            list(title = input$title_height %||% 0.12,
                 subtitle = input$subtitle_height %||% 0.08,
                 caption = input$caption_height %||% 0.05)
        })

        # Dimensions label
        output$dimensions_label <- shiny::renderText({
            paste0(input$output_width, '" x ', input$output_height, '"')
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

            # Convert named colors to hex
            title_txt <- convert_named_colors(input$title_text)
            subtitle_txt <- convert_named_colors(input$subtitle_text)
            narrative_txt <- convert_named_colors(input$narrative_text)
            caption_txt <- convert_named_colors(input$caption_text)

            # Create title block with custom margins
            title_plot <- title_block(
                title_txt,
                title_size = input$title_size,
                margin_bottom = input$title_margin_bottom
            )

            # Create subtitle block with custom margins
            subtitle_plot <- subtitle_block(
                subtitle_txt,
                subtitle_size = input$subtitle_size,
                margin_bottom = input$subtitle_margin_bottom
            )

            # Create narrative
            narrative_plot <- text_narrative(
                narrative_txt,
                size = input$narrative_size
            )

            # Create caption
            caption_plot <- caption_block(
                caption_txt,
                caption_size = input$caption_size
            )

            # Build content area (plot + narrative)
            content <- user_plot
            plot_width <- 1 - input$narrative_width
            if (input$narrative_position == "right") {
                content <- content + narrative_plot +
                    patchwork::plot_layout(widths = c(plot_width, input$narrative_width))
            } else if (input$narrative_position == "left") {
                content <- narrative_plot + content +
                    patchwork::plot_layout(widths = c(input$narrative_width, plot_width))
            } else {
                content <- content / narrative_plot +
                    patchwork::plot_layout(heights = c(plot_width, input$narrative_width))
            }

            # Calculate content height
            content_height <- 1 - h$title - h$subtitle - h$caption

            # Stack everything vertically
            result <- title_plot / subtitle_plot / content / caption_plot +
                patchwork::plot_layout(heights = c(h$title, h$subtitle, content_height, h$caption))

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
            user_plot
        }, res = 96, bg = "white")

        output$preview_title <- shiny::renderPlot({
            title_block(convert_named_colors(input$title_text), title_size = input$title_size,
                        margin_bottom = input$title_margin_bottom)
        }, res = 96, bg = "white")

        output$preview_subtitle <- shiny::renderPlot({
            subtitle_block(convert_named_colors(input$subtitle_text), subtitle_size = input$subtitle_size,
                           margin_bottom = input$subtitle_margin_bottom)
        }, res = 96, bg = "white")

        output$preview_narrative <- shiny::renderPlot({
            text_narrative(convert_named_colors(input$narrative_text), size = input$narrative_size)
        }, res = 96, bg = "white")

        # Generated code
        code_to_copy <- shiny::reactive({
            h <- current_heights()

            # Check if using custom margins (non-default values)
            uses_custom_margins <- input$title_margin_bottom != 5 || input$subtitle_margin_bottom != 5

            if (uses_custom_margins) {
                # Generate code using block functions for margin control
                paste0(
                    '# Custom margins require using block functions directly\n',
                    'library(patchwork)\n\n',
                    'title_plot <- title_block(\n',
                    '    "', gsub('"', '\\"', input$title_text), '",\n',
                    '    title_size = ', input$title_size, ',\n',
                    '    margin_bottom = ', input$title_margin_bottom, '\n',
                    ')\n\n',
                    'subtitle_plot <- subtitle_block(\n',
                    '    "', gsub('"', '\\"', input$subtitle_text), '",\n',
                    '    subtitle_size = ', input$subtitle_size, ',\n',
                    '    margin_bottom = ', input$subtitle_margin_bottom, '\n',
                    ')\n\n',
                    'narrative_plot <- text_narrative(\n',
                    '    "', gsub('\n', '\\n', gsub('"', '\\"', input$narrative_text)), '",\n',
                    '    size = ', input$narrative_size, '\n',
                    ')\n\n',
                    'caption_plot <- caption_block("', gsub('"', '\\"', input$caption_text), '")\n\n',
                    '# Combine plot + narrative\n',
                    'content <- my_plot + narrative_plot +\n',
                    '    plot_layout(widths = c(', round(1 - input$narrative_width, 2), ', ', input$narrative_width, '))\n\n',
                    '# Stack everything\n',
                    'final <- title_plot / subtitle_plot / content / caption_plot +\n',
                    '    plot_layout(heights = c(', h$title, ', ', h$subtitle, ', ',
                    round(1 - h$title - h$subtitle - h$caption, 3), ', ', h$caption, '))'
                )
            } else {
                # Standard story_layout code
                paste0(
                    'story_layout(\n',
                    '    plot = my_plot,\n',
                    '    title = "', gsub('"', '\\"', input$title_text), '",\n',
                    '    subtitle = "', gsub('"', '\\"', input$subtitle_text), '",\n',
                    '    narrative = "', gsub('\n', '\\n', gsub('"', '\\"', input$narrative_text)), '",\n',
                    '    caption = "', gsub('"', '\\"', input$caption_text), '",\n',
                    '    narrative_position = "', input$narrative_position, '",\n',
                    '    narrative_width = ', input$narrative_width, ',\n',
                    '    title_size = ', input$title_size, ',\n',
                    '    subtitle_size = ', input$subtitle_size, ',\n',
                    '    narrative_size = ', input$narrative_size, ',\n',
                    '    caption_size = ', input$caption_size, ',\n',
                    '    title_height = ', h$title, ',\n',
                    '    subtitle_height = ', h$subtitle, ',\n',
                    '    caption_height = ', h$caption, '\n',
                    ')'
                )
            }
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
