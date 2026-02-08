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
                           narrative = "**KEY INSIGHT:**\nYour narrative text here.\n\n**ACTION:**\nWhat should the audience do?",
                           caption = "SOURCE: Your data source") {


    if (!requireNamespace("shiny", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg shiny} is required for {.fn story_designer}.",
            "i" = "Install it with: {.code install.packages('shiny')}"
        )
    }

    if (!requireNamespace("marquee", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg marquee} is required for {.fn story_designer}.",
            "i" = "Install it with: {.code install.packages('marquee')}"
        )
    }

    # Store the user's plot in the app environment
    user_plot <- plot

    # Default placeholder plot if none provided
    if (is.null(user_plot)) {
        user_plot <- ggplot2::ggplot(
            data.frame(x = c("A", "B", "C", "D"), y = c(4, 7, 3, 8)),
            ggplot2::aes(x = x, y = y)
        ) +
            ggplot2::geom_col(fill = "#E69F00") +
            theme_ibcs() +
            ggplot2::labs(x = NULL, y = NULL, title = NULL)
    }

    # UI
    ui <- shiny::fluidPage(
        shiny::tags$head(
            shiny::tags$style(shiny::HTML("
                .clipping-warning {
                    background-color: #FFEBEE;
                    border-left: 4px solid #E63946;
                    padding: 10px;
                    margin: 5px 0;
                    font-family: monospace;
                }
                .clipping-ok {
                    background-color: #E8F5E9;
                    border-left: 4px solid #4CAF50;
                    padding: 10px;
                    margin: 5px 0;
                    font-family: monospace;
                }
                .metric-box {
                    background-color: #F5F5F5;
                    padding: 8px 12px;
                    margin: 3px;
                    border-radius: 4px;
                    display: inline-block;
                    font-family: monospace;
                    font-size: 12px;
                }
                .section-header {
                    font-weight: bold;
                    margin-top: 15px;
                    margin-bottom: 10px;
                    padding-bottom: 5px;
                    border-bottom: 2px solid #333;
                }
                .code-output {
                    background-color: #2D2D2D;
                    color: #F8F8F2;
                    padding: 15px;
                    font-family: 'Fira Code', 'Consolas', monospace;
                    font-size: 12px;
                    border-radius: 5px;
                    white-space: pre-wrap;
                    max-height: 300px;
                    overflow-y: auto;
                }
                .height-bar {
                    height: 20px;
                    margin: 5px 0;
                    border-radius: 3px;
                }
                .height-bar-allocated {
                    background-color: #2196F3;
                }
                .height-bar-required {
                    background-color: #E63946;
                    opacity: 0.7;
                }
                .preview-container {
                    border: 1px solid #ddd;
                    padding: 10px;
                    background-color: white;
                }
            "))
        ),

        shiny::titlePanel("Story Layout Designer - Clipping Debugger"),

        shiny::fluidRow(
            # Left column - Component inputs
            shiny::column(4,
                shiny::div(class = "section-header", "OUTPUT SETTINGS"),
                shiny::fluidRow(
                    shiny::column(6,
                        shiny::numericInput("output_width", "Output Width (inches)",
                                            value = 12, min = 6, max = 20, step = 1)
                    ),
                    shiny::column(6,
                        shiny::numericInput("output_height", "Output Height (inches)",
                                            value = 9, min = 4, max = 16, step = 1)
                    )
                ),

                shiny::div(class = "section-header", "TITLE"),
                shiny::textAreaInput("title_text", NULL, value = title,
                                     rows = 2, width = "100%",
                                     placeholder = "Enter title with **bold** or {#color text}"),
                shiny::sliderInput("title_size", "Font Size (pt)",
                                   min = 10, max = 24, value = 16, step = 1),
                shiny::uiOutput("title_metrics"),

                shiny::div(class = "section-header", "SUBTITLE"),
                shiny::textAreaInput("subtitle_text", NULL, value = subtitle,
                                     rows = 2, width = "100%"),
                shiny::sliderInput("subtitle_size", "Font Size (pt)",
                                   min = 8, max = 16, value = 11, step = 1),
                shiny::uiOutput("subtitle_metrics"),

                shiny::div(class = "section-header", "NARRATIVE"),
                shiny::textAreaInput("narrative_text", NULL, value = narrative,
                                     rows = 4, width = "100%"),
                shiny::sliderInput("narrative_size", "Font Size (pt)",
                                   min = 8, max = 14, value = 10, step = 1),
                shiny::selectInput("narrative_position", "Position",
                                   choices = c("right", "left", "bottom"),
                                   selected = "right"),
                shiny::sliderInput("narrative_width", "Width",
                                   min = 0.15, max = 0.50, value = 0.35, step = 0.05),

                shiny::div(class = "section-header", "CAPTION"),
                shiny::textInput("caption_text", NULL, value = caption, width = "100%"),
                shiny::sliderInput("caption_size", "Font Size (pt)",
                                   min = 7, max = 12, value = 9, step = 1)
            ),

            # Middle column - Height controls and clipping status
            shiny::column(4,
                shiny::div(class = "section-header", "HEIGHT ALLOCATION"),
                shiny::checkboxInput("auto_heights", "Auto-calculate heights", value = TRUE),

                shiny::conditionalPanel(
                    condition = "!input.auto_heights",
                    shiny::sliderInput("title_height", "Title Height",
                                       min = 0.05, max = 0.40, value = 0.15, step = 0.01),
                    shiny::sliderInput("subtitle_height", "Subtitle Height",
                                       min = 0.03, max = 0.20, value = 0.07, step = 0.01),
                    shiny::sliderInput("caption_height", "Caption Height",
                                       min = 0.02, max = 0.10, value = 0.04, step = 0.01)
                ),

                shiny::div(class = "section-header", "CLIPPING STATUS"),
                shiny::uiOutput("clipping_status"),

                shiny::div(class = "section-header", "HEIGHT VISUALIZATION"),
                shiny::plotOutput("height_diagram", height = "200px"),

                shiny::div(class = "section-header", "GENERATED CODE"),
                shiny::uiOutput("generated_code"),
                shiny::actionButton("copy_code", "Copy Code", class = "btn-primary"),
                shiny::downloadButton("download_plot", "Download PNG")
            ),

            # Right column - Live preview
            shiny::column(4,
                shiny::div(class = "section-header", "LIVE PREVIEW"),
                shiny::div(class = "preview-container",
                    shiny::plotOutput("preview_plot", height = "500px")
                ),

                shiny::div(class = "section-header", "INDIVIDUAL COMPONENTS"),
                shiny::tabsetPanel(
                    shiny::tabPanel("Title",
                        shiny::plotOutput("preview_title", height = "100px")
                    ),
                    shiny::tabPanel("Subtitle",
                        shiny::plotOutput("preview_subtitle", height = "80px")
                    ),
                    shiny::tabPanel("Narrative",
                        shiny::plotOutput("preview_narrative", height = "150px")
                    )
                )
            )
        )
    )

    # Server
    server <- function(input, output, session) {

        # Calculate text metrics
        calc_text_metrics <- function(text, font_size, output_width, is_narrative = FALSE) {
            if (is.null(text) || text == "") {
                return(list(chars = 0, est_lines = 0, required_height = 0))
            }

            # Clean marquee formatting
            clean_text <- gsub("\\{#[A-Fa-f0-9]+ ([^}]+)\\}", "\\1", text)
            clean_text <- gsub("\\*+", "", clean_text)
            chars <- nchar(clean_text)

            # Estimate lines (same logic as estimate_layout_heights)
            if (is_narrative) {
                # Narrative is in a narrower column
                effective_width <- output_width * input$narrative_width * 0.85
            } else {
                effective_width <- output_width * 0.85
            }

            base_chars_per_line <- 28
            chars_per_line <- max(15, base_chars_per_line - (font_size - 12))
            chars_per_line <- floor(chars_per_line * (effective_width / 12))

            # Count explicit newlines
            explicit_lines <- length(strsplit(text, "\n")[[1]])

            # Estimate wrapped lines (with word-wrap overhead)
            est_lines <- ceiling(chars / max(chars_per_line, 15) * 1.3)
            est_lines <- max(est_lines, explicit_lines)

            # Calculate required height
            height_per_line <- 0.05 + (font_size / 250)
            required_height <- max(0.08, est_lines * height_per_line + 0.04)

            # Cap at reasonable max
            if (!is_narrative) {
                required_height <- min(required_height, 0.40)
            }

            list(
                chars = chars,
                est_lines = est_lines,
                chars_per_line = chars_per_line,
                required_height = round(required_height, 3)
            )
        }

        # Reactive: title metrics
        title_metrics <- shiny::reactive({
            calc_text_metrics(input$title_text, input$title_size, input$output_width)
        })

        # Reactive: subtitle metrics
        subtitle_metrics <- shiny::reactive({
            calc_text_metrics(input$subtitle_text, input$subtitle_size, input$output_width)
        })

        # Reactive: current heights (auto or manual)
        current_heights <- shiny::reactive({
            if (input$auto_heights) {
                list(
                    title = title_metrics()$required_height,
                    subtitle = subtitle_metrics()$required_height,
                    caption = 0.04
                )
            } else {
                list(
                    title = input$title_height,
                    subtitle = input$subtitle_height,
                    caption = input$caption_height
                )
            }
        })

        # Output: title metrics display
        output$title_metrics <- shiny::renderUI({
            m <- title_metrics()
            h <- current_heights()

            is_clipping <- !input$auto_heights && h$title < m$required_height
            css_class <- if (is_clipping) "clipping-warning" else "clipping-ok"

            shiny::div(class = css_class,
                shiny::span(class = "metric-box", paste0("Chars: ", m$chars)),
                shiny::span(class = "metric-box", paste0("Lines: ~", m$est_lines)),
                shiny::span(class = "metric-box", paste0("Required: ", m$required_height)),
                shiny::span(class = "metric-box", paste0("Allocated: ", h$title)),
                if (is_clipping) {
                    shiny::div(style = "margin-top: 5px; color: #E63946;",
                        shiny::icon("exclamation-triangle"),
                        " CLIPPING! ",
                        shiny::actionButton("fix_title", "Auto-fix",
                                            class = "btn-sm btn-danger")
                    )
                }
            )
        })

        # Output: subtitle metrics display
        output$subtitle_metrics <- shiny::renderUI({
            m <- subtitle_metrics()
            h <- current_heights()

            is_clipping <- !input$auto_heights && h$subtitle < m$required_height
            css_class <- if (is_clipping) "clipping-warning" else "clipping-ok"

            shiny::div(class = css_class,
                shiny::span(class = "metric-box", paste0("Chars: ", m$chars)),
                shiny::span(class = "metric-box", paste0("Lines: ~", m$est_lines)),
                shiny::span(class = "metric-box", paste0("Required: ", m$required_height)),
                shiny::span(class = "metric-box", paste0("Allocated: ", h$subtitle)),
                if (is_clipping) {
                    shiny::div(style = "margin-top: 5px; color: #E63946;",
                        shiny::icon("exclamation-triangle"),
                        " CLIPPING! ",
                        shiny::actionButton("fix_subtitle", "Auto-fix",
                                            class = "btn-sm btn-danger")
                    )
                }
            )
        })

        # Auto-fix buttons
        shiny::observeEvent(input$fix_title, {
            shiny::updateSliderInput(session, "title_height",
                                     value = title_metrics()$required_height)
        })

        shiny::observeEvent(input$fix_subtitle, {
            shiny::updateSliderInput(session, "subtitle_height",
                                     value = subtitle_metrics()$required_height)
        })

        # Output: overall clipping status
        output$clipping_status <- shiny::renderUI({
            h <- current_heights()
            tm <- title_metrics()
            sm <- subtitle_metrics()

            content_height <- 1 - h$title - h$subtitle - h$caption

            issues <- character(0)
            if (!input$auto_heights) {
                if (h$title < tm$required_height) {
                    issues <- c(issues, paste0("Title clipped (need ", tm$required_height, ")"))
                }
                if (h$subtitle < sm$required_height) {
                    issues <- c(issues, paste0("Subtitle clipped (need ", sm$required_height, ")"))
                }
            }
            if (content_height < 0.3) {
                issues <- c(issues, paste0("Content area small (", round(content_height, 2), ")"))
            }

            if (length(issues) == 0) {
                shiny::div(class = "clipping-ok",
                    shiny::icon("check-circle"),
                    " All components fit! Content area: ", round(content_height * 100), "%"
                )
            } else {
                shiny::div(class = "clipping-warning",
                    shiny::icon("exclamation-triangle"),
                    " Issues detected:",
                    shiny::tags$ul(
                        lapply(issues, shiny::tags$li)
                    )
                )
            }
        })

        # Output: height visualization diagram
        output$height_diagram <- shiny::renderPlot({
            h <- current_heights()
            content_height <- 1 - h$title - h$subtitle - h$caption

            # Create stacked bar showing height allocation
            df <- data.frame(
                component = factor(c("Title", "Subtitle", "Content", "Caption"),
                                   levels = c("Caption", "Content", "Subtitle", "Title")),
                height = c(h$title, h$subtitle, content_height, h$caption),
                label = c(
                    paste0("Title: ", round(h$title * 100), "%"),
                    paste0("Subtitle: ", round(h$subtitle * 100), "%"),
                    paste0("Content: ", round(content_height * 100), "%"),
                    paste0("Caption: ", round(h$caption * 100), "%")
                )
            )

            ggplot2::ggplot(df, ggplot2::aes(x = 1, y = height, fill = component)) +
                ggplot2::geom_col(width = 0.5, color = "white", linewidth = 1) +
                ggplot2::geom_text(ggplot2::aes(label = label),
                                   position = ggplot2::position_stack(vjust = 0.5),
                                   size = 3.5, color = "white", fontface = "bold") +
                ggplot2::scale_fill_manual(values = c(
                    "Title" = "#2196F3",
                    "Subtitle" = "#64B5F6",
                    "Content" = "#4CAF50",
                    "Caption" = "#9E9E9E"
                )) +
                ggplot2::coord_flip() +
                ggplot2::theme_void() +
                ggplot2::theme(legend.position = "none")
        })

        # Output: preview plot
        output$preview_plot <- shiny::renderPlot({
            h <- current_heights()

            story_layout(
                plot = user_plot,
                title = input$title_text,
                subtitle = input$subtitle_text,
                narrative = input$narrative_text,
                caption = input$caption_text,
                narrative_position = input$narrative_position,
                narrative_width = input$narrative_width,
                title_size = input$title_size,
                subtitle_size = input$subtitle_size,
                narrative_size = input$narrative_size,
                caption_size = input$caption_size,
                title_height = h$title,
                subtitle_height = h$subtitle,
                caption_height = h$caption,
                auto_heights = FALSE  # We control heights explicitly
            )
        }, res = 96)

        # Output: individual component previews
        output$preview_title <- shiny::renderPlot({
            title_block(input$title_text, title_size = input$title_size)
        }, res = 96)

        output$preview_subtitle <- shiny::renderPlot({
            subtitle_block(input$subtitle_text, subtitle_size = input$subtitle_size)
        }, res = 96)

        output$preview_narrative <- shiny::renderPlot({
            text_narrative(input$narrative_text, size = input$narrative_size)
        }, res = 96)

        # Output: generated code
        output$generated_code <- shiny::renderUI({
            h <- current_heights()

            code <- paste0(
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
                if (input$auto_heights) {
                    paste0('    auto_heights = TRUE,\n',
                           '    output_width = ', input$output_width, '\n')
                } else {
                    paste0('    title_height = ', h$title, ',\n',
                           '    subtitle_height = ', h$subtitle, ',\n',
                           '    caption_height = ', h$caption, ',\n',
                           '    auto_heights = FALSE\n')
                },
                ')'
            )

            shiny::div(class = "code-output",
                shiny::tags$code(code)
            )
        })

        # Store code for copying
        code_to_copy <- shiny::reactive({
            h <- current_heights()

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
                if (input$auto_heights) {
                    paste0('    auto_heights = TRUE,\n',
                           '    output_width = ', input$output_width, '\n')
                } else {
                    paste0('    title_height = ', h$title, ',\n',
                           '    subtitle_height = ', h$subtitle, ',\n',
                           '    caption_height = ', h$caption, ',\n',
                           '    auto_heights = FALSE\n')
                },
                ')'
            )
        })

        # Copy code button
        shiny::observeEvent(input$copy_code, {
            if (requireNamespace("clipr", quietly = TRUE)) {
                clipr::write_clip(code_to_copy())
                shiny::showNotification("Code copied to clipboard!", type = "message")
            } else {
                shiny::showNotification(
                    "Install 'clipr' package for clipboard support. Code shown above.",
                    type = "warning"
                )
            }
        })

        # Download handler
        output$download_plot <- shiny::downloadHandler(
            filename = function() {
                paste0("story_layout_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
            },
            content = function(file) {
                h <- current_heights()

                p <- story_layout(
                    plot = user_plot,
                    title = input$title_text,
                    subtitle = input$subtitle_text,
                    narrative = input$narrative_text,
                    caption = input$caption_text,
                    narrative_position = input$narrative_position,
                    narrative_width = input$narrative_width,
                    title_size = input$title_size,
                    subtitle_size = input$subtitle_size,
                    narrative_size = input$narrative_size,
                    caption_size = input$caption_size,
                    title_height = h$title,
                    subtitle_height = h$subtitle,
                    caption_height = h$caption,
                    auto_heights = FALSE
                )

                ggplot2::ggsave(file, p,
                                width = input$output_width,
                                height = input$output_height,
                                dpi = 150, bg = "white")
            }
        )
    }

    # Launch the app
    shiny::shinyApp(ui = ui, server = server)
}
