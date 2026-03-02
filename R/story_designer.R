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
#' @param title Initial title text. Supports marquee formatting:
#'   `**bold**`, `*italic*`, `{#E69F00 colored text}`.
#' @param subtitle Initial subtitle text.
#' @param narrative Initial narrative text for the side panel.
#' @param caption Initial caption/source text.
#'
#' @returns Launches a Shiny app. Returns NULL invisibly.
#'
#' @section App Features:
#' **Text Blocks:**
#' - Title, Subtitle, Narrative, Caption with marquee formatting
#' - Font size, alignment, line height, wrap width controls
#' - Section height allocation
#'
#' **Legend Block:**
#' - Inline colored text legend (e.g., "ACCURATE | NULL | ERROR")
#' - Position: above, below, left, or right of chart
#' - Separator, size, bold, uppercase options
#'
#' **Color Palette:**
#' - 9 palette packages: ggsci, MetBrewer, nord, PNWColors, rcartocolor,
#'   RColorBrewer, scico, viridis, wesanderson
#' - Click swatches to select specific colors
#' - Discrete or continuous scale application
#' - Apply to fill, color, or both
#'
#' **Manual Colors:**
#' - Set default color for unassigned categories (e.g., gray)
#' - Assign specific colors to individual categories by name or number
#' - Useful for highlighting specific data points
#'
#' **Plot Styling:**
#' - Themes: STWD (clean SWD-style) or Void (minimal)
#' - Axis labels: size, bold, alignment, rotation, color
#' - Grid lines: major/minor, horizontal/vertical, color
#' - Axis lines and tick marks
#'
#' **Export:**
#' - PNG, PDF, or SVG format
#' - Custom dimensions and DPI
#' - "Validate Actual Size" to preview true output
#' - Copy generated patchwork code
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
#' p <- ggplot(mtcars, aes(factor(cyl), mpg, fill = factor(cyl))) +
#'   geom_col()
#' story_designer(
#'   plot = p,
#'   title = "**Fuel Economy** by {#E69F00 Cylinder Count}",
#'   subtitle = "MPG varies significantly across engine sizes",
#'   narrative = "**Key finding:** 4-cylinder cars have the best fuel economy.",
#'   caption = "SOURCE: mtcars dataset"
#' )
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

    # Register resource path for CSS
    css_path <- system.file("www", package = "stwd")
    if (nzchar(css_path)) {
        shiny::addResourcePath("stwd-assets", css_path)
    }

    # UI
    ui <- bslib::page_sidebar(
        title = "Story Layout Designer",
        theme = bslib::bs_theme(
            version = 5,
            bootswatch = "flatly",
            primary = "#2c3e50"
        ),
        # Include custom CSS
        shiny::tags$head(
            shiny::tags$link(rel = "stylesheet", type = "text/css", href = "stwd-assets/story_designer.css")
        ),

        # Sidebar with inputs
        sidebar = bslib::sidebar(
            id = "main_sidebar",
            width = 320,
            open = TRUE,

            # Output dimensions - compact
            shiny::div(
                class = "mb-3 p-2 bg-light rounded",
                shiny::div(
                    class = "d-flex justify-content-between align-items-center mb-2",
                    shiny::strong("Output Dimensions"),
                    shiny::div(
                        class = "d-flex align-items-center gap-1",
                        shiny::tags$small("Sidebar:", class = "text-muted"),
                        shiny::selectInput("sidebar_width", NULL, width = "70px",
                            choices = c("280" = "280", "320" = "320", "380" = "380", "450" = "450", "520" = "520"),
                            selected = "320")
                    )
                ),
                bslib::layout_column_wrap(
                    width = 1/2,
                    shiny::numericInput("output_width", "W (in)", value = 12, min = 6, max = 20, step = 1),
                    shiny::numericInput("output_height", "H (in)", value = 9, min = 4, max = 16, step = 1)
                )
            ),

            # Accordion for components - using UI helper functions
            bslib::accordion(
                id = "inputs_accordion",
                open = FALSE,
                text_panel("title", "Title", "primary", "heading", title,
                           size_min = 6, size_max = 24, size_default = 12),
                text_panel("subtitle", "Subtitle", "info", "font", subtitle,
                           size_min = 8, size_max = 16, size_default = 11),
                narrative_panel(narrative),
                text_panel("caption", "Caption", "secondary", "quote-right", caption,
                           rows = 1, show_margin = FALSE, size_min = 7, size_max = 12, size_default = 9,
                           tooltip = "Source attribution (e.g., SOURCE: Company Database)"),
                legend_panel(),
                plot_panel(),
                palette_panel(),
                manual_colors_panel(),
                axis_grid_panel()
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
                    # Controls on left
                    bslib::card(
                        bslib::card_header(class = "bg-light py-2", "Section Heights"),
                        bslib::card_body(
                            shiny::sliderInput("title_height", "Title", min = 0.05, max = 0.40, value = 0.08, step = 0.01),
                            shiny::sliderInput("subtitle_height", "Subtitle", min = 0.03, max = 0.20, value = 0.06, step = 0.01),
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
                        ),
                        shiny::sliderInput("title_wrap", "Wrap at chars (0=off)", min = 0, max = 80, value = 0, step = 5)
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
                        ),
                        shiny::sliderInput("subtitle_wrap", "Wrap at chars (0=off)", min = 0, max = 80, value = 0, step = 5)
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
                        shiny::numericInput("narrative_padding", "Padding (pt)", value = 10, min = 0, max = 30, step = 1, width = "100%"),
                        shiny::sliderInput("narrative_wrap", "Wrap at chars (0=off)", min = 0, max = 80, value = 0, step = 5)
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
                        ),
                        shiny::sliderInput("caption_wrap", "Wrap at chars (0=off)", min = 0, max = 100, value = 0, step = 5)
                    )
                )
            ),

            # Validate Size & Export tab
            bslib::nav_panel(
                title = shiny::span(shiny::icon("search-plus"), " Validate & Export"),
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
                        shiny::actionButton("validate_actual", "Validate Actual Size",
                                            class = "btn-success", icon = shiny::icon("search-plus")),
                        shiny::actionButton("popout_preview", "Pop Out",
                                            class = "btn-outline-secondary", icon = shiny::icon("external-link-alt")),
                        shiny::downloadButton("download_export", "Download", class = "btn-primary")
                    ),
                    shiny::uiOutput("export_info"),
                    shiny::div(
                        class = "bg-light p-2 rounded mb-3",
                        shiny::div(
                            class = "d-flex justify-content-between align-items-center mb-1",
                            shiny::tags$small(class = "text-muted fw-bold", "Quarto code chunk options:"),
                            shiny::tags$button(
                                id = "copy_quarto_opts",
                                class = "btn btn-sm btn-outline-secondary py-0",
                                onclick = "navigator.clipboard.writeText(document.getElementById('quarto-opts-text').innerText).then(function() { document.getElementById('copy_quarto_opts').innerHTML = '<i class=\"fa fa-check\"></i> Copied'; setTimeout(function() { document.getElementById('copy_quarto_opts').innerHTML = '<i class=\"fa fa-copy\"></i> Copy'; }, 1500); });",
                                shiny::icon("copy"), " Copy"
                            )
                        ),
                        shiny::uiOutput("quarto_chunk_opts")
                    ),
                    shiny::uiOutput("actual_size_preview")
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

        # Use helper functions from story_designer_utils.R
        # convert_named_colors(), get_palette_names(), get_palette_colors()
        # are defined there and available in package namespace

        # Default color palette for legend (from utils)
        default_colors <- default_legend_colors

        # --- Sidebar Width Resize ---
        shiny::observeEvent(input$sidebar_width, {
            new_width <- paste0(input$sidebar_width, "px")
            shiny::insertUI(
                selector = "head",
                where = "beforeEnd",
                ui = shiny::tags$style(
                    shiny::HTML(paste0(".bslib-sidebar-layout > .sidebar { width: ", new_width, " !important; }"))
                ),
                immediate = TRUE
            )
        })

        # --- Detect Plot Categories ---
        plot_categories <- shiny::reactive({
            tryCatch({
                detect_plot_categories(user_plot)
            }, error = function(e) {
                list(fill_levels = NULL, color_levels = NULL, n_fill = 0, n_color = 0)
            })
        })

        # --- Manual Color Assignments Storage ---
        manual_color_values <- shiny::reactiveVal(list())

        # --- Color Palette Functions ---
        # Using get_palette_names() and get_palette_colors() from story_designer_utils.R

        # Reactive: available palettes for selected package
        available_palettes <- shiny::reactive({
            get_palette_names(input$palette_package %||% "none")
        })

        # Reactive: current palette index
        palette_idx <- shiny::reactiveVal(1)

        # Reactive: selected color indices (for custom selection)
        selected_colors <- shiny::reactiveVal(integer(0))

        # Reset index and selection when package changes
        shiny::observeEvent(input$palette_package, {
            palette_idx(1)
            selected_colors(integer(0))
        })

        # Reset selection when palette changes
        shiny::observeEvent(input$palette_name, {
            selected_colors(integer(0))
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

        # Handle swatch clicks for selection
        shiny::observeEvent(input$swatch_click, {
            clicked_idx <- input$swatch_click
            current <- selected_colors()
            if (clicked_idx %in% current) {
                selected_colors(setdiff(current, clicked_idx))
            } else {
                selected_colors(c(current, clicked_idx))
            }
        })

        # Render palette preview swatches with selection support
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
                # Show selection order number if selected
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
                            onclick = paste0("Shiny.setInputValue('swatch_click', ", i, ", {priority: 'event'});")
                        ),
                        badge
                    ),
                    paste0(col, if (is_selected) " (selected)" else " (click to select)")
                )
            })

            # Show selected colors as copyable text
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

        # Get current palette colors (use selected if any, otherwise all)
        current_palette <- shiny::reactive({
            pkg <- input$palette_package %||% "none"
            if (pkg == "none") return(NULL)
            palettes <- available_palettes()
            if (length(palettes) == 0) return(NULL)
            idx <- min(palette_idx(), length(palettes))
            all_colors <- get_palette_colors(pkg, palettes[idx], 8)
            sel <- selected_colors()
            if (length(sel) > 0) {
                # Return only selected colors in selection order
                all_colors[sel]
            } else {
                all_colors
            }
        })

        # Render palette warning when categories > colors
        output$palette_warning <- shiny::renderUI({
            pkg <- input$palette_package %||% "none"
            if (pkg == "none") return(NULL)

            cats <- plot_categories()
            palette_colors <- current_palette()
            n_colors <- length(palette_colors)

            apply_to <- input$palette_apply %||% "fill"
            n_cats <- if (apply_to == "fill") cats$n_fill else if (apply_to == "color") cats$n_color else max(cats$n_fill, cats$n_color)

            if (n_cats > 0 && n_cats > n_colors) {
                shiny::div(
                    class = "alert alert-warning py-1 px-2 small mb-2",
                    shiny::icon("exclamation-triangle"),
                    paste0(" ", n_cats, " categories but only ", n_colors, " colors. Consider Manual Colors section for precise control.")
                )
            } else NULL
        })

        # --- End Color Palette Functions ---

        # --- Manual Colors Category Assignment UI ---
        output$category_assignments <- shiny::renderUI({
            cats <- plot_categories()
            apply_to <- input$manual_apply %||% "fill"

            # Get the appropriate levels based on apply_to
            levels_to_use <- if (apply_to == "fill") {
                cats$fill_levels
            } else if (apply_to == "color") {
                cats$color_levels
            } else {
                # Both - prefer fill levels
                if (!is.null(cats$fill_levels)) cats$fill_levels else cats$color_levels
            }

            if (is.null(levels_to_use) || length(levels_to_use) == 0) {
                return(shiny::div(
                    class = "text-muted small",
                    shiny::icon("info-circle"),
                    " No discrete categories detected in plot"
                ))
            }

            # Get available colors from current palette (or default palette)
            palette_colors <- current_palette()
            if (is.null(palette_colors) || length(palette_colors) == 0) {
                palette_colors <- default_colors
            }

            # Create color choices: "default" + numbered colors
            color_choices <- c("Default" = "default")
            for (i in seq_along(palette_colors)) {
                color_choices[paste0("#", i, " (", palette_colors[i], ")")] <- palette_colors[i]
            }

            assign_mode <- input$assign_mode %||% "number"

            # Create assignment UI for each category
            assignment_ui <- lapply(seq_along(levels_to_use), function(i) {
                level_name <- levels_to_use[i]
                input_id <- paste0("cat_color_", i)

                # Create label with truncation
                if (assign_mode == "number") {
                    label_text <- paste0("#", i)
                    needs_tooltip <- FALSE
                } else {
                    if (nchar(level_name) > 12) {
                        label_text <- paste0(substr(level_name, 1, 10), "..")
                        needs_tooltip <- TRUE
                    } else {
                        label_text <- level_name
                        needs_tooltip <- FALSE
                    }
                }

                # Label element with optional tooltip
                label_elem <- if (needs_tooltip) {
                    bslib::tooltip(
                        shiny::span(label_text, class = "small",
                            style = "width:70px;display:inline-block;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;cursor:help;"),
                        level_name
                    )
                } else {
                    shiny::span(label_text, class = "small",
                        style = "width:70px;display:inline-block;")
                }

                shiny::div(
                    class = "d-flex align-items-center gap-2 mb-1",
                    label_elem,
                    shiny::selectInput(input_id, NULL, width = "100%",
                        choices = color_choices,
                        selected = "default")
                )
            })

            shiny::div(
                shiny::div(class = "small text-muted mb-2",
                    paste0(length(levels_to_use), " categories detected")),
                assignment_ui
            )
        })

        # Collect manual color assignments
        shiny::observe({
            cats <- plot_categories()
            apply_to <- input$manual_apply %||% "fill"

            levels_to_use <- if (apply_to == "fill") {
                cats$fill_levels
            } else if (apply_to == "color") {
                cats$color_levels
            } else {
                if (!is.null(cats$fill_levels)) cats$fill_levels else cats$color_levels
            }

            if (is.null(levels_to_use)) return()

            # Collect values from dynamic inputs
            color_map <- list()
            for (i in seq_along(levels_to_use)) {
                val <- input[[paste0("cat_color_", i)]]
                if (!is.null(val) && val != "default") {
                    color_map[[levels_to_use[i]]] <- val
                }
            }
            manual_color_values(color_map)
        })

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

        current_heights <- shiny::reactive({
            legend_h <- if (input$legend_enabled %||% FALSE) 0.04 else 0
            list(title = input$title_height %||% 0.12,
                 subtitle = input$subtitle_height %||% 0.08,
                 legend = legend_h,
                 caption = input$caption_height %||% 0.05)
        })

        # Apply plot theme and axis settings (using helpers from story_designer_utils.R)
        styled_plot <- shiny::reactive({
            # Base theme
            base_theme <- switch(input$plot_theme %||% "stwd",
                "stwd" = theme_stwd(),
                "void" = ggplot2::theme_void(),
                theme_stwd()
            )

            # Build theme modifications
            theme_mods <- build_theme_mods(input, `%||%`)

            # Apply base theme and modifications
            p <- user_plot + base_theme + theme_mods

            # Apply palette colors
            p <- apply_color_scales(p, current_palette(),
                input$palette_apply %||% "fill", input$palette_scale %||% "discrete")

            # Apply manual colors if enabled (overrides palette)
            if (input$manual_colors_enabled %||% FALSE) {
                color_map <- manual_color_values()
                default_col <- input$default_color %||% "#808080"
                manual_apply <- input$manual_apply %||% "fill"

                if (length(color_map) > 0 || default_col != "#808080") {
                    cats <- plot_categories()
                    levels_to_use <- if (manual_apply == "fill") {
                        cats$fill_levels
                    } else if (manual_apply == "color") {
                        cats$color_levels
                    } else {
                        if (!is.null(cats$fill_levels)) cats$fill_levels else cats$color_levels
                    }

                    if (!is.null(levels_to_use) && length(levels_to_use) > 0) {
                        final_colors <- sapply(levels_to_use, function(lvl) {
                            if (lvl %in% names(color_map)) color_map[[lvl]] else default_col
                        })
                        names(final_colors) <- levels_to_use
                        p <- apply_color_scales(p, final_colors, manual_apply, "discrete")
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

        # Reset to defaults (using helper from story_designer_utils.R)
        shiny::observeEvent(input$reset_defaults, {
            reset_all_inputs(session)
            palette_idx(1)
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
            title_plot <- title_block(
                title_txt,
                title_size = input$title_size,
                halign = input$title_align %||% "left",
                lineheight = input$title_lineheight %||% 1.1,
                margin_left = 5,
                margin_right = 5,
                margin_bottom = input$title_margin_bottom,
                wrap_width = if ((input$title_wrap %||% 0) > 0) input$title_wrap else NULL
            )

            # Create subtitle block with fine tune settings
            subtitle_plot <- subtitle_block(
                subtitle_txt,
                subtitle_size = input$subtitle_size,
                halign = input$subtitle_align %||% "left",
                lineheight = input$subtitle_lineheight %||% 1.2,
                margin_left = 5,
                margin_right = 5,
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
                lineheight = input$narrative_lineheight %||% 1.4,
                wrap_width = if ((input$narrative_wrap %||% 0) > 0) input$narrative_wrap else NULL
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
                w <- input$export_width %||% 12
                h <- input$export_height %||% 9
                dpi <- input$export_dpi %||% 150
                temp_file <- tempfile(fileext = ".png")
                ggplot2::ggsave(temp_file, build_layout(),
                                width = w, height = h,
                                dpi = dpi, bg = "white")
                img_data <- base64enc::base64encode(temp_file)
                actual_size_data(list(
                    data = img_data,
                    px_width = w * dpi,
                    px_height = h * dpi
                ))
                unlink(temp_file)
            })
        })

        output$actual_size_preview <- shiny::renderUI({
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
                    shiny::actionButton("close_actual", "Close", class = "btn-sm btn-light")
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

        shiny::observeEvent(input$close_actual, { actual_size_data(NULL) })

        # Pop out preview in new window
        shiny::observeEvent(input$popout_preview, {
            img_info <- actual_size_data()
            if (is.null(img_info)) {
                # Generate if not already done
                shiny::withProgress(message = "Rendering at actual size...", {
                    temp_file <- tempfile(fileext = ".png")
                    ggplot2::ggsave(temp_file, build_layout(),
                                    width = input$export_width %||% 12,
                                    height = input$export_height %||% 9,
                                    dpi = input$export_dpi %||% 150, bg = "white")
                    img_data <- base64enc::base64encode(temp_file)
                    actual_size_data(list(
                        data = img_data,
                        px_width = (input$export_width %||% 12) * (input$export_dpi %||% 150),
                        px_height = (input$export_height %||% 9) * (input$export_dpi %||% 150)
                    ))
                    unlink(temp_file)
                })
                img_info <- actual_size_data()
            }
            # Open in new browser window via JavaScript
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

        # Export tab handlers
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

        output$quarto_chunk_opts <- shiny::renderUI({
            w <- input$export_width %||% 12
            h <- input$export_height %||% 9
            dpi <- input$export_dpi %||% 150
            shiny::tags$pre(
                id = "quarto-opts-text",
                class = "mb-0 small",
                style = "background: white; padding: 8px; border-radius: 4px; font-family: monospace;",
                paste0("#| fig-width: ", w, "\n#| fig-height: ", h, "\n#| fig-dpi: ", dpi)
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

        # Generated code - using helper functions from story_designer_utils.R
        code_to_copy <- shiny::reactive({
            h <- current_heights()
            caption_halign <- switch(input$caption_position %||% "full_left",
                "full_left" = "left", "full_center" = "center", "full_right" = "right", "under_chart" = "left", "left")

            # Get palette info for code generation
            pkg <- input$palette_package %||% "none"
            pal_name <- if (pkg != "none") {
                palettes <- available_palettes()
                if (length(palettes) > 0) palettes[min(palette_idx(), length(palettes))] else "viridis"
            } else NULL

            # Build legend code if enabled
            legend_code <- ""
            if (input$legend_enabled %||% FALSE) {
                labels <- trimws(strsplit(input$legend_labels %||% "", ",")[[1]])
                labels <- labels[labels != ""]
                if (length(labels) > 0) {
                    colors <- sapply(seq_along(labels), function(i) {
                        input[[paste0("legend_color_", i)]] %||% default_colors[((i - 1) %% length(default_colors)) + 1]
                    })
                    legend_pos <- input$legend_position %||% "above"
                    orientation <- if (legend_pos %in% c("left", "right")) "vertical" else "horizontal"
                    h_align <- if (legend_pos == "left") "left" else if (legend_pos == "right") "right" else (input$legend_halign %||% "right")
                    legend_code <- generate_legend_code(
                        labels, colors, h_align, orientation, input$legend_sep %||% " | ",
                        input$legend_size %||% 10, input$legend_bold %||% TRUE, input$legend_uppercase %||% FALSE,
                        if (orientation == "vertical") input$legend_lineheight %||% 1.6 else NULL,
                        input$legend_wrap %||% 0
                    )
                }
            }

            paste0(
                'library(patchwork)\n',
                '# Add ... args to pass extra options to marquee (e.g., family = "Arial")\n\n',
                '# Style the plot\n',
                generate_theme_code(input, `%||%`),
                generate_palette_code(pkg, pal_name, input$palette_apply %||% "fill", input$palette_scale %||% "discrete"),
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
                    valign = input$narrative_valign %||% "top", padding = input$narrative_padding %||% 10),
                generate_block_code("caption", input$caption_text, input$caption_size,
                    caption_halign, lineheight = NULL, wrap_width = input$caption_wrap %||% 0,
                    color = input$caption_color %||% "#808080"),
                legend_code,
                generate_composition_code(h, input$narrative_width,
                    input$legend_enabled %||% FALSE, input$legend_position %||% "above",
                    input$legend_width %||% 0.12)
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
