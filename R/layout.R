# Layout Functions
# Functions for composing story layouts with patchwork

#' Estimate Layout Heights for Text
#'
#' Estimates appropriate height proportions for title, subtitle, and narrative
#' based on text length and font size. Use this to avoid text clipping.
#' This is called automatically by [story_layout()] when `auto_heights = TRUE`.
#'
#' @param title Title text (or NULL).
#' @param subtitle Subtitle text (or NULL).
#' @param narrative Narrative text (or NULL).
#' @param title_size Title font size in pts. Default: 16.
#' @param subtitle_size Subtitle font size in pts. Default: 11.
#' @param narrative_size Narrative font size in pts. Default: 10.
#' @param caption Caption text (or NULL).
#' @param narrative_width Width proportion for narrative (affects line wrapping). Default: 0.35.
#' @param output_width Expected output width in inches. Default: 12.
#'   Larger widths mean fewer line wraps and smaller height requirements.
#'
#' @returns A named list with recommended height proportions:
#' - `title_height`: Proportion for title (0-1)
#' - `subtitle_height`: Proportion for subtitle (0-1)
#' - `caption_height`: Proportion for caption (0-1)
#' - `content_height`: Remaining space for plot/narrative (0-1)
#' - `narrative_width`: Echo of input (for convenience)
#' @keywords internal
#'
estimate_layout_heights <- function(title = NULL,
                                     subtitle = NULL,
                                     narrative = NULL,
                                     title_size = 12,
                                     subtitle_size = 10,
                                     narrative_size = 10,
                                     caption = NULL,
                                     narrative_width = 0.35,
                                     output_width = 12) {

    # Estimate characters per line based on font size and output width
    # Larger fonts = fewer chars per line
    # output_width is in inches (typical: 10-14 for presentations)

    title_height <- 0
    subtitle_height <- 0
    caption_height <- 0

    if (!is.null(title) && is.character(title)) {
        clean_title <- strip_marquee_formatting(title)

        # Empirical: bold text at 16pt wraps at ~25-30 chars per line
        # But word wrapping means actual lines are fewer chars on average
        base_chars_per_line <- 28
        # Scale down for larger fonts (larger font = fewer chars fit)
        chars_per_line <- max(15, base_chars_per_line - (title_size - 12))
        # Adjust for output width (wider output = more chars per line)
        chars_per_line <- floor(chars_per_line * (output_width / 12))

        # Word wrapping adds ~30% overhead vs perfect char packing
        est_lines <- ceiling(nchar(clean_title) / max(chars_per_line, 15) * 1.3)
        # Height per line: at 16pt, each line needs ~0.07-0.08 of total height
        height_per_line <- 0.05 + (title_size / 250)
        # Add padding for margins
        title_height <- max(0.10, est_lines * height_per_line + 0.02)
        # Cap at reasonable maximum - very long titles should be shortened
        title_height <- min(title_height, 0.40)
    }

    if (!is.null(subtitle) && is.character(subtitle)) {
        clean_subtitle <- strip_marquee_formatting(subtitle)

        # At 11pt, ~40-50 chars per line at typical width
        base_chars_per_line <- 45
        chars_per_line <- max(25, base_chars_per_line - (subtitle_size - 10))
        chars_per_line <- floor(chars_per_line * (output_width / 12))

        # Word wrapping adds overhead
        est_lines <- ceiling(nchar(clean_subtitle) / max(chars_per_line, 25) * 1.2)
        height_per_line <- 0.035 + (subtitle_size / 400)
        subtitle_height <- max(0.04, est_lines * height_per_line + 0.01)
        # Cap at reasonable max
        subtitle_height <- min(subtitle_height, 0.20)
    }

    if (!is.null(caption) && is.character(caption)) {
        caption_height <- 0.04
    }

    # Calculate remaining space for content
    content_height <- 1 - title_height - subtitle_height - caption_height

    list(
        title_height = round(title_height, 3),
        subtitle_height = round(subtitle_height, 3),
        caption_height = round(caption_height, 3),
        content_height = round(content_height, 3),
        narrative_width = narrative_width
    )
}

#' Compose a Story Layout
#'
#' Combines a chart with optional title, narrative text, and caption
#' into a complete story layout using patchwork. Each component is a
#' modular plot element, giving full control over sizing and positioning.
#'
#' This function creates layouts inspired by
#' [Storytelling with Data](https://www.storytellingwithdata.com/) principles,
#' with large, prominent titles and supporting narrative text.
#'
#' @param plot The main ggplot visualization.
#' @param title Optional title - either text (marquee-formatted) or a ggplot object
#'   created by [title_block()]. When text is provided, it's converted to a
#'   title_block with the specified title_size.
#' @param subtitle Optional subtitle - either text (marquee-formatted) or a ggplot
#'   object created by [subtitle_block()].
#' @param narrative Optional narrative - either text (marquee-formatted) or a ggplot
#'   object created by [text_narrative()].
#' @param caption Optional caption - either text or a ggplot object created by
#'   [caption_block()].
#' @param narrative_position Position of narrative: "right", "left", "bottom".
#'   Default: "right".
#' @param narrative_width Width proportion for narrative (0-1). Default: 0.35.
#' @param title_size Size for title in pts. Default: 16 (large, prominent).
#' @param subtitle_size Size for subtitle in pts. Default: 11.
#' @param narrative_size Size for narrative text in pts. Default: 10.
#' @param caption_size Size for caption in pts. Default: 9.
#' @param title_height Height proportion for title row (0-1). When NULL and
#'   `auto_heights = TRUE`, this is calculated automatically based on text length.
#' @param subtitle_height Height proportion for subtitle row (0-1). When NULL and
#'   `auto_heights = TRUE`, this is calculated automatically.
#' @param caption_height Height proportion for caption row (0-1). When NULL and
#'   `auto_heights = TRUE`, this is calculated automatically.
#' @param auto_heights Logical. If TRUE (default), automatically calculates
#'   appropriate heights for title/subtitle/caption based on text length to
#'   prevent clipping. Set to FALSE to use fixed heights.
#' @param output_width Numeric. Expected output width in inches (default: 12).
#'   Used by auto_heights to estimate how many lines text will wrap to.
#'
#' @returns A patchwork object.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(marquee)
#'
#' p <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
#'     geom_col(fill = "#404040") +
#'     theme_minimal()
#'
#' # Basic usage with text
#' story_layout(
#'     plot = p,
#'     title = "**Fuel Efficiency by Cylinder Count**",
#'     subtitle = "{#E63946 4-cylinder} cars lead in MPG",
#'     narrative = "The data shows a clear trend:\n\n**Fewer cylinders = better mileage.**",
#'     caption = "Source: Motor Trend, 1974"
#' )
#'
#' # Full control with block functions
#' story_layout(
#'     plot = p,
#'     title = title_block(
#'         "**Fuel Efficiency by Cylinder Count**",
#'         title_size = 20
#'     ),
#'     subtitle = subtitle_block(
#'         "{#E63946 4-cylinder} cars lead in MPG",
#'         subtitle_size = 12
#'     ),
#'     narrative = text_narrative(
#'         "The data shows a clear trend:\n\n**Fewer cylinders = better mileage.**",
#'         size = 11
#'     )
#' )
#' }
#'
story_layout <- function(plot,
                         title = NULL,
                         subtitle = NULL,
                         narrative = NULL,
                         caption = NULL,
                         narrative_position = "right",
                         narrative_width = 0.35,
                         title_size = 12,
                         subtitle_size = 10,
                         narrative_size = 10,
                         caption_size = 6,
                         title_height = 0.10,
                         subtitle_height = NULL,
                         caption_height = NULL,
                         auto_heights = TRUE,
                         output_width = 12) {
    check_marquee("story_layout")

    # Auto-calculate heights if not provided
    if (auto_heights) {
        est <- estimate_layout_heights(
            title = title,
            subtitle = subtitle,
            caption = caption,
            title_size = title_size,
            subtitle_size = subtitle_size,
            output_width = output_width
        )
        # Use estimated heights unless explicitly provided
        if (is.null(title_height)) title_height <- est$title_height
        if (is.null(subtitle_height)) subtitle_height <- est$subtitle_height
        if (is.null(caption_height)) caption_height <- est$caption_height
    } else {
        # Use defaults when auto_heights = FALSE and no value provided
        if (is.null(title_height)) title_height <- 0.15
        if (is.null(subtitle_height)) subtitle_height <- 0.07
        if (is.null(caption_height)) caption_height <- 0.05
    }

    # Convert text to block plots if needed
    title_plot <- as_block(title, title_block, title_size = title_size)
    subtitle_plot <- as_block(subtitle, subtitle_block, subtitle_size = subtitle_size)
    narrative_plot <- as_block(narrative, text_narrative, size = narrative_size, valign = "top")
    caption_plot <- as_block(caption, caption_block, caption_size = caption_size)

    # Build the main content area (plot + optional narrative)
    content <- plot
    if (!is.null(narrative_plot)) {
        plot_width <- 1 - narrative_width

        if (narrative_position == "right") {
            content <- content + narrative_plot +
                plot_layout(widths = c(plot_width, narrative_width))
        } else if (narrative_position == "left") {
            content <- narrative_plot + content +
                plot_layout(widths = c(narrative_width, plot_width))
        } else if (narrative_position == "bottom") {
            content <- content / narrative_plot +
                plot_layout(heights = c(plot_width, narrative_width))
        }
    }

    # Calculate remaining height for content
    content_height <- 1
    heights <- c()
    components <- list()

    # Stack: title / subtitle / content / caption
    if (!is.null(title_plot)) {
        components <- c(components, list(title_plot))
        heights <- c(heights, title_height)
        content_height <- content_height - title_height
    }

    if (!is.null(subtitle_plot)) {
        components <- c(components, list(subtitle_plot))
        heights <- c(heights, subtitle_height)
        content_height <- content_height - subtitle_height
    }

    # Add content
    components <- c(components, list(content))

    if (!is.null(caption_plot)) {
        content_height <- content_height - caption_height
        heights <- c(heights, content_height, caption_height)
        components <- c(components, list(caption_plot))
    } else {
        heights <- c(heights, content_height)
    }

    # Combine all components vertically
    if (length(components) == 1) {
        components[[1]]
    } else {
        purrr::reduce(components, `/`) + plot_layout(heights = heights)
    }
}
