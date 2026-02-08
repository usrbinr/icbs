#' @import ggplot2
#' @import S7
#' @importFrom cli cli_abort cli_h1 cli_h2 cli_text cli_bullets cli_alert_warning
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom grid gpar linesGrob grobTree rectGrob unit
#' @importFrom rlang quo quo_get_expr %||%
#' @importFrom scales percent
#' @importFrom stats reorder setNames
#' @importFrom utils head tail
NULL

# ============================================================================
# S7 Classes
# ============================================================================

# Story Sitrep S7 Class (internal)
# Use story_sitrep() to create instances
StorySitrep <- new_class(
    name = "StorySitrep",
    properties = list(
        components = class_data.frame,
        layout = class_list,
        diagram = class_character,
        tips = class_character
    )
)

# Print method for StorySitrep
method(print, StorySitrep) <- function(x, ...) {
    cli_h1("Story Layout Sitrep")

    # Components table
    cli_h2("Components")
    present <- x@components[x@components$present, ]
    if (nrow(present) > 0) {
        for (i in seq_len(nrow(present))) {
            row <- present[i, ]
            size_info <- if (!is.na(row$size_pt)) paste0(" (", row$size_pt, "pt)") else ""
            type_info <- if (!is.na(row$type)) paste0(" [", row$type, "]") else ""
            cli_bullets(setNames(
                paste0("{.field ", row$component, "}", type_info, size_info),
                "*"
            ))
        }
    }

    absent <- x@components[!x@components$present, "component"]
    if (length(absent) > 0) {
        cli_text("{.emph Not included:} {.val {absent}}")
    }

    # Layout proportions
    cli_h2("Layout Proportions")
    ly <- x@layout
    cli_bullets(c(
        "*" = "Title height: {.val {percent(ly$title_height, accuracy = 1)}}",
        "*" = "Subtitle height: {.val {percent(ly$subtitle_height, accuracy = 1)}}",
        "*" = "Content height: {.val {percent(ly$content_height, accuracy = 1)}}",
        "*" = "Caption height: {.val {percent(ly$caption_height, accuracy = 1)}}"
    ))

    if (!is.na(ly$narrative_position)) {
        cli_bullets(c(
            "*" = "Narrative: {.val {ly$narrative_position}} ({.val {percent(ly$narrative_width, accuracy = 1)}} width)",
            "*" = "Plot width: {.val {percent(ly$plot_width, accuracy = 1)}}"
        ))
    }

    # ASCII diagram
    cli_h2("Layout Diagram")
    cat(x@diagram, sep = "\n")

    # Tips
    if (length(x@tips) > 0) {
        cli_h2("Tips")
        for (tip in x@tips) {
            cli::cli_alert_info(tip)
        }
    }

    invisible(x)
}

# Global variable declarations to avoid R CMD check notes
utils::globalVariables(c(
    ".actual", ".category", ".conn_x", ".conn_xend", ".conn_y", ".conn_yend",
    ".formatted_label", ".label_hjust", ".label_x", ".marquee_label",
    ".reference", ".scenario", ".type", ".value",
    ".variance_pct", ".variance_sign", ".x", ".y", ".ymax", ".ymin",
    "x", "xend", "y", "yend"
))

# ============================================================================
# Marquee Text Color Matching
# ============================================================================

#' Format Labels with Matching Colors
#'
#' Creates marquee-formatted text labels where the text color matches
#' a specified fill or color value. Use with `marquee::geom_marquee()`.
#'
#' @param labels Character vector of labels to format.
#' @param colors Character vector of colors (same length as labels).
#'   Can be hex codes (e.g., "#FF0000") or named colors (e.g., "red").
#' @param bold Logical, make text bold. Default: FALSE.
#' @param italic Logical, make text italic. Default: FALSE.
#'
#' @returns Character vector of marquee-formatted labels.
#' @export
#'
#' @examples
#' labels <- c("Positive", "Negative")
#' colors <- c("#006400", "#8B0000")
#' marquee_color_labels(labels, colors)
#' # Returns: c("{#006400 Positive}", "{#8B0000 Negative}")
#'
#' # With bold formatting:
#' marquee_color_labels(labels, colors, bold = TRUE)
#' # Returns: c("{#006400 **Positive**}", "{#8B0000 **Negative**}")
#'
marquee_color_labels <- function(labels, colors, bold = FALSE, italic = FALSE) {
    if (length(labels) != length(colors)) {
        stop("labels and colors must have the same length")
    }

    # Build format string
    formatted <- mapply(function(label, color) {
        text <- label
        if (bold && italic) {
            text <- paste0("***", text, "***")
        } else if (bold) {
            text <- paste0("**", text, "**")
        } else if (italic) {
            text <- paste0("*", text, "*")
        }
        paste0("{", color, " ", text, "}")
    }, labels, colors, USE.NAMES = FALSE)

    return(formatted)
}

#' Create a Variance Bar Chart with Colored Labels
#'
#' Creates a horizontal bar chart showing variances with color-coded labels
#' using marquee. Positive variances are shown in green, negative in red,
#' following IBCS standards.
#'
#' @param data A data frame containing the data.
#' @param category Column name for categories (unquoted).
#' @param value Column name for variance values (unquoted).
#' @param positive_color Color for positive values. Default: IBCS green.
#' @param negative_color Color for negative values. Default: IBCS red.
#' @param bar_color Fill color for bars. Default: IBCS gray.
#' @param label_size Size of the value labels. Default: 4.
#' @param bold Logical, make labels bold. Default: TRUE.
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(marquee)
#' df <- data.frame(
#'     product = c("Product A", "Product B", "Product C", "Product D"),
#'     variance = c(15, -8, 22, -3)
#' )
#' plot_variance_bars(df, category = product, value = variance)
#' }
#'
plot_variance_bars <- function(data,
                               category,
                               value,
                               positive_color = NULL,
                               negative_color = NULL,
                               bar_color = NULL,
                               label_size = 4,
                               bold = TRUE) {

    # Check if marquee is available
    if (!requireNamespace("marquee", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg marquee} is required for {.fn plot_variance_bars}.",
            "i" = "Install it with: {.code install.packages('marquee')}"
        )
    }

    cols <- ibcs_colors()

    # Set default colors
    if (is.null(positive_color)) positive_color <- cols$variance_positive
    if (is.null(negative_color)) negative_color <- cols$variance_negative
    if (is.null(bar_color)) bar_color <- cols$actual

    # Extract column names
    cat_col <- deparse(substitute(category))
    val_col <- deparse(substitute(value))

    # Prepare plot data
    plot_data <- data
    plot_data$.category <- factor(
        plot_data[[cat_col]],
        levels = rev(plot_data[[cat_col]])
    )
    plot_data$.value <- plot_data[[val_col]]

    # Determine colors based on sign
    plot_data$.label_color <- ifelse(
        plot_data$.value >= 0,
        positive_color,
        negative_color
    )

    # Format labels with marquee syntax
    plot_data$.formatted_label <- marquee_color_labels(
        labels = sprintf("%+.1f%%", plot_data$.value),
        colors = plot_data$.label_color,
        bold = bold
    )

    # Calculate label positions (outside the bars)
    plot_data$.label_x <- ifelse(
        plot_data$.value >= 0,
        plot_data$.value + max(abs(plot_data$.value)) * 0.05,
        plot_data$.value - max(abs(plot_data$.value)) * 0.05
    )
    plot_data$.label_hjust <- ifelse(plot_data$.value >= 0, 0, 1)

    # Create the plot
    p <- ggplot(plot_data, aes(x = .value, y = .category)) +
        geom_col(fill = bar_color, width = 0.7) +
        geom_vline(xintercept = 0, color = cols$actual, linewidth = 0.5) +
        marquee::geom_marquee(
            aes(x = .label_x, label = .formatted_label, hjust = .label_hjust),
            size = label_size
        ) +
        scale_x_continuous(
            expand = expansion(mult = c(0.15, 0.15)),
            labels = function(x) paste0(x, "%")
        ) +
        theme_ibcs(grid_x = TRUE, grid_y = FALSE) +
        labs(x = NULL, y = NULL)

    return(p)
}

# ============================================================================
# Polished Chart Helpers (SWD/IBCS Style)
# ============================================================================

#' Create a Title with Embedded Colored Category Names
#'
#' Builds a title string where category names are colored to match their
#' fill colors. Useful for creating self-documenting titles that replace
#' traditional legends.
#'
#' @param template A string with placeholders for categories. Use `{category_name}`
#'   syntax for each category you want colored.
#' @param colors Named vector of colors (names = category levels).
#' @param bold Logical, make category names bold. Default: TRUE.
#'
#' @returns A marquee-formatted string for use in `labs(title = ...)` or
#'   `labs(subtitle = ...)` with `theme(plot.title = element_marquee())`.
#' @export
#'
#' @examples
#' fill_colors <- c("Apples" = "#E63946", "Oranges" = "#F4A261")
#'
#' marquee_title(
#'     "{Apples} outsold {Oranges} in Q4",
#'     fill_colors
#' )
#' # Returns: "{#E63946 **Apples**} outsold {#F4A261 **Oranges**} in Q4"
#'
marquee_title <- function(template, colors, bold = TRUE) {
    result <- template

    for (name in names(colors)) {
        color <- colors[[name]]
        formatted <- if (bold) {
            paste0("{", color, " **", name, "**}")
        } else {
            paste0("{", color, " ", name, "}")
        }
        # Replace {name} with formatted version
        result <- gsub(paste0("\\{", name, "\\}"), formatted, result)
    }

    result
}

#' Create a Highlight Color Palette
#'
#' Creates a color vector where only specified categories are highlighted
#' and all others are grayed out. Follows the SWD principle of using
#' color strategically for emphasis.
#'
#' @param categories Character vector of all category names.
#' @param highlight Character vector of categories to highlight, or a named
#'   vector where names are categories and values are their highlight colors.
#' @param highlight_color Color for highlighted categories (used when
#'   `highlight` is unnamed). Default: IBCS highlight blue.
#' @param gray_color Color for non-highlighted categories. Default: light gray.
#'
#' @returns A named vector of colors.
#' @export
#'
#' @examples
#' categories <- c("A", "B", "C", "D", "E")
#'
#' # Highlight one category
#' highlight_colors(categories, "C")
#'
#' # Highlight multiple with custom colors
#' highlight_colors(categories, c("A" = "#E63946", "C" = "#2A9D8F"))
#'
highlight_colors <- function(categories,
                             highlight,
                             highlight_color = NULL,
                             gray_color = "#D3D3D3") {

    if (is.null(highlight_color)) {
        highlight_color <- ibcs_colors()$highlight
    }

    # Build color vector
    cols <- setNames(rep(gray_color, length(categories)), categories)

    if (is.null(names(highlight))) {
        # Unnamed: use single highlight color
        cols[highlight] <- highlight_color
    } else {
        # Named: use specified colors
        cols[names(highlight)] <- highlight
    }

    cols
}

#' Create an Inline Legend Annotation
#'
#' Creates a data frame suitable for adding colored text annotations
#' as an inline legend replacement. Use with `geom_marquee()`.
#'
#' @param colors Named vector of colors (names = category labels).
#' @param x X position for the legend. Default: 0.
#' @param y Y position for the legend. Default: 0.
#' @param orientation Either "horizontal" or "vertical". Default: "horizontal".
#' @param sep Separator between items for horizontal layout. Default: bullet.
#' @param bold Make text bold. Default: TRUE.
#'
#' @returns A data frame with columns `x`, `y`, and `label` (marquee-formatted).
#' @export
#'
#' @examples
#' \dontrun{
#' fill_colors <- c("Apples" = "#E63946", "Oranges" = "#F4A261")
#'
#' # Add as annotation layer
#' ggplot(...) +
#'     geom_marquee(
#'         data = inline_legend(fill_colors, x = 1, y = 100),
#'         aes(x = x, y = y, label = label),
#'         hjust = 0
#'     )
#' }
#'
inline_legend <- function(colors,
                          x = 0,
                          y = 0,
                          orientation = "horizontal",
                          sep = " \u2022 ",
                          bold = TRUE) {
    if (orientation == "horizontal") {
        # Single row with all items
        formatted <- marquee_color_labels(
            labels = names(colors),
            colors = unname(colors),
            bold = bold
        )
        label <- paste(formatted, collapse = sep)
        data.frame(x = x, y = y, label = label)
    } else {
        # Separate row for each item
        formatted <- marquee_color_labels(
            labels = names(colors),
            colors = unname(colors),
            bold = bold
        )
        n <- length(colors)
        data.frame(
            x = rep(x, n),
            y = seq(y, by = -1, length.out = n),
            label = formatted
        )
    }
}

#' Apply Marquee Styling to Common Text Elements
#'
#' A theme addition that enables marquee rendering for title, subtitle,
#' caption, and legend text. Use after your base theme.
#'
#' @returns A ggplot2 theme object.
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(...) +
#'     theme_ibcs() +
#'     theme_marquee()
#' }
#'
theme_marquee <- function() {
    if (!requireNamespace("marquee", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg marquee} is required for {.fn theme_marquee}."
        )
    }

    theme(
        plot.title = marquee::element_marquee(),
        plot.subtitle = marquee::element_marquee(),
        plot.caption = marquee::element_marquee(),
        legend.text = marquee::element_marquee()
    )
}

# ============================================================================
# Story Layout Framework (SWD-style compositions)
# ============================================================================

#' Create a Text Narrative Block
#'
#' Creates a ggplot object containing formatted text, suitable for
#' combining with charts via patchwork. Use for adding story context,
#' callouts, or explanatory text alongside visualizations.
#'
#' @param text Character string with marquee-formatted text. Supports
#'   markdown: `**bold**`, `*italic*`, `{#color text}`.
#' @param size Text size. Default: 4.
#' @param halign Horizontal alignment: "left", "center", or "right". Default: "left".
#' @param valign Vertical alignment: "top", "center", or "bottom". Default: "top".
#' @param padding Padding around text as a margin() object or numeric vector
#'   (top, right, bottom, left). Default: 10 on all sides.
#' @param width Maximum width for text wrapping. Default: 1 (full width).
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(marquee)
#' library(patchwork)
#'
#' text_block <- text_narrative(
#'     "**Key insight:** Sales in {#E63946 Q4} exceeded
#'     expectations by 15%, driven primarily by
#'     the {#2A9D8F new product line}.",
#'     size = 4
#' )
#'
#' my_chart + text_block + plot_layout(widths = c(0.6, 0.4))
#' }
#'
text_narrative <- function(text,
                           size = 4,
                           halign = "left",
                           valign = "top",
                           padding = 10,
                           width = 1) {

    if (!requireNamespace("marquee", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg marquee} is required for {.fn text_narrative}."
        )
    }

    # Convert alignment to numeric
    hjust <- switch(halign, left = 0, center = 0.5, right = 1, 0)
    vjust <- switch(valign, top = 1, center = 0.5, bottom = 0, 1)

    # X position based on alignment
    x_pos <- switch(halign, left = 0, center = 0.5, right = 1, 0)
    y_pos <- switch(valign, top = 1, center = 0.5, bottom = 0, 1)

    p <- ggplot() +
        marquee::geom_marquee(
            aes(x = x_pos, y = y_pos, label = text),
            hjust = hjust,
            vjust = vjust,
            size = size,
            width = width
        ) +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        theme_void() +
        theme(plot.margin = margin(padding, padding, padding, padding))

    p
}

#' Create a Title Block Plot
#'
#' Creates a standalone title element as a ggplot, for use as the
#' top element in a patchwork composition. Allows for larger, more
#' prominent titles than standard ggplot titles, matching the SWD style
#' where titles are bold statements that draw the eye.
#'
#' @param title Main title text (marquee-formatted). Supports markdown
#'   formatting: `**bold**`, `*italic*`, `{#color text}`.
#' @param title_size Size for title in pts. Default: 16 (matches SWD style).
#' @param halign Horizontal alignment: "left", "center", or "right". Default: "left".
#' @param lineheight Line height multiplier. Default: 1.1.
#' @param margin_top Top margin in pts. Default: 10.
#' @param margin_bottom Bottom margin in pts. Default: 15.
#' @param margin_left Left margin in pts. Default: 5.
#' @param margin_right Right margin in pts. Default: 5.
#' @param width Maximum width for text wrapping (0-1). Default: 0.95.
#'
#' @returns A ggplot object that can be combined with other plots via patchwork.
#' @export
#'
#' @examples
#' \dontrun{
#' library(patchwork)
#'
#' title_plot <- title_block(
#'     "**Q4 Sales: {#E63946 Apples} Take the Lead**",
#'     title_size = 18
#' )
#'
#' title_plot / my_chart + plot_layout(heights = c(0.08, 0.92))
#' }
#'
title_block <- function(title,
                        title_size = 16,
                        halign = "left",
                        lineheight = 1.1,
                        margin_top = 5,
                        margin_bottom = 5,
                        margin_left = 5,
                        margin_right = 5,
                        width = 0.95) {

    if (!requireNamespace("marquee", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg marquee} is required for {.fn title_block}."
        )
    }

    hjust <- switch(halign, left = 0, center = 0.5, right = 1, 0)
    # Offset from edge to avoid clipping
    x_pos <- switch(halign, left = 0.02, center = 0.5, right = 0.98, 0.02)

    # Position text from top (y=0.95) flowing downward (vjust=1)
    # This gives maximum room for wrapped text to expand downward
    # The y-axis expansion adds buffer at the bottom
    p <- ggplot() +
        marquee::geom_marquee(
            aes(x = x_pos, y = 0.95, label = title),
            hjust = hjust,
            vjust = 1,
            size = title_size,
            lineheight = lineheight,
            width = width
        ) +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.1, 0.02))) +
        theme_void() +
        theme(plot.margin = margin(margin_top, margin_right, margin_bottom, margin_left))

    p
}

#' Create a Subtitle Block Plot
#'
#' Creates a standalone subtitle element as a ggplot, for use below
#' a title block in a patchwork composition. Subtitles provide context
#' and supporting information for the main title.
#'
#' @param subtitle Subtitle text (marquee-formatted). Supports markdown
#'   formatting: `**bold**`, `*italic*`, `{#color text}`.
#' @param subtitle_size Size for subtitle in pts. Default: 11 (matches SWD style).
#' @param halign Horizontal alignment: "left", "center", or "right". Default: "left".
#' @param lineheight Line height multiplier. Default: 1.2.
#' @param margin_top Top margin in pts. Default: 0.
#' @param margin_bottom Bottom margin in pts. Default: 10.
#' @param margin_left Left margin in pts. Default: 5.
#' @param margin_right Right margin in pts. Default: 5.
#' @param width Maximum width for text wrapping (0-1). Default: 0.95.
#'
#' @returns A ggplot object that can be combined with other plots via patchwork.
#' @export
#'
#' @examples
#' \dontrun{
#' library(patchwork)
#'
#' subtitle_plot <- subtitle_block(
#'     "Fruit category performance, October-December 2024"
#' )
#'
#' title_plot / subtitle_plot / my_chart +
#'     plot_layout(heights = c(0.08, 0.05, 0.87))
#' }
#'
subtitle_block <- function(subtitle,
                           subtitle_size = 11,
                           halign = "left",
                           lineheight = 1.2,
                           margin_top = 0,
                           margin_bottom = 5,
                           margin_left = 5,
                           margin_right = 5,
                           width = 0.95) {

    if (!requireNamespace("marquee", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg marquee} is required for {.fn subtitle_block}."
        )
    }

    hjust <- switch(halign, left = 0, center = 0.5, right = 1, 0)
    x_pos <- switch(halign, left = 0.02, center = 0.5, right = 0.98, 0.02)

    # Position from top (y=0.9), flowing downward
    p <- ggplot() +
        marquee::geom_marquee(
            aes(x = x_pos, y = 0.9, label = subtitle),
            hjust = hjust,
            vjust = 1,
            size = subtitle_size,
            lineheight = lineheight,
            width = width
        ) +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.1, 0.02))) +
        theme_void() +
        theme(plot.margin = margin(margin_top, margin_right, margin_bottom, margin_left))

    p
}

#' Create a Caption Block Plot
#'
#' Creates a standalone caption element as a ggplot, for use at the
#' bottom of a patchwork composition. Captions typically contain source
#' attributions and notes.
#'
#' @param caption Caption text (marquee-formatted).
#' @param caption_size Size for caption in pts. Default: 9.
#' @param halign Horizontal alignment. Default: "left".
#' @param color Text color. Default: gray (#808080).
#' @param margin_top Top margin in pts. Default: 10.
#' @param margin_bottom Bottom margin in pts. Default: 5.
#' @param margin_left Left margin in pts. Default: 5.
#' @param margin_right Right margin in pts. Default: 5.
#'
#' @returns A ggplot object that can be combined with other plots via patchwork.
#' @export
#'
#' @examples
#' \dontrun{
#' library(patchwork)
#'
#' caption_plot <- caption_block("Source: Internal Sales Database")
#'
#' my_chart / caption_plot + plot_layout(heights = c(0.95, 0.05))
#' }
#'
caption_block <- function(caption,
                          caption_size = 9,
                          halign = "left",
                          color = "#808080",
                          margin_top = 10,
                          margin_bottom = 5,
                          margin_left = 5,
                          margin_right = 5) {

    if (!requireNamespace("marquee", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg marquee} is required for {.fn caption_block}."
        )
    }

    hjust <- switch(halign, left = 0, center = 0.5, right = 1, 0)
    x_pos <- switch(halign, left = 0.02, center = 0.5, right = 0.98, 0.02)

    # Wrap caption in color if not already formatted
    if (!grepl("^\\{#", caption)) {
        caption <- paste0("{", color, " ", caption, "}")
    }

    p <- ggplot() +
        marquee::geom_marquee(
            aes(x = x_pos, y = 0.5, label = caption),
            hjust = hjust,
            vjust = 0.5,
            size = caption_size
        ) +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        theme_void() +
        theme(plot.margin = margin(margin_top, margin_right, margin_bottom, margin_left))

    p
}

#' Create a Large Title Block (Deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [title_block()] and [subtitle_block()] instead for more control.
#'
#' @param title Main title text (marquee-formatted).
#' @param subtitle Optional subtitle text (marquee-formatted).
#' @param title_size Size for title. Default: 8.
#' @param subtitle_size Size for subtitle. Default: 4.
#' @param halign Horizontal alignment. Default: "left".
#'
#' @returns A ggplot object.
#' @export
#' @keywords internal
#'
title_header <- function(title,
                         subtitle = NULL,
                         title_size = 8,
                         subtitle_size = 4,
                         halign = "left") {

    if (!requireNamespace("marquee", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg marquee} is required for {.fn title_header}."
        )
    }

    hjust <- switch(halign, left = 0, center = 0.5, right = 1, 0)
    x_pos <- switch(halign, left = 0, center = 0.5, right = 1, 0)

    p <- ggplot() +
        marquee::geom_marquee(
            aes(x = x_pos, y = 0.6, label = title),
            hjust = hjust,
            vjust = 0,
            size = title_size
        )

    if (!is.null(subtitle)) {
        p <- p +
            marquee::geom_marquee(
                aes(x = x_pos, y = 0.4, label = subtitle),
                hjust = hjust,
                vjust = 1,
                size = subtitle_size
            )
    }

    p <- p +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        theme_void() +
        theme(plot.margin = margin(5, 10, 5, 10))

    p
}

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
                                     title_size = 16,
                                     subtitle_size = 11,
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
        # Remove marquee formatting for character count
        clean_title <- gsub("\\{#[A-Fa-f0-9]+ ([^}]+)\\}", "\\1", title)
        clean_title <- gsub("\\*+", "", clean_title)

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
        title_height <- max(0.12, est_lines * height_per_line + 0.04)
        # Cap at reasonable maximum - very long titles should be shortened
        title_height <- min(title_height, 0.40)
    }

    if (!is.null(subtitle) && is.character(subtitle)) {
        clean_subtitle <- gsub("\\{#[A-Fa-f0-9]+ ([^}]+)\\}", "\\1", subtitle)
        clean_subtitle <- gsub("\\*+", "", clean_subtitle)

        # At 11pt, ~40-50 chars per line at typical width
        base_chars_per_line <- 45
        chars_per_line <- max(25, base_chars_per_line - (subtitle_size - 10))
        chars_per_line <- floor(chars_per_line * (output_width / 12))

        # Word wrapping adds overhead
        est_lines <- ceiling(nchar(clean_subtitle) / max(chars_per_line, 25) * 1.2)
        height_per_line <- 0.035 + (subtitle_size / 400)
        subtitle_height <- max(0.06, est_lines * height_per_line + 0.02)
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

#' Get a Situation Report for a Story Layout
#'
#' Returns diagnostic information about a story layout composition,
#' including which components are present, their sizes, layout proportions,
#' and an ASCII diagram of the structure.
#'
#' @param plot The main ggplot visualization (or NULL to just see layout structure).
#' @param title Optional title (text or ggplot).
#' @param subtitle Optional subtitle (text or ggplot).
#' @param narrative Optional narrative (text or ggplot).
#' @param caption Optional caption (text or ggplot).
#' @param narrative_position Position of narrative: "right", "left", "bottom".
#' @param narrative_width Width proportion for narrative (0-1).
#' @param title_size Size for title in pts.
#' @param subtitle_size Size for subtitle in pts.
#' @param narrative_size Size for narrative text in pts.
#' @param caption_size Size for caption in pts.
#' @param title_height Height proportion for title row.
#' @param subtitle_height Height proportion for subtitle row.
#' @param caption_height Height proportion for caption row.
#' @param show_tips Logical. Show SWD-inspired tips? Default: TRUE.
#'
#' @returns A `StorySitrep` S7 object with properties:
#'   - `@components`: data.frame of component details
#'   - `@layout`: list of layout proportions
#'   - `@diagram`: ASCII representation of the layout
#'   - `@tips`: character vector of SWD-inspired suggestions
#'
#' @export
#'
#' @examples
#' # Check layout before building
#' story_sitrep(
#'     title = "My Title",
#'     subtitle = "My Subtitle",
#'     narrative = "Some text",
#'     narrative_position = "right"
#' )
#'
story_sitrep <- function(plot = NULL,
                         title = NULL,
                         subtitle = NULL,
                         narrative = NULL,
                         caption = NULL,
                         narrative_position = "right",
                         narrative_width = 0.35,
                         title_size = 16,
                         subtitle_size = 11,
                         narrative_size = 10,
                         caption_size = 9,
                         title_height = 0.15,
                         subtitle_height = 0.07,
                         caption_height = 0.05,
                         show_tips = TRUE) {

    # Determine which components are present
    has_title <- !is.null(title)
    has_subtitle <- !is.null(subtitle)
    has_narrative <- !is.null(narrative)
    has_caption <- !is.null(caption)
    has_plot <- !is.null(plot)

    # Build components data frame
    components <- data.frame(
        component = c("title", "subtitle", "plot", "narrative", "caption"),
        present = c(has_title, has_subtitle, has_plot, has_narrative, has_caption),
        type = c(
            if (has_title) if (inherits(title, "gg")) "ggplot" else "text" else NA_character_,
            if (has_subtitle) if (inherits(subtitle, "gg")) "ggplot" else "text" else NA_character_,
            if (has_plot) "ggplot" else NA_character_,
            if (has_narrative) if (inherits(narrative, "gg")) "ggplot" else "text" else NA_character_,
            if (has_caption) if (inherits(caption, "gg")) "ggplot" else "text" else NA_character_
        ),
        size_pt = c(
            if (has_title) title_size else NA_real_,
            if (has_subtitle) subtitle_size else NA_real_,
            NA_real_,
            if (has_narrative) narrative_size else NA_real_,
            if (has_caption) caption_size else NA_real_
        ),
        stringsAsFactors = FALSE
    )

    # Calculate actual heights
    content_height <- 1
    if (has_title) content_height <- content_height - title_height
    if (has_subtitle) content_height <- content_height - subtitle_height
    if (has_caption) content_height <- content_height - caption_height

    # Layout info
    layout <- list(
        title_height = if (has_title) title_height else 0,
        subtitle_height = if (has_subtitle) subtitle_height else 0,
        content_height = content_height,
        caption_height = if (has_caption) caption_height else 0,
        narrative_position = if (has_narrative) narrative_position else NA_character_,
        narrative_width = if (has_narrative) narrative_width else 0,
        plot_width = if (has_narrative) 1 - narrative_width else 1
    )

    # Build ASCII diagram
    diagram <- build_story_diagram(
        has_title, has_subtitle, has_plot, has_narrative, has_caption,
        narrative_position, layout
    )

    # Generate tips (SWD-inspired suggestions)
    tips <- character(0)

    if (show_tips) {
        # Title tips
        if (!has_title) {
            tips <- c(tips, "Consider adding a title that states your key insight, not just a description")
        }
        if (has_title && title_size < 14) {
            tips <- c(tips, "Larger titles (14pt+) create visual hierarchy and draw attention to your message")
        }
        if (has_title && is.character(title) && nchar(title) > 80) {
            tips <- c(tips, "Long titles can overwhelm; consider moving details to the subtitle")
        }

        # Subtitle tips
        if (has_title && !has_subtitle) {
            tips <- c(tips, "A subtitle can provide supporting context (time period, methodology, scope)")
        }
        if (has_title && has_subtitle && (title_size - subtitle_size) < 4) {
            tips <- c(tips, "Create visual hierarchy: title should be noticeably larger than subtitle")
        }

        # Color usage tips (detect marquee color syntax)
        if (has_title && is.character(title) && grepl("\\{#[A-Fa-f0-9]+", title)) {
            tips <- c(tips, "Colored text in title should match corresponding chart elements for consistency")
        }

        # Narrative tips
        if (!has_narrative && has_plot) {
            tips <- c(tips, "A narrative panel can explain the 'so what?' and guide your audience")
        }
        if (has_narrative && narrative_width > 0.5) {
            tips <- c(tips, "Wide narratives (>50%) may overshadow the visualization")
        }
        if (has_narrative && narrative_width < 0.2) {
            tips <- c(tips, "Narrow narratives (<20%) can be hard to read; consider more width")
        }
        if (has_narrative && is.character(narrative) && !grepl("\\*\\*", narrative)) {
            tips <- c(tips, "Use **bold** in narrative to emphasize key takeaways and action items")
        }

        # Layout tips
        if (content_height < 0.5) {
            tips <- c(tips, "Headers/footers using >50% of space; ensure the chart remains the focus")
        }

        # Caption/source tip
        if (!has_caption && has_plot) {
            tips <- c(tips, "Including a source caption builds credibility with your audience")
        }

        # General reminders
        if (has_plot && !has_title && !has_narrative) {
            tips <- c(tips, "Charts alone require audiences to infer meaning; add context to guide interpretation")
        }

        # Long title tip (SWD best practice: keep titles concise)
        if (has_title && is.character(title)) {
            clean_title <- gsub("\\{#[A-Fa-f0-9]+ ([^}]+)\\}", "\\1", title)
            clean_title <- gsub("\\*+", "", clean_title)
            if (nchar(clean_title) > 60) {
                tips <- c(tips, "Long titles (>60 chars) may wrap to many lines; consider a shorter, punchier title")
            }
        }

        # Auto-heights suggestion
        if (has_title || has_subtitle) {
            tips <- c(tips, "Use auto_heights=TRUE in story_layout() to automatically size title/subtitle")
        }
    }

    # Build result as S7 object
    StorySitrep(
        components = components,
        layout = layout,
        diagram = diagram,
        tips = tips
    )
}

# Helper to build ASCII diagram
build_story_diagram <- function(has_title, has_subtitle, has_plot, has_narrative,
                                 has_caption, narrative_position, layout) {

    width <- 50
    lines <- character(0)

    border_top <- paste0("+", strrep("-", width - 2), "+")
    border_bot <- border_top

    lines <- c(lines, border_top)

    # Title row
    if (has_title) {
        pct <- percent(layout$title_height, accuracy = 1)
        label <- paste0("TITLE (", pct, ")")
        pad <- width - 4 - nchar(label)
        lines <- c(lines, paste0("| ", label, strrep(" ", pad), " |"))
        lines <- c(lines, paste0("|", strrep("-", width - 2), "|"))
    }

    # Subtitle row
    if (has_subtitle) {
        pct <- percent(layout$subtitle_height, accuracy = 1)
        label <- paste0("SUBTITLE (", pct, ")")
        pad <- width - 4 - nchar(label)
        lines <- c(lines, paste0("| ", label, strrep(" ", pad), " |"))
        lines <- c(lines, paste0("|", strrep("-", width - 2), "|"))
    }

    # Content area (plot + narrative)
    pct <- percent(layout$content_height, accuracy = 1)
    if (has_narrative && has_plot) {
        # Calculate widths based on proportions
        inner_width <- width - 4  # space inside borders
        plot_chars <- round(inner_width * layout$plot_width)
        narr_chars <- inner_width - plot_chars - 1  # -1 for divider

        if (narrative_position == "right") {
            # PLOT on left, NARRATIVE on right
            left_label <- "PLOT"
            right_label <- "NARRATIVE"
            left_w <- plot_chars
            right_w <- narr_chars
        } else if (narrative_position == "left") {
            # NARRATIVE on left, PLOT on right
            left_label <- "NARRATIVE"
            right_label <- "PLOT"
            left_w <- narr_chars
            right_w <- plot_chars
        } else {
            # bottom - show stacked
            lines <- c(lines, paste0("| PLOT", strrep(" ", width - 8), " |"))
            lines <- c(lines, paste0("| ", strrep(" ", width - 4), " |"))
            lines <- c(lines, paste0("|", strrep("-", width - 2), "|"))
            narr_pct <- percent(layout$narrative_width, accuracy = 1)
            lines <- c(lines, paste0("| NARRATIVE (", narr_pct, " height)", strrep(" ", width - 24), " |"))
            lines <- c(lines, paste0("|", strrep("-", width - 2), "|"))
            if (has_caption) {
                pct_cap <- percent(layout$caption_height, accuracy = 1)
                label <- paste0("CAPTION (", pct_cap, ")")
                pad <- width - 4 - nchar(label)
                lines <- c(lines, paste0("| ", label, strrep(" ", pad), " |"))
            }
            lines <- c(lines, border_bot)
            return(lines)
        }

        # Side by side layout
        # Row 1: Labels
        left_pad <- max(0, left_w - nchar(left_label))
        right_pad <- max(0, right_w - nchar(right_label))
        row1 <- paste0("| ", left_label, strrep(" ", left_pad),
                       "|", right_label, strrep(" ", right_pad), " |")

        # Row 2: Widths info
        left_info <- percent(layout$plot_width, accuracy = 1)
        right_info <- percent(layout$narrative_width, accuracy = 1)
        if (narrative_position == "left") {
            tmp <- left_info
            left_info <- right_info
            right_info <- tmp
        }
        left_info_pad <- max(0, left_w - nchar(left_info))
        right_info_pad <- max(0, right_w - nchar(right_info))
        row2 <- paste0("| ", left_info, strrep(" ", left_info_pad),
                       "|", right_info, strrep(" ", right_info_pad), " |")

        # Row 3: Empty
        row3 <- paste0("| ", strrep(" ", left_w), "|", strrep(" ", right_w), " |")

        lines <- c(lines, row1, row2, row3)
        lines <- c(lines, paste0("|", strrep("-", width - 2), "|"))
    } else if (has_plot) {
        lines <- c(lines, paste0("| PLOT", strrep(" ", width - 8), " |"))
        lines <- c(lines, paste0("| ", strrep(" ", width - 4), " |"))
        lines <- c(lines, paste0("| (", pct, ")", strrep(" ", width - 9), " |"))
        lines <- c(lines, paste0("|", strrep("-", width - 2), "|"))
    } else if (has_narrative) {
        lines <- c(lines, paste0("| NARRATIVE", strrep(" ", width - 13), " |"))
        lines <- c(lines, paste0("| (", pct, ")", strrep(" ", width - 9), " |"))
        lines <- c(lines, paste0("|", strrep("-", width - 2), "|"))
    }

    # Caption row
    if (has_caption) {
        pct <- percent(layout$caption_height, accuracy = 1)
        label <- paste0("CAPTION (", pct, ")")
        pad <- width - 4 - nchar(label)
        lines <- c(lines, paste0("| ", label, strrep(" ", pad), " |"))
    }

    lines <- c(lines, border_bot)

    lines
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
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(marquee)
#'
#' p <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
#'     geom_col(fill = "#404040") +
#'     theme_ibcs()
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
                         title_size = 16,
                         subtitle_size = 11,
                         narrative_size = 10,
                         caption_size = 9,
                         title_height = NULL,
                         subtitle_height = NULL,
                         caption_height = NULL,
                         auto_heights = TRUE,
                         output_width = 12) {

    if (!requireNamespace("marquee", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg marquee} is required for {.fn story_layout}."
        )
    }

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
    title_plot <- NULL
    if (!is.null(title)) {
        if (inherits(title, "gg")) {
            title_plot <- title
        } else {
            title_plot <- title_block(title, title_size = title_size)
        }
    }

    subtitle_plot <- NULL
    if (!is.null(subtitle)) {
        if (inherits(subtitle, "gg")) {
            subtitle_plot <- subtitle
        } else {
            subtitle_plot <- subtitle_block(subtitle, subtitle_size = subtitle_size)
        }
    }

    narrative_plot <- NULL
    if (!is.null(narrative)) {
        if (inherits(narrative, "gg")) {
            narrative_plot <- narrative
        } else {
            narrative_plot <- text_narrative(
                text = narrative,
                size = narrative_size,
                valign = "top"
            )
        }
    }

    caption_plot <- NULL
    if (!is.null(caption)) {
        if (inherits(caption, "gg")) {
            caption_plot <- caption
        } else {
            caption_plot <- caption_block(caption, caption_size = caption_size)
        }
    }

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
        result <- components[[1]]
    } else {
        result <- components[[1]]
        for (i in 2:length(components)) {
            result <- result / components[[i]]
        }
        result <- result + plot_layout(heights = heights)
    }

    result
}

#' Create Marquee-Formatted Labels from a Color Vector
#'
#' Takes a named color vector and a vector of values, returning
#' marquee-formatted labels where each value's text color matches
#' its corresponding fill color. Useful for labeling bars with
#' color-matched text.
#'
#' @param x Character vector of values to format (must match names in `colors`).
#' @param colors Named vector of colors (names = category levels).
#' @param bold Logical, make text bold. Default: FALSE.
#' @param italic Logical, make text italic. Default: FALSE.
#'
#' @returns Character vector of marquee-formatted labels.
#' @export
#'
#' @examples
#' fill_colors <- c(
#'     "Apples" = "#E63946",
#'     "Oranges" = "#F4A261",
#'     "Bananas" = "#E9C46A"
#' )
#'
#' marquee_labels_from_colors(c("Apples", "Oranges"), fill_colors, bold = TRUE)
#' # Returns: c("{#E63946 **Apples**}", "{#F4A261 **Oranges**}")
#'
marquee_labels_from_colors <- function(x, colors, bold = FALSE, italic = FALSE) {
    # Look up color for each value
    matched_colors <- colors[as.character(x)]

    marquee_color_labels(
        labels = as.character(x),
        colors = unname(matched_colors),
        bold = bold,
        italic = italic
    )
}

#' Create a Fill Scale with Color-Matched Legend Text
#'
#' Creates a `scale_fill_manual` where the legend text color matches
#' the fill color of each category. Uses marquee for text rendering.
#'
#' @param values Named vector of fill colors (names = category levels).
#' @param bold Logical, make legend text bold. Default: FALSE.
#' @param italic Logical, make legend text italic. Default: FALSE.
#' @param ... Additional arguments passed to `scale_fill_manual()`.
#'
#' @returns A ggplot2 scale object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(marquee)
#'
#' df <- data.frame(
#'     product = c("Apples", "Oranges", "Bananas", "Grapes"),
#'     sales = c(120, 85, 95, 110)
#' )
#'
#' fill_colors <- c(
#'     "Apples" = "#E63946",
#'     "Oranges" = "#F4A261",
#'     "Bananas" = "#E9C46A",
#'     "Grapes" = "#7B2D8E"
#' )
#'
#' ggplot(df, aes(x = product, y = sales, fill = product)) +
#'     geom_col() +
#'     scale_fill_marquee(fill_colors, bold = TRUE) +
#'     theme_ibcs() +
#'     theme(legend.text = marquee::element_marquee())
#' }
#'
scale_fill_marquee <- function(values, bold = FALSE, italic = FALSE, ...) {

    # Create marquee-formatted labels
    formatted_labels <- marquee_color_labels(
        labels = names(values),
        colors = unname(values),
        bold = bold,
        italic = italic
    )
    names(formatted_labels) <- names(values)

    scale_fill_manual(
        values = values,
        labels = formatted_labels,
        ...
    )
}

#' Create a Color Scale with Color-Matched Legend Text
#'
#' Creates a `scale_color_manual` where the legend text color matches
#' the line/point color of each category. Uses marquee for text rendering.
#'
#' @param values Named vector of colors (names = category levels).
#' @param bold Logical, make legend text bold. Default: FALSE.
#' @param italic Logical, make legend text italic. Default: FALSE.
#' @param ... Additional arguments passed to `scale_color_manual()`.
#'
#' @returns A ggplot2 scale object.
#' @export
#'
scale_color_marquee <- function(values, bold = FALSE, italic = FALSE, ...) {

    # Create marquee-formatted labels
    formatted_labels <- marquee_color_labels(
        labels = names(values),
        colors = unname(values),
        bold = bold,
        italic = italic
    )
    names(formatted_labels) <- names(values)

    scale_color_manual(
        values = values,
        labels = formatted_labels,
        ...
    )
}

#' Create a Color-Matched Label Layer
#'
#' A convenience function that adds colored text labels using marquee.
#' Pre-formats labels with marquee color syntax before plotting.
#'
#' @param data Data frame containing the data.
#' @param mapping Aesthetic mapping created by `aes()`. Must include `x`, `y`,
#'   `label`, and `color` (the color to use for the text).
#' @param ... Additional arguments passed to `marquee::geom_marquee()`.
#' @param bold Make text bold. Default: FALSE.
#' @param italic Make text italic. Default: FALSE.
#'
#' @returns A ggplot2 layer object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(marquee)
#' df <- data.frame(
#'     x = c("A", "B", "C"),
#'     y = c(10, -5, 8),
#'     text_color = c("#006400", "#8B0000", "#006400"),
#'     label = c("+10", "-5", "+8")
#' )
#' ggplot(df, aes(x = x, y = y)) +
#'     geom_col(aes(fill = text_color)) +
#'     geom_marquee_colored(
#'         data = df,
#'         mapping = aes(x = x, y = y, label = label, color = text_color),
#'         vjust = -0.5
#'     )
#' }
#'
geom_marquee_colored <- function(data,
                                  mapping,
                                  ...,
                                  bold = FALSE,
                                  italic = FALSE) {

    # Check if marquee is available
    if (!requireNamespace("marquee", quietly = TRUE)) {
        stop("Package 'marquee' is required for geom_marquee_colored(). ",
             "Install it with: install.packages('marquee')")
    }

    # Extract column names from mapping
    mapping_vars <- lapply(mapping, rlang::quo_get_expr)

    label_col <- as.character(mapping_vars$label)
    color_col <- as.character(mapping_vars$color %||% mapping_vars$colour)

    if (length(label_col) == 0) {
        stop("mapping must include 'label' aesthetic")
    }
    if (length(color_col) == 0) {
        stop("mapping must include 'color' or 'colour' aesthetic for text color")
    }

    # Create formatted labels
    formatted_data <- data
    formatted_data$.marquee_label <- marquee_color_labels(
        labels = as.character(data[[label_col]]),
        colors = as.character(data[[color_col]]),
        bold = bold,
        italic = italic
    )

    # Create new mapping without color (it's now embedded in label)
    new_mapping <- mapping
    new_mapping$label <- rlang::quo(.marquee_label)
    new_mapping$color <- NULL
    new_mapping$colour <- NULL

    marquee::geom_marquee(
        data = formatted_data,
        mapping = new_mapping,
        ...
    )
}

# ============================================================================
# IBCS Color Constants
# ============================================================================

#' IBCS Standard Colors
#'
#' Returns the standard IBCS color palette. IBCS recommends minimal use of
#' color, with red and green reserved exclusively for variances.
#'
#' @returns A named list of IBCS standard colors
#' @export
#'
#' @examples
#' cols <- ibcs_colors()
#' cols$actual
#' cols$variance_positive
#'
ibcs_colors <- function() {
    list(
        actual = "#404040",
        actual_light = "#808080",
        previous = "#A0A0A0",
        plan = "#404040",
        forecast = "#404040",
        variance_positive = "#006400",
        variance_negative = "#8B0000",
        highlight = "#1E90FF",
        background = "#FFFFFF",
        grid = "#E0E0E0",
        text = "#262626"
    )
}

# ============================================================================
# IBCS Theme
# ============================================================================

#' IBCS Theme for ggplot2
#'
#' A clean, minimal theme following IBCS standards. Removes chartjunk,
#' uses appropriate gridlines, and follows the SIMPLIFY principle.
#'
#' @param base_size Base font size. Default: 11.
#' @param base_family Base font family. Default: "".
#' @param grid_x Show vertical gridlines. Default: FALSE.
#' @param grid_y Show horizontal gridlines. Default: TRUE.
#'
#' @returns A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
#'     geom_col() +
#'     theme_ibcs()
#'
theme_ibcs <- function(base_size = 11,
                       base_family = "",
                       grid_x = FALSE,
                       grid_y = TRUE) {

    cols <- ibcs_colors()

    theme_minimal(base_size = base_size, base_family = base_family) %+replace%
        theme(
            # Text elements
            text = element_text(color = cols$text),
            plot.title = element_text(
                face = "bold",
                size = rel(1.2),
                hjust = 0,
                margin = margin(b = 10)
            ),
            plot.subtitle = element_text(
                size = rel(0.9),
                hjust = 0,
                margin = margin(b = 10)
            ),
            plot.caption = element_text(
                size = rel(0.8),
                hjust = 1,
                color = cols$previous
            ),

            # Axis elements
            axis.title = element_text(size = rel(0.9), face = "bold"),
            axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 10), angle = 90),
            axis.text = element_text(size = rel(0.85), color = cols$text),
            axis.line = element_line(color = cols$actual, linewidth = 0.5),
            axis.ticks = element_line(color = cols$actual, linewidth = 0.3),

            # Grid elements (IBCS: minimal gridlines)
            panel.grid.major.x = if (grid_x) {
                element_line(color = cols$grid, linewidth = 0.3)
            } else {
                element_blank()
            },
            panel.grid.major.y = if (grid_y) {
                element_line(color = cols$grid, linewidth = 0.3)
            } else {
                element_blank()
            },
            panel.grid.minor = element_blank(),

            # Panel elements
            panel.background = element_rect(fill = cols$background, color = NA),
            plot.background = element_rect(fill = cols$background, color = NA),

            # Legend (IBCS prefers integrated legends)
            legend.position = "top",
            legend.justification = "left",
            legend.title = element_text(face = "bold", size = rel(0.9)),
            legend.text = element_text(size = rel(0.85)),
            legend.background = element_blank(),
            legend.key = element_blank(),

            # Strip (facets)
            strip.text = element_text(face = "bold", size = rel(0.9)),
            strip.background = element_blank(),

            # Complete theme
            complete = TRUE
        )
}

#' make a 1 split 2 plot
#'
#' @param fig list of plots
#' @param first_level_sign sign
#'
#' @returns ggplot
#' @export
#'
one_to_split_to_two_layout <- function(fig,first_level_sign="+"){
    
    layout <- "
##BCC
AABCC
AABDD
##BDD
"    
    
    pa <- fig[[1]]
    pc <- fig[[2]]
    pd <- fig[[3]]

    pb <- make_connector(direction = "split", sign = first_level_sign)
    
    
    
    out <- pa + pb + pc + pd + plot_layout(design = layout)
    
    return(out)
    
}


#' Make a 2 merge to 1 plot
#'
#' Creates a layout where two plots on the left converge via a connector
#' into a single plot on the right.
#'
#' @param fig list of 3 ggplot objects: two left panels and one right panel
#' @param first_level_sign sign to display on the connector (default: "+")
#'
#' @returns ggplot
#' @export
#'
two_to_merge_to_one_layout <- function(fig, first_level_sign = "+") {

    layout <- "
CCB##
CCBAA
DDBAA
DDB##
"

    pa <- fig[[3]]
    pc <- fig[[1]]
    pd <- fig[[2]]

    pb <- make_connector(
        direction = "merge",
        sign = first_level_sign
    )

    out <- pa + pb + pc + pd + plot_layout(design = layout)

    return(out)
}


#' Make a connector panel
#'
#' Creates a standalone connector ggplot element for use in multi-panel layouts.
#' Supports split (one-to-two) and merge (two-to-one) directions.
#'
#' @param direction character, either "split" (left-to-right branching) or
#'   "merge" (right-to-left converging). Default: "split".
#' @param sign character label to display at the center of the connector.
#'   Default: "+".
#' @param line_col color of the connector lines. Default: "black".
#' @param line_size linewidth of the connector lines. Default: 0.5.
#' @param label_size text size of the center label. Default: 10.
#' @param label_fill background fill of the center label. Default: "white".
#'
#' @returns ggplot
#' @export
#'
make_connector <- function(direction = "split",
                           sign = "+",
                           line_col = "black",
                           line_size = 0.5,
                           label_size = 10,
                           label_fill = "white") {

    if (direction == "split") {
        segs <- data.frame(
            x    = c(-1, 0, 0, 0),
            xend = c( 0, 0, 1, 1),
            y    = c( 0, 3, 3, -3),
            yend = c( 0, -3, 3, -3)
        )
    } else if (direction == "merge") {
        segs <- data.frame(
            x    = c( 0, 0, -1, -1),
            xend = c( 1, 0,  0,  0),
            y    = c( 0, 3,  3, -3),
            yend = c( 0, -3, 3, -3)
        )
    } else {
        stop("direction must be 'split' or 'merge'")
    }

    p <- ggplot(segs, aes(x = x, xend = xend, y = y, yend = yend)) +
        geom_segment(color = line_col, linewidth = line_size) +
        annotate(
            "label",
            x = 0, y = 0,
            label = sign,
            label.r = unit(0.5, "lines"),
            fill = label_fill,
            size = label_size
        ) +
        coord_fixed() +
        theme_void()

    return(p)
}


#' Add panel labels to a patchwork plot
#'
#' Adds letter labels (e.g. "A", "B", "C") to each panel of a patchwork
#' composite plot.
#'
#' @param plot a patchwork plot object
#' @param labels character vector of labels. Default: uppercase letters
#'   matching the number of panels.
#' @param style one of "upper" (A, B, C), "lower" (a, b, c), or "custom"
#'   (use labels as-is). Default: "upper".
#' @param fontface font face for labels. Default: "bold".
#' @param size text size in pts. Default: 14.
#' @param x x position of label within each panel. Default: 0.
#' @param y y position of label within each panel. Default: 1.
#' @param hjust horizontal justification. Default: -0.1.
#' @param vjust vertical justification. Default: 1.1.
#'
#' @returns patchwork plot with annotation labels
#' @export
#'
add_panel_labels <- function(plot,
                             labels = NULL,
                             style = "upper",
                             fontface = "bold",
                             size = 14,
                             x = 0,
                             y = 1,
                             hjust = -0.1,
                             vjust = 1.1) {

    tag_levels <- switch(style,
        upper = "A",
        lower = "a",
        NULL
    )

    if (!is.null(labels)) {
        out <- plot + plot_annotation(tag_levels = list(labels))
    } else if (!is.null(tag_levels)) {
        out <- plot + plot_annotation(tag_levels = tag_levels)
    } else {
        stop("style must be 'upper', 'lower', or provide labels directly")
    }

    out <- out & theme(
        plot.tag = element_text(
            face = fontface,
            size = size,
            hjust = hjust,
            vjust = vjust
        ),
        plot.tag.position = c(x, y)
    )

    return(out)
}

# ============================================================================
# IBCS Scenario Notation
# ============================================================================

#' IBCS Scenario Fill Scale
#'
#' A fill scale using IBCS scenario notation:
#' - Actual (AC): solid dark fill
#' - Plan (PL): outlined (uses color, not fill)
#' - Previous Year (PY): lighter solid fill
#' - Forecast (FC): hatched pattern (approximated with lighter fill + pattern)
#'
#' @param ... Additional arguments passed to discrete_scale
#'
#' @returns A ggplot2 scale object
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'     scenario = c("AC", "PL", "PY", "FC"),
#'     value = c(100, 95, 90, 105)
#' )
#' ggplot(df, aes(x = scenario, y = value, fill = scenario)) +
#'     geom_col() +
#'     scale_fill_ibcs_scenario()
#'
scale_fill_ibcs_scenario <- function(...) {
    cols <- ibcs_colors()
    scale_fill_manual(
        values = c(
            "AC" = cols$actual,
            "Actual" = cols$actual,
            "PL" = NA,
            "Plan" = NA,
            "PY" = cols$previous,
            "Previous" = cols$previous,
            "FC" = cols$actual_light,
            "Forecast" = cols$actual_light
        ),
        ...
    )
}

#' IBCS Scenario Color Scale
#'
#' A color scale for IBCS scenario notation. Used for bar outlines,
#' especially for Plan (PL) which shows as outlined bars.
#'
#' @param ... Additional arguments passed to discrete_scale
#'
#' @returns A ggplot2 scale object
#' @export
#'
scale_color_ibcs_scenario <- function(...) {
    cols <- ibcs_colors()
    scale_color_manual(
        values = c(
            "AC" = cols$actual,
            "Actual" = cols$actual,
            "PL" = cols$plan,
            "Plan" = cols$plan,
            "PY" = cols$previous,
            "Previous" = cols$previous,
            "FC" = cols$forecast,
            "Forecast" = cols$forecast
        ),
        ...
    )
}

#' IBCS Variance Fill Scale
#'
#' A fill scale for variance charts using IBCS colors:
#' green for positive/favorable, red for negative/unfavorable.
#'
#' @param ... Additional arguments passed to discrete_scale
#'
#' @returns A ggplot2 scale object
#' @export
#'
scale_fill_ibcs_variance <- function(...) {
    cols <- ibcs_colors()
    scale_fill_manual(
        values = c(
            "positive" = cols$variance_positive,
            "negative" = cols$variance_negative,
            "favorable" = cols$variance_positive,
            "unfavorable" = cols$variance_negative
        ),
        ...
    )
}

# ============================================================================
# IBCS Variance Chart
# ============================================================================

#' Create an IBCS Variance Chart
#'
#' Creates a bar chart showing values with integrated variance indicators.
#' Variances are shown using IBCS standard colors (green = favorable,
#' red = unfavorable).
#'
#' @param data A data frame containing the data
#' @param x Column name for x-axis (categories)
#' @param actual Column name for actual values
#' @param reference Column name for reference values (plan, previous year, etc.)
#' @param show_variance_bars Show variance as separate bars. Default: TRUE.
#' @param show_variance_pct Show percentage variance. Default: TRUE.
#' @param horizontal Plot horizontal bars. Default: FALSE.
#' @param sort_by Sort bars: "none", "actual", "variance". Default: "none".
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' df <- data.frame(
#'     category = c("Product A", "Product B", "Product C", "Product D"),
#'     actual = c(120, 85, 95, 110),
#'     plan = c(100, 90, 100, 100)
#' )
#' ibcs_variance_chart(df, x = category, actual = actual, reference = plan)
#'
ibcs_variance_chart <- function(data,
                                 x,
                                 actual,
                                 reference,
                                 show_variance_bars = TRUE,
                                 show_variance_pct = TRUE,
                                 horizontal = FALSE,
                                 sort_by = "none") {

    cols <- ibcs_colors()

    # Use non-standard evaluation
    x_col <- deparse(substitute(x))
    actual_col <- deparse(substitute(actual))
    ref_col <- deparse(substitute(reference))

    # Calculate variances
    plot_data <- data
    plot_data$.actual <- plot_data[[actual_col]]
    plot_data$.reference <- plot_data[[ref_col]]
    plot_data$.variance <- plot_data$.actual - plot_data$.reference
    plot_data$.variance_pct <- (plot_data$.variance / plot_data$.reference) * 100
    plot_data$.variance_sign <- ifelse(plot_data$.variance >= 0, "positive", "negative")
    plot_data$.x <- plot_data[[x_col]]

    # Sorting
    if (sort_by == "actual") {
        plot_data$.x <- reorder(plot_data$.x, plot_data$.actual)
    } else if (sort_by == "variance") {
        plot_data$.x <- reorder(plot_data$.x, plot_data$.variance)
    }

    # Base plot with actual and reference
    p <- ggplot(plot_data, aes(x = .x))

    if (show_variance_bars) {
        # Show actual as solid bar, reference as outlined
        p <- p +
            geom_col(
                aes(y = .reference),
                fill = NA,
                color = cols$actual,
                linewidth = 0.8,
                width = 0.6
            ) +
            geom_col(
                aes(y = .actual),
                fill = cols$actual,
                width = 0.4
            )
    } else {
        p <- p +
            geom_col(
                aes(y = .actual),
                fill = cols$actual,
                width = 0.6
            )
    }

    # Add variance indicators
    p <- p +
        geom_point(
            aes(y = .actual, color = .variance_sign),
            shape = 18,
            size = 3,
            show.legend = FALSE
        ) +
        scale_color_manual(
            values = c(
                "positive" = cols$variance_positive,
                "negative" = cols$variance_negative
            )
        )

    # Add variance percentage labels
    if (show_variance_pct) {
        p <- p +
            geom_text(
                aes(
                    y = .actual,
                    label = sprintf("%+.1f%%", .variance_pct),
                    color = .variance_sign
                ),
                hjust = -0.2,
                size = 3,
                show.legend = FALSE
            )
    }

    # Apply theme and flip if horizontal
    p <- p + theme_ibcs()

    if (horizontal) {
        p <- p + coord_flip()
    }

    p <- p +
        labs(x = NULL, y = NULL) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

    return(p)
}

# ============================================================================
# IBCS Waterfall Chart
# ============================================================================

#' Create an IBCS Waterfall Chart
#'
#' Creates a waterfall chart following IBCS standards. Useful for showing
#' how an initial value is affected by intermediate positive/negative values
#' to reach a final value.
#'
#' @param data A data frame with categories and values
#' @param x Column name for categories (in order)
#' @param y Column name for values
#' @param type Column indicating type: "start", "end", "positive", "negative",
#'   or "subtotal". If NULL, inferred from values.
#' @param fill_positive Fill color for positive changes. Default: IBCS gray.
#' @param fill_negative Fill color for negative changes. Default: IBCS gray.
#' @param fill_total Fill color for start/end totals. Default: darker gray.
#' @param show_connectors Show connector lines between bars. Default: TRUE.
#' @param show_values Show value labels on bars. Default: TRUE.
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' df <- data.frame(
#'     category = c("Revenue", "COGS", "Gross Profit", "OpEx", "Net Income"),
#'     value = c(1000, -400, NA, -350, NA),
#'     type = c("start", "negative", "subtotal", "negative", "end")
#' )
#' ibcs_waterfall(df, x = category, y = value, type = type)
#'
ibcs_waterfall <- function(data,
                           x,
                           y,
                           type = NULL,
                           fill_positive = NULL,
                           fill_negative = NULL,
                           fill_total = NULL,
                           show_connectors = TRUE,
                           show_values = TRUE) {

    cols <- ibcs_colors()

    # Default colors (IBCS uses gray for waterfall, not red/green)
    if (is.null(fill_positive)) fill_positive <- cols$actual_light
    if (is.null(fill_negative)) fill_negative <- cols$actual_light
    if (is.null(fill_total)) fill_total <- cols$actual

    # Extract column names
    x_col <- deparse(substitute(x))
    y_col <- deparse(substitute(y))
    type_col <- if (!is.null(substitute(type))) deparse(substitute(type)) else NULL

    # Build waterfall data
    wf_data <- data
    wf_data$.x_label <- wf_data[[x_col]]
    wf_data$.x <- seq_len(nrow(wf_data))  # Use numeric positions
    wf_data$.y <- wf_data[[y_col]]

    if (!is.null(type_col) && type_col != "NULL") {
        wf_data$.type <- wf_data[[type_col]]
    } else {
        # Infer type: first is start, last is end, others based on sign
        n <- nrow(wf_data)
        wf_data$.type <- ifelse(
            wf_data$.y >= 0, "positive", "negative"
        )
        wf_data$.type[1] <- "start"
        wf_data$.type[n] <- "end"
    }

    # Calculate running total and bar positions
    wf_data$.running <- 0
    wf_data$.ymin <- 0
    wf_data$.ymax <- 0

    running <- 0
    for (i in seq_len(nrow(wf_data))) {
        if (wf_data$.type[i] %in% c("start", "subtotal")) {
            if (!is.na(wf_data$.y[i])) {
                running <- wf_data$.y[i]
            }
            wf_data$.ymin[i] <- 0
            wf_data$.ymax[i] <- running
        } else if (wf_data$.type[i] == "end") {
            wf_data$.ymin[i] <- 0
            wf_data$.ymax[i] <- running
            wf_data$.y[i] <- running
        } else {
            wf_data$.ymin[i] <- running
            running <- running + wf_data$.y[i]
            wf_data$.ymax[i] <- running
        }
        wf_data$.running[i] <- running
    }

    # Determine fill colors
    wf_data$.fill <- ifelse(
        wf_data$.type %in% c("start", "end", "subtotal"),
        fill_total,
        ifelse(wf_data$.y >= 0, fill_positive, fill_negative)
    )

    # Create plot with numeric x positions
    p <- ggplot(wf_data, aes(x = .x))

    # Add connector lines
    if (show_connectors && nrow(wf_data) > 1) {
        connector_data <- data.frame(
            .conn_x = head(wf_data$.x, -1) + 0.45,
            .conn_xend = tail(wf_data$.x, -1) - 0.45,
            .conn_y = head(wf_data$.running, -1),
            .conn_yend = head(wf_data$.running, -1)
        )
        p <- p +
            geom_segment(
                data = connector_data,
                aes(x = .conn_x, xend = .conn_xend, y = .conn_y, yend = .conn_yend),
                linetype = "dotted",
                color = cols$previous,
                linewidth = 0.5,
                inherit.aes = FALSE
            )
    }

    # Add bars
    p <- p +
        geom_rect(
            aes(
                xmin = .x - 0.4,
                xmax = .x + 0.4,
                ymin = .ymin,
                ymax = .ymax
            ),
            fill = wf_data$.fill,
            color = cols$actual,
            linewidth = 0.3
        )

    # Add value labels
    if (show_values) {
        p <- p +
            geom_text(
                aes(
                    y = (.ymin + .ymax) / 2,
                    label = ifelse(
                        .type %in% c("start", "end", "subtotal"),
                        sprintf("%.0f", .ymax),
                        sprintf("%+.0f", .y)
                    )
                ),
                color = "white",
                fontface = "bold",
                size = 3.5
            )
    }

    # Apply theme and scales
    p <- p +
        theme_ibcs() +
        labs(x = NULL, y = NULL) +
        scale_x_continuous(
            breaks = wf_data$.x,
            labels = wf_data$.x_label,
            expand = expansion(add = 0.5)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)))

    return(p)
}

# ============================================================================
# IBCS Hatched Pattern (for Forecast)
# ============================================================================

#' Create Hatched Fill Pattern for Forecast
#'
#' Returns a grob that can be used as a fill pattern for forecast data.
#' This is a helper for creating IBCS-compliant hatched bars.
#'
#' @param angle Angle of hatch lines in degrees. Default: 45.
#' @param spacing Spacing between lines. Default: 0.1.
#' @param color Color of hatch lines. Default: IBCS actual color.
#'
#' @returns A grid grob object
#' @keywords internal
#'
ibcs_hatch_pattern <- function(angle = 45,
                               spacing = 0.1,
                               color = NULL) {
    if (is.null(color)) color <- ibcs_colors()$actual

    # Create a simple hatch pattern grob
    grobTree(
        rectGrob(gp = gpar(fill = "white", col = NA)),
        linesGrob(
            x = unit(c(0, 1), "npc"),
            y = unit(c(0, 1), "npc"),
            gp = gpar(col = color, lwd = 1)
        )
    )
}

#' IBCS Bar Chart with Scenario Notation
#'
#' Creates a bar chart with proper IBCS scenario notation:
#' - Actual: solid filled bars
#' - Plan: outlined (hollow) bars
#' - Previous Year: lighter filled bars
#' - Forecast: hatched bars (shown as lighter with border)
#'
#' @param data A data frame
#' @param x Column for x-axis
#' @param y Column for y-axis (values)
#' @param scenario Column indicating scenario type (AC, PL, PY, FC)
#' @param width Bar width. Default: 0.7.
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' df <- data.frame(
#'     month = rep(c("Jan", "Feb", "Mar"), each = 2),
#'     scenario = rep(c("AC", "PL"), 3),
#'     value = c(100, 95, 110, 105, 120, 115)
#' )
#' ibcs_bar_chart(df, x = month, y = value, scenario = scenario)
#'
ibcs_bar_chart <- function(data, x, y, scenario, width = 0.7) {

    cols <- ibcs_colors()

    x_col <- deparse(substitute(x))
    y_col <- deparse(substitute(y))
    scenario_col <- deparse(substitute(scenario))

    plot_data <- data
    plot_data$.x <- plot_data[[x_col]]
    plot_data$.y <- plot_data[[y_col]]
    plot_data$.scenario <- plot_data[[scenario_col]]

    # Determine fill and color based on scenario
    plot_data$.fill <- ifelse(
        plot_data$.scenario %in% c("AC", "Actual"), cols$actual,
        ifelse(
            plot_data$.scenario %in% c("PY", "Previous"), cols$previous,
            ifelse(
                plot_data$.scenario %in% c("FC", "Forecast"), cols$actual_light,
                NA  # Plan gets no fill
            )
        )
    )

    plot_data$.color <- cols$actual
    plot_data$.linewidth <- ifelse(
        plot_data$.scenario %in% c("PL", "Plan"), 1.2, 0.3
    )

    # Dodge position for grouped bars
    pos <- position_dodge(width = width)

    p <- ggplot(plot_data, aes(x = .x, y = .y, group = .scenario)) +
        geom_col(
            aes(fill = .scenario, color = .scenario),
            position = pos,
            width = width * 0.9,
            linewidth = 0.8
        ) +
        scale_fill_manual(
            values = c(
                "AC" = cols$actual,
                "Actual" = cols$actual,
                "PL" = NA,
                "Plan" = NA,
                "PY" = cols$previous,
                "Previous" = cols$previous,
                "FC" = cols$actual_light,
                "Forecast" = cols$actual_light
            ),
            na.value = NA
        ) +
        scale_color_manual(
            values = c(
                "AC" = cols$actual,
                "Actual" = cols$actual,
                "PL" = cols$plan,
                "Plan" = cols$plan,
                "PY" = cols$previous,
                "Previous" = cols$previous,
                "FC" = cols$forecast,
                "Forecast" = cols$forecast
            )
        ) +
        theme_ibcs() +
        labs(x = NULL, y = NULL, fill = "Scenario", color = "Scenario")

    return(p)
}