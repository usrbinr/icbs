#' @import ggplot2
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom patchwork plot_layout
#' @importFrom grid gpar linesGrob grobTree rectGrob unit
#' @importFrom rlang %||%
#' @importFrom stats setNames
#' @importFrom utils head tail
NULL

# Global variable declarations to avoid R CMD check notes
utils::globalVariables(c(
    ".actual", ".category", ".conn_x", ".conn_xend", ".conn_y", ".conn_yend",
    ".formatted_label", ".label_hjust", ".label_x", ".marquee_label",
    ".reference", ".scenario", ".type", ".value",
    ".variance_pct", ".variance_sign", ".x", ".y", ".ymax", ".ymin",
    "component", "height", "label",  # for story_designer
    "x", "xend", "y", "yend",
    "ymin", "ymax", "ymid"  # for story_designer clipping visualization
))

# ============================================================================
# Color Helper Functions
# ============================================================================

#' Color Text with Marquee Syntax
#'
#' A helper function to apply color to text using marquee syntax.
#' Accepts both hex codes (e.g., "#191970") and named R colors
#' (e.g., "midnightblue").
#'
#' @param color A color specified as hex code (e.g., "#E69F00") or
#'   named R color (e.g., "midnightblue", "steelblue", "forestgreen").
#' @param text The text to color.
#' @param bold Logical; wrap text in bold? Default: FALSE.
#'
#' @returns A marquee-formatted string for use in title, subtitle, or narrative.
#'
#' @export
#'
#' @examples
#' # Using hex code
#' stwd_color("#E69F00", "highlighted text")
#'
#' # Using named color
#' stwd_color("midnightblue", "North region")
#'
#' # With bold
#' stwd_color("steelblue", "important", bold = TRUE)
#'
#' # In a title
#' title <- paste0("Sales in ", stwd_color("forestgreen", "Q4"), " exceeded targets")
#'
stwd_color <- function(color, text, bold = FALSE) {
    color <- color_to_hex(color)
    result <- paste0("{", color, " ", text, "}")
    if (bold) {
        result <- paste0("**", result, "**")
    }
    result
}

#' List Available Color Names
#'
#' Shows common named colors with their hex equivalents for use with
#' `stwd_color()` or direct marquee syntax.
#'
#' @param pattern Optional regex pattern to filter color names.
#' @param n Maximum number of colors to show. Default: 20.
#'
#' @returns A data frame with color names and hex codes (invisibly).
#'   Prints a formatted list to the console.
#'
#' @export
#'
#' @examples
#' # Show common colors
#' stwd_colors()
#'
#' # Search for blue colors
#' stwd_colors("blue")
#'
#' # Search for dark colors
#' stwd_colors("dark")
#'
stwd_colors <- function(pattern = NULL, n = 20) {
    # Get all R color names
    all_colors <- grDevices::colors(distinct = TRUE)

    # Filter by pattern if provided
    if (!is.null(pattern)) {
        all_colors <- all_colors[grepl(pattern, all_colors, ignore.case = TRUE)]
    }

    # Limit to n colors
    all_colors <- head(all_colors, n)

    if (length(all_colors) == 0) {
        cli::cli_alert_warning("No colors found matching pattern: {.val {pattern}}")
        return(invisible(data.frame(name = character(), hex = character())))
    }

    # Convert to hex
    hex_codes <- sapply(all_colors, color_to_hex)

    result <- data.frame(name = all_colors, hex = hex_codes, row.names = NULL)

    # Print formatted output
    cli::cli_h2("Available Colors")
    for (i in seq_len(nrow(result))) {
        cli::cli_text("{.field {result$name[i]}} \u2192 {.val {result$hex[i]}}")
    }

    if (!is.null(pattern)) {
        cli::cli_text("{.emph Showing {nrow(result)} colors matching '{pattern}'}")
    } else {
        cli::cli_text("{.emph Showing first {n} colors. Use pattern to search, e.g., stwd_colors('blue')}")
    }

    invisible(result)
}

# ============================================================================
# Marquee Text Color Matching
# ============================================================================
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
#'   `highlight` is unnamed). Default: "#1E90FF" (dodger blue).
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
                             highlight_color = "#1E90FF",
                             gray_color = "#D3D3D3") {

    if (is.null(highlight_color)) {
        highlight_color <- "#1E90FF"
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
    # Format labels with marquee color syntax
    labels <- names(colors)
    cols <- unname(colors)
    formatted <- mapply(function(label, color) {
        text <- if (bold) paste0("**", label, "**") else label
        paste0("{", color, " ", text, "}")
    }, labels, cols, USE.NAMES = FALSE)

    if (orientation == "horizontal") {
        label <- paste(formatted, collapse = sep)
        data.frame(x = x, y = y, label = label)
    } else {
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
#'     theme_minimal() +
#'     theme_marquee()
#' }
#'
theme_marquee <- function() {
    check_marquee("theme_marquee")
    theme(
        plot.title = marquee::element_marquee(),
        plot.subtitle = marquee::element_marquee(),
        plot.caption = marquee::element_marquee(),
        legend.text = marquee::element_marquee()
    )
}

#' STWD Theme for Storytelling
#'
#' A clean, minimal theme optimized for data storytelling. Follows
#' best practices from "Storytelling with Data" - removes distractions,
#' emphasizes the data, and provides clean typography.
#'
#' @param base_size Base font size. Default: 11.
#' @param base_family Base font family. Default: "" (system default).
#'
#' @returns A ggplot2 theme object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'     geom_point() +
#'     theme_stwd()
#' }
#'
theme_stwd <- function(base_size = 11, base_family = "") {
    theme_minimal(base_size = base_size, base_family = base_family) +
        theme(
            # Clean background
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),

            # Subtle grid - only major horizontal lines
            panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),

            # Clean axes
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_text(size = base_size, color = "#333333"),
            axis.text = element_text(size = base_size - 1, color = "#666666"),

            # No legend title by default
            legend.title = element_blank(),
            legend.position = "top",
            legend.justification = "left",
            legend.text = element_text(size = base_size - 1, color = "#666666"),

            # Clean spacing
            plot.margin = margin(15, 15, 15, 15),
            plot.title = element_text(
                size = base_size + 4,
                face = "bold",
                color = "#333333",
                hjust = 0,
                margin = margin(b = 10)
            ),
            plot.subtitle = element_text(
                size = base_size + 1,
                color = "#666666",
                hjust = 0,
                margin = margin(b = 15)
            ),
            plot.caption = element_text(
                size = base_size - 2,
                color = "#999999",
                hjust = 0
            )
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
#' @param lineheight Line height multiplier. Default: 1.4.
#' @param width Maximum width for text wrapping. Default: 1 (full width).
#' @param wrap_width Character width to wrap text at. Default: NULL (no wrapping).
#' @param ... Additional arguments passed to [marquee::geom_marquee()].
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
                           lineheight = 1.4,
                           width = 1,
                           wrap_width = NULL,
                           ...) {
    check_marquee("text_narrative")
    text <- maybe_wrap_text(text, wrap_width)

    hjust <- get_hjust(halign)
    vjust <- get_vjust(valign)
    x_pos <- get_x_pos(halign)
    y_pos <- get_y_pos(valign)

    # Use narrower width to allow alignment to be visible
    text_width <- if (width == 1) 0.96 else width

    p <- ggplot() +
        marquee::geom_marquee(
            aes(x = x_pos, y = y_pos, label = text),
            hjust = hjust,
            vjust = vjust,
            size = size,
            lineheight = lineheight,
            width = text_width,
            ...
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
#' @param wrap_width Character width to wrap text at. Default: NULL (no wrapping).
#' @param ... Additional arguments passed to [marquee::geom_marquee()].
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
                        title_size = 12,
                        halign = "left",
                        lineheight = 1.1,
                        margin_top = 5,
                        margin_bottom = 5,
                        margin_left = 5,
                        margin_right = 5,
                        width = 0.95,
                        wrap_width = NULL,
                        ...) {
    check_marquee("title_block")
    title <- maybe_wrap_text(title, wrap_width)
    hjust <- get_hjust(halign)
    x_pos <- get_x_pos(halign)

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
            width = width,
            ...
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
#' @param wrap_width Character width to wrap text at. Default: NULL (no wrapping).
#' @param ... Additional arguments passed to [marquee::geom_marquee()].
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
                           subtitle_size = 10,
                           halign = "left",
                           lineheight = 1.2,
                           margin_top = 0,
                           margin_bottom = 5,
                           margin_left = 5,
                           margin_right = 5,
                           width = 0.95,
                           wrap_width = NULL,
                           ...) {
    check_marquee("subtitle_block")
    subtitle <- maybe_wrap_text(subtitle, wrap_width)
    hjust <- get_hjust(halign)
    x_pos <- get_x_pos(halign)

    p <- ggplot() +
        marquee::geom_marquee(
            aes(x = x_pos, y = 0.9, label = subtitle),
            hjust = hjust,
            vjust = 1,
            size = subtitle_size,
            lineheight = lineheight,
            width = width,
            ...
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
#' @param wrap_width Character width to wrap text at. Default: NULL (no wrapping).
#' @param ... Additional arguments passed to [marquee::geom_marquee()].
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
                          caption_size = 6,
                          halign = "left",
                          color = "#808080",
                          margin_top = 10,
                          margin_bottom = 5,
                          margin_left = 5,
                          margin_right = 5,
                          wrap_width = NULL,
                          ...) {
    check_marquee("caption_block")
    caption <- maybe_wrap_text(caption, wrap_width)
    hjust <- get_hjust(halign)
    x_pos <- get_x_pos(halign)

    # Wrap caption in color if not already formatted
    if (!grepl("^\\{#", caption)) {
        caption <- paste0("{", color, " ", caption, "}")
    }

    p <- ggplot() +
        marquee::geom_marquee(
            aes(x = x_pos, y = 0.5, label = caption),
            hjust = hjust,
            vjust = 0.5,
            size = caption_size,
            ...
        ) +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        theme_void() +
        theme(plot.margin = margin(margin_top, margin_right, margin_bottom, margin_left))

    p
}

#' Create a Legend Block Plot
#'
#' Creates a standalone inline legend as a ggplot, showing colored category
#' labels separated by a delimiter. Use above/below charts or beside them
#' in patchwork compositions to replace traditional legends with clean text.
#'
#' @param colors Named vector where names are category labels and values are
#'   colors (hex codes or R color names).
#' @param halign Horizontal alignment: "left", "center", or "right". Default: "right".
#' @param valign Vertical alignment (for vertical orientation): "top", "center",
#'   or "bottom". Default: "center".
#' @param orientation Either "horizontal" (labels side by side) or "vertical"
#'   (labels stacked). Default: "horizontal".
#' @param sep Separator between category labels (horizontal only). Default: " | ".
#' @param size Text size in pts. Default: 10.
#' @param bold Logical; make labels bold? Default: TRUE.
#' @param uppercase Logical; convert labels to uppercase? Default: FALSE.
#' @param lineheight Line height multiplier for vertical legends. Default: 1.6.
#' @param width Text width for wrapping (0-1). Default: 0.95.
#' @param wrap_width Character width to wrap labels at. Useful for long labels
#'   in vertical legends. Default: NULL (no wrapping).
#' @param margin_top Top margin in pts. Default: 0.
#' @param margin_bottom Bottom margin in pts. Default: 5.
#' @param margin_left Left margin in pts. Default: 5.
#' @param margin_right Right margin in pts. Default: 5.
#' @param ... Additional arguments passed to [marquee::geom_marquee()].
#'
#' @returns A ggplot object that can be combined with other plots via patchwork.
#' @export
#'
#' @examples
#' \dontrun{
#' library(patchwork)
#'
#' legend_colors <- c("ACCURATE" = "#808080", "NULL" = "#B0B0B0", "ERROR" = "#E69F00")
#'
#' # Horizontal above chart
#' legend_plot <- legend_block(legend_colors, halign = "right")
#'
#' # Vertical beside chart
#' legend_vert <- legend_block(legend_colors, orientation = "vertical", valign = "top")
#'
#' title_block("My Title") / legend_plot / my_chart +
#'     plot_layout(heights = c(0.08, 0.04, 0.88))
#' }
#'
legend_block <- function(colors,
                         halign = "right",
                         valign = "center",
                         orientation = "horizontal",
                         sep = " | ",
                         size = 10,
                         bold = TRUE,
                         uppercase = FALSE,
                         lineheight = 1.6,
                         width = 0.95,
                         wrap_width = NULL,
                         margin_top = 0,
                         margin_bottom = 5,
                         margin_left = 5,
                         margin_right = 5,
                         ...) {
    check_marquee("legend_block")

    if (length(colors) == 0) {
        cli::cli_abort("At least one color must be provided.")
    }

    # Get labels
    labels <- names(colors)
    if (is.null(labels)) {
        cli::cli_abort("Colors must be a named vector (names = category labels).")
    }

    if (uppercase) {
        labels <- toupper(labels)
    }

    # Wrap labels if wrap_width specified
    if (!is.null(wrap_width) && wrap_width > 0) {
        labels <- sapply(labels, function(lbl) {
            paste(strwrap(lbl, width = wrap_width), collapse = "\n")
        }, USE.NAMES = FALSE)
    }

    # Format each label with its color using marquee syntax
    formatted <- mapply(function(label, color) {
        color <- color_to_hex(color)
        if (bold) {
            paste0("**{", color, " ", label, "}**")
        } else {
            paste0("{", color, " ", label, "}")
        }
    }, labels, colors, SIMPLIFY = TRUE, USE.NAMES = FALSE)

    # Position settings (default to right alignment for legend)
    hjust <- get_hjust(halign, default = 1)
    x_pos <- get_x_pos(halign, default = 0.98)

    if (orientation == "vertical") {
        # Vertical: stack labels with newlines
        legend_text <- paste(formatted, collapse = "\n")
        vjust_val <- get_vjust(valign, default = 0.5)
        # Legend uses tighter margins (0.95/0.05) than standard blocks (0.98/0.02)
        y_pos <- switch(valign, top = 0.95, center = 0.5, bottom = 0.05, 0.5)

        p <- ggplot() +
            marquee::geom_marquee(
                aes(x = x_pos, y = y_pos, label = legend_text),
                hjust = hjust,
                vjust = vjust_val,
                size = size,
                lineheight = lineheight,
                width = width,
                ...
            ) +
            scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
            scale_y_continuous(limits = c(0, 1), expand = c(0.05, 0.05)) +
            theme_void() +
            theme(plot.margin = margin(margin_top, margin_right, margin_bottom, margin_left))
    } else {
        # Horizontal: combine with separator
        legend_text <- paste(formatted, collapse = sep)

        p <- ggplot() +
            marquee::geom_marquee(
                aes(x = x_pos, y = 0.5, label = legend_text),
                hjust = hjust,
                vjust = 0.5,
                size = size,
                width = width,
                ...
            ) +
            scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
            scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
            theme_void() +
            theme(plot.margin = margin(margin_top, margin_right, margin_bottom, margin_left))
    }

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
    check_marquee("title_header")
    hjust <- get_hjust(halign)
    x_pos <- get_hjust(halign)
    # Note: title_header uses hjust value for x_pos (0, 0.5, 1) not edge offset

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
        title_height <- max(0.10, est_lines * height_per_line + 0.02)
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

