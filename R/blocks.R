# Block Functions for Patchwork Compositions
# Create text blocks for title, subtitle, caption, narrative, and legend

#' Create a Text Narrative Block
#'
#' @description
#' Creates a ggplot object containing formatted text for combining
#' with charts via patchwork.
#'
#' @details
#' Use for adding story context, callouts, or the "so what?" explanation
#' alongside visualizations. Text supports marquee markdown formatting:
#' `**bold**`, `*italic*`, and `{#hexcolor text}` for colored text.
#'
#' Typically placed beside a chart using patchwork's `+` operator with
#' `plot_layout(widths = c(0.6, 0.4))` to control proportions.
#'
#' @param text Character string with marquee-formatted text. Supports
#'   markdown: `**bold**`, `*italic*`, `{#color text}`.
#' @param size Text size. Default: 4.
#' @param halign Horizontal alignment: "left", "center", or "right". Default: "left".
#' @param valign Vertical alignment: "top", "center", or "bottom". Default: "top".
#' @param padding Padding around text. Default: 10 on all sides.
#' @param lineheight Line height multiplier. Default: 1.4.
#' @param width Maximum width for text wrapping. Default: 1 (full width).
#' @param wrap_width Character width to wrap text at. Default: NULL (no wrapping).
#' @param ... Additional arguments passed to [marquee::geom_marquee()].
#'
#' @returns A ggplot object.
#'
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

    # Use narrower width to allow alignment to be visible
    text_width <- if (width == 1) 0.96 else width

    create_text_block(
        text = text,
        size = size,
        halign = halign,
        valign = valign,
        lineheight = lineheight,
        width = text_width,
        wrap_width = wrap_width,
        margin_top = padding,
        margin_right = padding,
        margin_bottom = padding,
        margin_left = padding,
        ...
    )
}

#' Create a Title Block Plot
#'
#' @description
#' Creates a standalone title element as a ggplot for use in patchwork
#' compositions.
#'
#' @details
#' Allows for larger, more prominent titles than standard ggplot titles.
#' Follows the SWD style where titles are bold statements that tell the
#' story, not just describe the chart. Supports marquee markdown:
#' `**bold**`, `*italic*`, and `{#hexcolor text}` for colored text.
#'
#' Use with patchwork's `/` operator to stack above your chart, and
#' `plot_layout(heights = ...)` to control proportions.
#'
#' @param title Main title text (marquee-formatted). Supports markdown
#'   formatting: `**bold**`, `*italic*`, `{#color text}`.
#' @param title_size Size for title in pts. Default: 12.
#' @param halign Horizontal alignment: "left", "center", or "right". Default: "left".
#' @param lineheight Line height multiplier. Default: 1.1.
#' @param margin_top Top margin in pts. Default: 5.
#' @param margin_bottom Bottom margin in pts. Default: 5.
#' @param margin_left Left margin in pts. Default: 5.
#' @param margin_right Right margin in pts. Default: 5.
#' @param width Maximum width for text wrapping (0-1). Default: 0.95.
#' @param wrap_width Character width to wrap text at. Default: NULL (no wrapping).
#' @param ... Additional arguments passed to [marquee::geom_marquee()].
#'
#' @returns A ggplot object that can be combined with other plots via patchwork.
#'
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

    create_text_block(
        text = title,
        size = title_size,
        halign = halign,
        y_pos = 0.95,
        vjust = 1,
        lineheight = lineheight,
        width = width,
        wrap_width = wrap_width,
        y_expand = expansion(mult = c(0.1, 0.02)),
        margin_top = margin_top,
        margin_right = margin_right,
        margin_bottom = margin_bottom,
        margin_left = margin_left,
        ...
    )
}

#' Create a Subtitle Block Plot
#'
#' @description
#' Creates a standalone subtitle element as a ggplot for use in patchwork
#' compositions.
#'
#' @details
#' Subtitles provide context and supporting information below the main title.
#' Use for timeframes, data descriptions, or methodological notes. Supports
#' marquee markdown: `**bold**`, `*italic*`, and `{#hexcolor text}`.
#'
#' Place below `title_block()` using patchwork's `/` operator.
#'
#' @param subtitle Subtitle text (marquee-formatted). Supports markdown
#'   formatting: `**bold**`, `*italic*`, `{#color text}`.
#' @param subtitle_size Size for subtitle in pts. Default: 10.
#' @param halign Horizontal alignment: "left", "center", or "right". Default: "left".
#' @param lineheight Line height multiplier. Default: 1.2.
#' @param margin_top Top margin in pts. Default: 0.
#' @param margin_bottom Bottom margin in pts. Default: 5.
#' @param margin_left Left margin in pts. Default: 5.
#' @param margin_right Right margin in pts. Default: 5.
#' @param width Maximum width for text wrapping (0-1). Default: 0.95.
#' @param wrap_width Character width to wrap text at. Default: NULL (no wrapping).
#' @param ... Additional arguments passed to [marquee::geom_marquee()].
#'
#' @returns A ggplot object that can be combined with other plots via patchwork.
#'
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

    create_text_block(
        text = subtitle,
        size = subtitle_size,
        halign = halign,
        y_pos = 0.9,
        vjust = 1,
        lineheight = lineheight,
        width = width,
        wrap_width = wrap_width,
        y_expand = expansion(mult = c(0.1, 0.02)),
        margin_top = margin_top,
        margin_right = margin_right,
        margin_bottom = margin_bottom,
        margin_left = margin_left,
        ...
    )
}

#' Create a Caption Block Plot
#'
#' @description
#' Creates a standalone caption element as a ggplot for use at the
#' bottom of patchwork compositions.
#'
#' @details
#' Captions typically contain source attributions, methodology notes, or
#' data caveats. Displayed in muted gray by default to avoid competing
#' with the main content. Supports marquee markdown formatting.
#'
#' Place at the bottom using patchwork's `/` operator.
#'
#' @param caption Caption text (marquee-formatted).
#' @param caption_size Size for caption in pts. Default: 6.
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
#'
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

    # Wrap caption in color if not already formatted
    caption_text <- maybe_wrap_text(caption, wrap_width)
    if (!grepl("^\\{#", caption_text)) {
        caption_text <- paste0("{", color, " ", caption_text, "}")
    }

    create_text_block(
        text = caption_text,
        size = caption_size,
        halign = halign,
        y_pos = 0.5,
        vjust = 0.5,
        width = 0.95,
        margin_top = margin_top,
        margin_right = margin_right,
        margin_bottom = margin_bottom,
        margin_left = margin_left,
        ...
    )
}

#' Create a Legend Block Plot
#'
#' @description
#' Creates a standalone inline legend as a ggplot, showing colored category
#' labels separated by a delimiter. Use above/below charts or beside them
#' in patchwork compositions to replace traditional legends with clean text.
#'
#' @details
#' Legend blocks follow SWD principles by embedding color meaning directly
#' in text rather than requiring readers to cross-reference a separate legend.
#' The colored labels can be positioned horizontally (as a row of labels
#' with separators) or vertically (stacked labels). Combine with other
#' block functions via patchwork to build complete story layouts.
#'
#' The function uses marquee syntax internally to render colored text.
#' Each category label is wrapped in its assigned color, with optional
#' bold formatting and case transformation.
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
        labels <- purrr::map_chr(labels, function(lbl) {
            paste(strwrap(lbl, width = wrap_width), collapse = "\n")
        })
    }

    # Format each label with its color using marquee syntax
    formatted <- purrr::map2_chr(labels, colors, function(label, color) {
        color <- color_to_hex(color)
        if (bold) {
            paste0("**{", color, " ", label, "}**")
        } else {
            paste0("{", color, " ", label, "}")
        }
    })

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
