# Color Helper Functions
# Functions for working with colors in stwd visualizations

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
    "component", "height", "label",
    "x", "xend", "y", "yend",
    "ymin", "ymax", "ymid"
))

#' List Available R Color Names
#'
#' @description
#' Browse named R colors with their hex equivalents.
#'
#' @details
#' Prints a formatted list of color names and hex codes to the console.
#' Useful for finding colors to use in story_designer text fields or
#' when writing marquee syntax manually. The hex codes can be copied
#' directly into color inputs.
#'
#' @param pattern Optional regex pattern to filter color names.
#' @param n Maximum number of colors to show. Default: 20.
#'
#' @returns A data frame with color names and hex codes (invisibly).
#'
#' @export
#'
#' @examples
#' # Show common colors
#' list_colors()
#'
#' # Search for blue colors
#' list_colors("blue")
#'
#' # Search for dark colors
#' list_colors("dark")
#'
list_colors <- function(pattern = NULL, n = 20) {
    all_colors <- grDevices::colors(distinct = TRUE)

    if (!is.null(pattern)) {
        all_colors <- all_colors[grepl(pattern, all_colors, ignore.case = TRUE)]
    }

    all_colors <- head(all_colors, n)

    if (length(all_colors) == 0) {
        cli::cli_alert_warning("No colors found matching pattern: {.val {pattern}}")
        return(invisible(data.frame(name = character(), hex = character())))
    }

    hex_codes <- purrr::map_chr(all_colors, color_to_hex)
    result <- data.frame(name = all_colors, hex = hex_codes, row.names = NULL)

    cli::cli_h2("Available Colors")
    for (i in seq_len(nrow(result))) {
        cli::cli_text("{.field {result$name[i]}} \u2192 {.val {result$hex[i]}}")
    }

    if (!is.null(pattern)) {
        cli::cli_text("{.emph Showing {nrow(result)} colors matching '{pattern}'}")
    } else {
        cli::cli_text("{.emph Showing first {n} colors. Use pattern to search, e.g., list_colors('blue')}")
    }

    invisible(result)
}

#' Create a Highlight Color Palette
#'
#' @description
#' Creates a color vector where only specified categories are highlighted
#' and all others are grayed out.
#'
#' @details
#' Follows the Storytelling with Data principle of using color strategically
#' for emphasis. By graying out non-essential categories, the viewer's
#' attention is drawn to the highlighted data points. Use with
#' `scale_fill_manual()` or `scale_color_manual()`.
#'
#' @param categories Character vector of all category names.
#' @param highlight Character vector of categories to highlight, or a named
#'   vector where names are categories and values are their highlight colors.
#' @param highlight_color Color for highlighted categories (used when
#'   `highlight` is unnamed). Default: "#1E90FF" (dodger blue).
#' @param gray_color Color for non-highlighted categories. Default: light gray.
#'
#' @returns A named vector of colors.
#'
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
#' @description
#' Creates a data frame for adding colored text annotations as an
#' inline legend replacement.
#'
#' @details
#' Returns a data frame with marquee-formatted labels that can be added
#' to a plot using `marquee::geom_marquee()`. This allows placing a
#' color-coded legend directly within the plot area rather than using
#' the standard ggplot2 legend. Consider using `legend_block()` instead
#' for patchwork compositions.
#'
#' @param colors Named vector of colors (names = category labels).
#' @param x X position for the legend. Default: 0.
#' @param y Y position for the legend. Default: 0.
#' @param orientation Either "horizontal" or "vertical". Default: "horizontal".
#' @param sep Separator between items for horizontal layout. Default: bullet.
#' @param bold Make text bold. Default: TRUE.
#'
#' @returns A data frame with columns `x`, `y`, and `label` (marquee-formatted).
#'
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
    formatted <- purrr::map2_chr(labels, cols, function(label, color) {
        text <- if (bold) paste0("**", label, "**") else label
        paste0("{", color, " ", text, "}")
    })

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
