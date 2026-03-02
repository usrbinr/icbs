# Internal helper functions for stwd package
# These are not exported and used to reduce code duplication

#' Check if marquee package is available
#' @param fn_name Name of the function requiring marquee
#' @noRd
check_marquee <- function(fn_name) {
    if (!requireNamespace("marquee", quietly = TRUE)) {
        cli::cli_abort(
            "Package {.pkg marquee} is required for {.fn {fn_name}}."
        )
    }
}

#' Convert horizontal alignment to hjust value
#' @param halign Character: "left", "center", or "right"
#' @param default Default value if halign doesn't match
#' @return Numeric hjust value (0, 0.5, or 1)
#' @noRd
get_hjust <- function(halign, default = 0) {
    switch(halign, left = 0, center = 0.5, right = 1, default)
}

#' Convert horizontal alignment to x position
#' @param halign Character: "left", "center", or "right"
#' @param default Default value if halign doesn't match
#' @return Numeric x position (0.02, 0.5, or 0.98)
#' @noRd
get_x_pos <- function(halign, default = 0.02) {
    switch(halign, left = 0.02, center = 0.5, right = 0.98, default)
}

#' Convert vertical alignment to vjust value
#' @param valign Character: "top", "center", or "bottom"
#' @param default Default value if valign doesn't match
#' @return Numeric vjust value (0, 0.5, or 1)
#' @noRd
get_vjust <- function(valign, default = 1) {
    switch(valign, top = 1, center = 0.5, bottom = 0, default)
}

#' Convert vertical alignment to y position
#' @param valign Character: "top", "center", or "bottom"
#' @param default Default value if valign doesn't match
#' @return Numeric y position (0.02, 0.5, or 0.98)
#' @noRd
get_y_pos <- function(valign, default = 0.98) {
    switch(valign, top = 0.98, center = 0.5, bottom = 0.02, default)
}

#' Convert named R color to hex code
#' @param color Color name or hex code
#' @return Hex color code (e.g., "#FF0000")
#' @noRd
color_to_hex <- function(color) {
    if (grepl("^#", color)) {
        return(color)
    }
    tryCatch({
        rgb_vals <- grDevices::col2rgb(color)
        sprintf("#%02X%02X%02X", rgb_vals[1], rgb_vals[2], rgb_vals[3])
    }, error = function(e) {
        cli::cli_abort(c(
            "Invalid color: {.val {color}}",
            "i" = "Use a hex code like {.val #E69F00} or an R color name like {.val midnightblue}"
        ))
    })
}

#' Wrap text at specified width
#' @param text Character string to wrap
#' @param wrap_width Maximum width (NULL or 0 to disable)
#' @return Wrapped text with newlines
#' @noRd
maybe_wrap_text <- function(text, wrap_width) {
    if (!is.null(wrap_width) && wrap_width > 0) {
        paste(strwrap(text, width = wrap_width), collapse = "\n")
    } else {
        text
    }
}

#' Create a text block plot (internal helper)
#'
#' Core function for creating marquee text blocks. Used by title_block,
#' subtitle_block, caption_block, and text_narrative.
#'
#' @param text Text to display (marquee-formatted)
#' @param size Text size in pts
#' @param halign Horizontal alignment: "left", "center", "right"
#' @param valign Vertical alignment: "top", "center", "bottom" (NULL to use fixed y/vjust)
#' @param y_pos Fixed y position (used when valign is NULL)
#' @param vjust Fixed vjust value (used when valign is NULL)
#' @param lineheight Line height multiplier (NULL to omit)
#' @param width Text width for marquee wrapping (0-1)
#' @param wrap_width Character width for text wrapping (NULL to disable)
#' @param y_expand Y-axis expansion (NULL for default c(0,0))
#' @param margin_top,margin_right,margin_bottom,margin_left Margins in pts
#' @param ... Additional arguments passed to marquee::geom_marquee()
#' @return A ggplot object
#' @noRd
create_text_block <- function(text,
                              size,
                              halign = "left",
                              valign = NULL,
                              y_pos = NULL,
                              vjust = NULL,
                              lineheight = NULL,
                              width = 0.95,
                              wrap_width = NULL,
                              y_expand = NULL,
                              margin_top = 5,
                              margin_right = 5,
                              margin_bottom = 5,
                              margin_left = 5,
                              ...) {
    text <- maybe_wrap_text(text, wrap_width)

    hjust <- get_hjust(halign)
    x_pos <- get_x_pos(halign)

    # Use valign helpers if valign provided, otherwise use fixed values
    if (!is.null(valign)) {
        y_pos <- get_y_pos(valign)
        vjust <- get_vjust(valign)
    }

    # Build geom_marquee arguments
    marquee_args <- list(
        mapping = aes(x = x_pos, y = y_pos, label = text),
        hjust = hjust,
        vjust = vjust,
        size = size,
        width = width
    )
    if (!is.null(lineheight)) {
        marquee_args$lineheight <- lineheight
    }
    marquee_args <- c(marquee_args, list(...))

    # Build y-axis scale
    if (!is.null(y_expand)) {
        y_scale <- scale_y_continuous(limits = c(0, 1), expand = y_expand)
    } else {
        y_scale <- scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
    }

    ggplot() +
        do.call(marquee::geom_marquee, marquee_args) +
        scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        y_scale +
        theme_void() +
        theme(plot.margin = margin(margin_top, margin_right, margin_bottom, margin_left))
}

#' Convert text or ggplot to a block plot
#'
#' If input is already a ggplot, returns it unchanged.
#' If input is text, converts it using the specified block function.
#'
#' @param x Text string or ggplot object (or NULL)
#' @param block_fn Block function to use (e.g., title_block, subtitle_block)
#' @param ... Additional arguments passed to block_fn
#' @return A ggplot object or NULL
#' @noRd
as_block <- function(x, block_fn, ...) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "gg")) return(x)
    block_fn(x, ...)
}
