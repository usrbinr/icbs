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
