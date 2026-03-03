# Text Helper Functions
# Functions for processing and formatting text, particularly for marquee syntax

#' Convert named colors to hex in marquee syntax
#'
#' Finds patterns like `{red text}` and converts to `{#FF0000 text}`.
#'
#' @param text Text potentially containing named color syntax
#' @return Text with named colors converted to hex codes
#' @noRd
convert_named_colors <- function(text) {
    if (is.null(text) || text == "") return(text)

    pattern <- "\\{([a-zA-Z][a-zA-Z0-9]*)\\s+([^}]+)\\}"
    matches <- gregexpr(pattern, text, perl = TRUE)
    if (matches[[1]][1] == -1) return(text)

    all_matches <- regmatches(text, matches)[[1]]
    purrr::reduce(all_matches, function(result, match) {
        parts <- regmatches(match, regexec(pattern, match, perl = TRUE))[[1]]
        color_name <- parts[2]
        content <- parts[3]
        tryCatch({
            rgb_vals <- grDevices::col2rgb(color_name)
            hex_code <- sprintf("#%02X%02X%02X", rgb_vals[1], rgb_vals[2], rgb_vals[3])
            replacement <- paste0("{", hex_code, " ", content, "}")
            sub(match, replacement, result, fixed = TRUE)
        }, error = function(e) result)
    }, .init = text)
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

#' Strip marquee formatting from text
#'
#' Removes marquee syntax (colors and bold/italic markers) to get plain text.
#' Used for character counting in height estimation.
#'
#' @param text Text with marquee formatting
#' @return Plain text without formatting
#' @noRd
strip_marquee_formatting <- function(text) {
    if (is.null(text) || !is.character(text)) return("")
    text <- gsub("\\{#[A-Fa-f0-9]+ ([^}]+)\\}", "\\1", text)
    gsub("\\*+", "", text)
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
