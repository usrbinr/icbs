# Plot Analysis Functions
# Functions for analyzing ggplot objects to extract category information

#' Extract levels from a ggplot aesthetic mapping
#'
#' Looks for a mapping (fill, colour, etc.) first at the plot level,
#' then in layer mappings. Returns unique values/levels from the data.
#'
#' @param plot A ggplot object
#' @param mapping_name Name of the aesthetic (e.g., "fill", "colour")
#' @return Character vector of levels, or NULL if not found
#' @noRd
extract_mapping_levels <- function(plot, mapping_name) {
    plot_data <- plot$data
    if (is.null(plot_data) || !is.data.frame(plot_data)) return(NULL)

    # Helper to get levels from a column
    get_levels <- function(col_name) {
        if (is.null(col_name) || !col_name %in% names(plot_data)) return(NULL)
        vals <- plot_data[[col_name]]
        if (is.factor(vals)) levels(vals)
        else if (is.character(vals)) unique(vals)
        else NULL
    }

    # Helper to extract column name from mapping
    get_col_name <- function(mapping) {
        tryCatch(rlang::as_name(mapping), error = function(e) NULL)
    }

    # Try plot-level mapping first
    plot_mapping <- plot$mapping[[mapping_name]]
    if (!is.null(plot_mapping)) {
        result <- get_levels(get_col_name(plot_mapping))
        if (!is.null(result)) return(result)
    }

    # Try layer mappings
    for (layer in plot$layers) {
        # Handle both "colour" and "color" spellings
        layer_mapping <- layer$mapping[[mapping_name]]
        if (is.null(layer_mapping) && mapping_name == "colour") {
            layer_mapping <- layer$mapping[["color"]]
        }
        if (!is.null(layer_mapping)) {
            result <- get_levels(get_col_name(layer_mapping))
            if (!is.null(result)) return(result)
        }
    }

    NULL
}

#' Detect plot categories (fill and color levels)
#'
#' @param plot A ggplot object
#' @return List with fill_levels, color_levels, n_fill, n_color
#' @noRd
detect_plot_categories <- function(plot) {
    fill_levels <- extract_mapping_levels(plot, "fill")
    color_levels <- extract_mapping_levels(plot, "colour")

    list(
        fill_levels = fill_levels,
        color_levels = color_levels,
        n_fill = length(fill_levels),
        n_color = length(color_levels)
    )
}
