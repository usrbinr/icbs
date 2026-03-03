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
    # Helper to get levels from a column in a data frame
    # Only returns levels for discrete data (factor, character, or small integer sets)
    get_levels_from_data <- function(data, col_name) {
        if (is.null(data) || !is.data.frame(data)) return(NULL)
        if (is.null(col_name) || !col_name %in% names(data)) return(NULL)
        vals <- data[[col_name]]
        if (is.factor(vals)) {
            levels(vals)
        } else if (is.character(vals)) {
            unique(vals)
        } else {
            # Numeric values are typically continuous - don't treat as discrete
            NULL
        }
    }

    # Helper to extract column name from mapping (simple symbols only)
    get_col_name <- function(mapping) {
        tryCatch(rlang::as_name(mapping), error = function(e) NULL)
    }

    # Helper to evaluate a mapping expression against data
    # Only returns levels for discrete results (factor, character)
    eval_mapping <- function(mapping, data) {
        if (is.null(mapping) || is.null(data)) return(NULL)
        tryCatch({
            result <- rlang::eval_tidy(mapping, data = data)
            if (is.factor(result)) {
                # Use unique values actually in data, not all factor levels
                unique(as.character(result))
            } else if (is.character(result)) {
                unique(result)
            } else {
                # Numeric results are continuous - don't treat as discrete
                NULL
            }
        }, error = function(e) NULL)
    }

    # Helper to find levels from a mapping and data source
    find_levels <- function(mapping, data) {
        if (is.null(mapping)) return(NULL)
        # First try simple column lookup
        col_name <- get_col_name(mapping)
        result <- get_levels_from_data(data, col_name)
        if (!is.null(result)) return(result)
        # Fall back to evaluating the expression (handles interaction(), paste(), etc.)
        eval_mapping(mapping, data)
    }

    # Try plot-level mapping with plot data
    plot_mapping <- plot$mapping[[mapping_name]]
    if (!is.null(plot_mapping)) {
        result <- find_levels(plot_mapping, plot$data)
        if (!is.null(result)) return(result)
    }

    # Try layer mappings with layer or plot data
    for (layer in plot$layers) {
        layer_mapping <- layer$mapping[[mapping_name]]
        if (is.null(layer_mapping) && mapping_name == "colour") {
            layer_mapping <- layer$mapping[["color"]]
        }
        if (!is.null(layer_mapping)) {
            # Try layer data first, then fall back to plot data
            layer_data <- layer$data
            if (is.function(layer_data)) {
                layer_data <- tryCatch(layer_data(plot$data), error = function(e) NULL)
            }
            if (is.null(layer_data) || identical(layer_data, ggplot2::waiver())) {
                layer_data <- plot$data
            }
            result <- find_levels(layer_mapping, layer_data)
            if (!is.null(result)) return(result)
        }
    }

    # Last resort: try to build the plot and extract from scales
    tryCatch({
        built <- ggplot2::ggplot_build(plot)
        scale_name <- if (mapping_name == "colour") "colour" else mapping_name
        scale <- built$plot$scales$get_scales(scale_name)
        if (!is.null(scale) && !is.null(scale$get_limits)) {
            limits <- scale$get_limits()
            if (is.character(limits) || is.factor(limits)) {
                return(as.character(limits))
            }
        }
        NULL
    }, error = function(e) NULL)
}

#' Detect plot categories (fill and color levels)
#'
#' @param plot A ggplot object
#' @return List with fill_levels, color_levels, n_fill, n_color, fill_colors, color_colors
#' @noRd
detect_plot_categories <- function(plot) {
    fill_levels <- extract_mapping_levels(plot, "fill")
    color_levels <- extract_mapping_levels(plot, "colour")

    # Try to extract existing colors from scales
    fill_colors <- extract_scale_colors(plot, "fill")
    color_colors <- extract_scale_colors(plot, "colour")

    list(
        fill_levels = fill_levels,
        color_levels = color_levels,
        n_fill = length(fill_levels),
        n_color = length(color_levels),
        fill_colors = fill_colors,
        color_colors = color_colors
    )
}

#' Extract colors from a plot's existing scale
#'
#' @param plot A ggplot object
#' @param scale_name "fill" or "colour"
#' @return Named character vector of colors, or NULL
#' @noRd
extract_scale_colors <- function(plot, scale_name) {
    tryCatch({
        # Check for scale in plot$scales
        for (scale in plot$scales$scales) {
            if (scale_name %in% scale$aesthetics) {
                # Check if it has a palette or values
                if (!is.null(scale$palette)) {
                    # For manual scales, palette is a function that returns colors
                    pal <- scale$palette
                    if (is.function(pal)) {
                        # Try to get the values
                        n <- length(extract_mapping_levels(plot, scale_name))
                        if (n > 0) {
                            cols <- pal(n)
                            return(cols)
                        }
                    }
                }
                # For scale_*_manual, values are stored directly
                if (!is.null(scale$palette.cache)) {
                    return(scale$palette.cache)
                }
            }
        }

        # Fallback: build the plot and extract from the built data
        built <- ggplot2::ggplot_build(plot)
        scale <- built$plot$scales$get_scales(scale_name)
        if (!is.null(scale) && !is.null(scale$palette.cache)) {
            return(scale$palette.cache)
        }

        NULL
    }, error = function(e) NULL)
}
