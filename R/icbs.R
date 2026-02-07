#' @import ggplot2
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom grid gpar linesGrob grobTree rectGrob unit
#' @importFrom rlang quo quo_get_expr %||%
#' @importFrom stats reorder
#' @importFrom utils head tail
NULL

# Global variable declarations to avoid R CMD check notes
utils::globalVariables(c(
    ".actual", ".conn_x", ".conn_xend", ".conn_y", ".conn_yend",
    ".marquee_label", ".reference", ".scenario", ".type",
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