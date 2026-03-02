# STWD Theme
# Clean, minimal ggplot2 theme for data storytelling

#' STWD Theme for Storytelling
#'
#' @description
#' A clean, minimal ggplot2 theme optimized for data storytelling.
#'
#' @details
#' Follows best practices from "Storytelling with Data":
#' - White background with no chart border
#' - Horizontal grid lines only (subtle gray)
#' - No axis lines or ticks
#' - Legend positioned at top-left
#' - Clean typography with muted colors
#'
#' This theme removes visual distractions so the data takes center stage.
#'
#' @param base_size Base font size. Default: 11.
#' @param base_family Base font family. Default: "" (system default).
#'
#' @returns A ggplot2 theme object.
#'
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
