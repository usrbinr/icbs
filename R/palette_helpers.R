# Palette Helper Functions
# Functions for working with color palettes in story_designer

#' Get palette names for a given package
#' @noRd
get_palette_names <- function(pkg) {
    switch(pkg,
        "ggsci" = if (requireNamespace("ggsci", quietly = TRUE)) {
            c("npg", "aaas", "nejm", "lancet", "jama", "jco", "ucscgb",
              "d3", "locuszoom", "igv", "uchicago", "startrek", "tron",
              "futurama", "rickandmorty", "simpsons", "gsea", "material")
        } else character(0),
        "MetBrewer" = if (requireNamespace("MetBrewer", quietly = TRUE)) {
            names(MetBrewer::MetPalettes)
        } else character(0),
        "nord" = if (requireNamespace("nord", quietly = TRUE)) {
            names(nord::nord_palettes)
        } else character(0),
        "PNWColors" = if (requireNamespace("PNWColors", quietly = TRUE)) {
            names(PNWColors::pnw_palettes)
        } else character(0),
        "rcartocolor" = if (requireNamespace("rcartocolor", quietly = TRUE)) {
            rcartocolor::cartocolors$Name
        } else character(0),
        "RColorBrewer" = if (requireNamespace("RColorBrewer", quietly = TRUE)) {
            rownames(RColorBrewer::brewer.pal.info)
        } else character(0),
        "scico" = if (requireNamespace("scico", quietly = TRUE)) {
            scico::scico_palette_names()
        } else character(0),
        "viridis" = c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo"),
        "wesanderson" = if (requireNamespace("wesanderson", quietly = TRUE)) {
            names(wesanderson::wes_palettes)
        } else character(0),
        character(0)
    )
}

#' Get colors from a palette
#' @noRd
get_palette_colors <- function(pkg, name, n = 8) {
    tryCatch({
        switch(pkg,
            "ggsci" = {
                pal_fn <- get(paste0("pal_", name), envir = asNamespace("ggsci"))
                pal_fn()(n)
            },
            "MetBrewer" = MetBrewer::met.brewer(name, n),
            "nord" = nord::nord(name, n),
            "PNWColors" = PNWColors::pnw_palette(name, n),
            "rcartocolor" = rcartocolor::carto_pal(n, name),
            "RColorBrewer" = {
                max_n <- RColorBrewer::brewer.pal.info[name, "maxcolors"]
                RColorBrewer::brewer.pal(min(n, max_n), name)
            },
            "scico" = scico::scico(n, palette = name),
            "viridis" = if (requireNamespace("viridis", quietly = TRUE)) {
                viridis::viridis(n, option = name)
            } else grDevices::hcl.colors(n, name),
            "wesanderson" = wesanderson::wes_palette(name, n, type = "continuous"),
            grDevices::hcl.colors(n)
        )
    }, error = function(e) grDevices::hcl.colors(n))
}

#' Default colors for legend
#' @noRd
default_legend_colors <- c("#808080", "#B0B0B0", "#E69F00", "#56B4E9", "#009E73",
                           "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

#' Get category levels based on apply_to setting
#' @param cats List with fill_levels and color_levels
#' @param apply_to One of "fill", "color", or "both"
#' @return Character vector of levels or NULL
#' @noRd
get_levels_for_apply <- function(cats, apply_to) {
    if (apply_to == "fill") cats$fill_levels
    else if (apply_to == "color") cats$color_levels
    else if (!is.null(cats$fill_levels)) cats$fill_levels
    else cats$color_levels
}

#' Get caption horizontal alignment from position
#' @param position Caption position setting
#' @return Alignment string ("left", "center", or "right")
#' @noRd
get_caption_halign <- function(position) {
    switch(position %||% "full_left",
        "full_left" = "left",
        "full_center" = "center",
        "full_right" = "right",
        "under_chart" = "left",
        "left"
    )
}

#' Get legend orientation from position
#' @param position Legend position ("left", "right", "above", "below")
#' @return "vertical" or "horizontal"
#' @noRd
get_legend_orientation <- function(position) {
    if (position %in% c("left", "right")) "vertical" else "horizontal"
}

#' Apply color scales to a plot
#' @noRd
apply_color_scales <- function(p, colors, apply_to, scale_type) {
    if (is.null(colors) || length(colors) == 0) return(p)

    tryCatch({
        p_new <- p
        if (scale_type == "discrete") {
            if (apply_to %in% c("fill", "both")) {
                p_new <- p_new + ggplot2::scale_fill_manual(values = colors)
            }
            if (apply_to %in% c("color", "both")) {
                p_new <- p_new + ggplot2::scale_color_manual(values = colors)
            }
        } else if (length(colors) >= 2) {
            if (apply_to %in% c("fill", "both")) {
                p_new <- p_new + ggplot2::scale_fill_gradientn(colors = colors)
            }
            if (apply_to %in% c("color", "both")) {
                p_new <- p_new + ggplot2::scale_color_gradientn(colors = colors)
            }
        }
        ggplot2::ggplot_build(p_new)  # Test if valid
        p_new
    }, error = function(e) p)
}
