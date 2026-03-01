# =============================================================================
# Recreating the Storytelling with Data Look with stwd
# Based on: https://albert-rapp.de/posts/ggplot2-tips/10_recreating_swd_look/
# =============================================================================
#
# This example demonstrates how to use stwd::story_layout() to create
# presentation-ready visualizations following SWD principles.
#
# KEY CONCEPTS:
# 1. Marquee syntax for colored/formatted text
# 2. Strategic color highlighting
# 3. Modular layout components
# 4. Narrative that explains the "so what?"

library(ggplot2)
library(dplyr)
library(tidyr)
library(stwd)
library(patchwork)

# =============================================================================
# MARQUEE SYNTAX GUIDE
# =============================================================================
#
# The marquee package (used by stwd) supports markdown-like formatting:
#
#   **bold text**           -> bold
#   *italic text*           -> italic
#   {#E69F00 colored text}  -> text in specified hex color
#   {#E69F00 **bold color**} -> bold AND colored
#
# Examples:
#   "Sales are {#E63946 down 15%}"        -> "down 15%" in red
#   "**Key finding:** profits {#2A9D8F increased}" -> bold label, green text
#
# Use colors that MATCH your chart elements for consistency!

# =============================================================================
# DATA SETUP
# =============================================================================

# Warehouse performance data (19 highest-volume warehouses)
warehouse_data <- tibble::tibble(
    warehouse = paste0("WH-", sprintf("%02d", 1:19)),
    accurate = c(78, 80, 81, 82, 84, 85, 86, 87, 88, 89,
                 90, 91, 91, 92, 92, 93, 93, 94, 94),
    null = c(6, 6, 5, 5, 4, 4, 4, 3, 3, 3,
             2, 2, 2, 2, 2, 1, 1, 1, 1),
    error = c(16, 14, 14, 13, 12, 11, 10, 10, 9, 8,
              8, 7, 7, 6, 6, 6, 6, 5, 5)
)

# Flag high-error warehouses (>=10% error rate)
warehouse_data <- warehouse_data |>
    mutate(high_error = error >= 10)

# =============================================================================
# COLOR STRATEGY
# =============================================================================
#
# SWD Principle: Use color SPARINGLY and STRATEGICALLY
# - Gray out everything that's not the focus
# - Use ONE highlight color for the key message
# - Match text colors to chart colors for consistency

highlight_color <- "#E69F00"  # Orange for high-error warehouses

# Pivot data and assign colors based on our strategy
plot_data <- warehouse_data |>
    pivot_longer(
        cols = c(accurate, null, error),
        names_to = "metric",
        values_to = "pct"
    ) |>
    mutate(
        metric = factor(metric, levels = c("accurate", "null", "error")),
        # Color logic:
        # - High-error warehouses get highlight color (orange)
        # - Everything else is gray (de-emphasized)
        fill_color = case_when(
            metric == "error" & high_error ~ highlight_color,  # THE FOCUS
            metric == "error" ~ "grey70",                       # Other errors
            metric == "null" ~ "grey85",                        # Null (lighter)
            TRUE ~ "grey90"                                     # Accurate (lightest)
        )
    )

# =============================================================================
# BUILD THE CHART
# =============================================================================

chart <- ggplot(
    plot_data,
    aes(
        x = pct,
        y = reorder(warehouse, -as.numeric(factor(warehouse))),
        fill = fill_color
    )
) +
    geom_col(position = "stack", width = 0.7) +
    scale_fill_identity() +
    scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, 100),
        position = "top",
        breaks = seq(0, 100, 25),
        labels = paste0(seq(0, 100, 25), "%")
    ) +
    theme_ibcs(grid_x = TRUE, grid_y = FALSE) +
    theme(
        axis.text = element_text(size = 9, color = "grey40"),
        axis.title = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        plot.margin = margin(5, 10, 5, 5)
    )

# =============================================================================
# COMPOSE THE STORY LAYOUT
# =============================================================================
#
# story_layout() combines:
# - title: The key insight (not just a description!)
# - subtitle: Supporting context
# - narrative: The "so what?" and "now what?"
# - caption: Source for credibility
#
# MARQUEE FORMATTING IN ACTION:
# - Title uses {#E69F00 ...} to color "high error rates" in orange
#   This MATCHES the orange bars in the chart!
# - Narrative uses {#E69F00 10 centers} to connect text to visual
# - **bold** highlights section headers

final_plot <- story_layout(
    plot = chart,

    # TITLE: State the insight, not just what the chart shows
    # Bad:  "Warehouse Error Rates"
    # Good: "Action needed: 10 warehouses have high error rates"
    #
    # {#E69F00 ...} colors the text to match the chart highlight
    title = "**Action needed: {#E69F00 10 warehouses} have high error rates**",

    # SUBTITLE: Context that helps interpretation
    subtitle = "DISCUSS: what are next steps to improve errors at highest volume warehouses?",

    # NARRATIVE: Explain what matters and what to do
    # Structure: SITUATION -> INSIGHT -> RECOMMENDATION
    #
    # Use {#E69F00 ...} to visually link text to chart elements
    narrative = "**OVERALL:** Error rate is 10%
across all 66 warehouses.
Accuracy rate is 85%.

**OPPORTUNITY:** {#E69F00 10 centers}
have error rates of 10%-16%.
These are our highest-volume sites.

**ACTION:** Prioritize process
improvements at WH-01 to WH-07.",

    # CAPTION: Source builds credibility
    caption = "SOURCE: ProTip Dashboard as of Q4/2021",

    # LAYOUT OPTIONS
    narrative_position = "right",
    narrative_width = 0.35,

    # SIZE OPTIONS (in points)
    title_size = 16,
    subtitle_size = 10,
    narrative_size = 9,

    # AUTO-HEIGHT: Automatically calculates title/subtitle heights
    # based on text length to prevent clipping. Set to FALSE and
    # specify title_height, subtitle_height manually for full control.
    auto_heights = TRUE,
    output_width = 12  # Match your ggsave width
)

# =============================================================================
# SAVE THE RESULT
# =============================================================================

ggsave(
    "swd_warehouse_analysis.png",
    final_plot,
    width = 12,
    height = 9,
    dpi = 150,
    bg = "white"
)

# =============================================================================
# CHECK YOUR LAYOUT WITH SITREP
# =============================================================================
#
# Before rendering, use story_sitrep() to preview the structure
# and get SWD-inspired tips:

story_sitrep(
    plot = chart,
    title = "**Action needed: 10 warehouses have {#E69F00 high error rates}**",
    subtitle = "DISCUSS: what are next steps...",
    narrative = "**OVERALL:** Error rate is 10%...",
    caption = "SOURCE: ProTip Dashboard",
    narrative_position = "right",
    narrative_width = 0.35,
    title_size = 16,
    show_tips = TRUE
)
