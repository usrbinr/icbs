# Create icbs hex logo
library(ggplot2)
library(hexSticker)

# Create an IBCS-style variance chart subplot
# Using IBCS colors: gray for totals, green/red for variance
bar_data <- data.frame(
  x = 1:5,
  y = c(100, -30, -20, 30, 80),
  type = c("total", "negative", "negative", "positive", "total")
)

# Calculate waterfall positions
bar_data$yend <- cumsum(c(bar_data$y[1], bar_data$y[-1]))
bar_data$ystart <- c(0, bar_data$yend[-length(bar_data$yend)])

# IBCS colors
ibcs_gray <- "#404040"
ibcs_green <- "#006400"  # Dark green for positive
ibcs_red <- "#8B0000"    # Dark red for negative

bar_data$fill <- ifelse(bar_data$type == "total", ibcs_gray,
                        ifelse(bar_data$type == "positive", ibcs_green, ibcs_red))

# Create minimalist bar chart
p <- ggplot(bar_data) +
  geom_rect(
    aes(
      xmin = x - 0.35,
      xmax = x + 0.35,
      ymin = pmin(ystart, yend),
      ymax = pmax(ystart, yend)
    ),
    fill = bar_data$fill,
    color = "#262626",
    linewidth = 0.3
  ) +
  # Add connector lines
  annotate("segment", x = 1.35, xend = 1.65, y = 100, yend = 100,
           color = "#808080", linetype = "dotted", linewidth = 0.4) +
  annotate("segment", x = 2.35, xend = 2.65, y = 70, yend = 70,
           color = "#808080", linetype = "dotted", linewidth = 0.4) +
  annotate("segment", x = 3.35, xend = 3.65, y = 50, yend = 50,
           color = "#808080", linetype = "dotted", linewidth = 0.4) +
  annotate("segment", x = 4.35, xend = 4.65, y = 80, yend = 80,
           color = "#808080", linetype = "dotted", linewidth = 0.4) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

# Create the hex sticker with light background and warm accent
sticker(
  p,
  package = "icbs",
  p_size = 22,
  p_y = 1.45,
  p_color = "#2d2d2d",
  p_family = "sans",
  s_x = 1,
  s_y = 0.72,
  s_width = 1.4,
  s_height = 0.85,
  h_fill = "#fafafa",
  h_color = "#2a9d8f",
  h_size = 1.4,
  filename = "inst/logo/logo.png",
  dpi = 300
)

# Also save as SVG
sticker(
  p,
  package = "icbs",
  p_size = 22,
  p_y = 1.45,
  p_color = "#2d2d2d",
  p_family = "sans",
  s_x = 1,
  s_y = 0.72,
  s_width = 1.4,
  s_height = 0.85,
  h_fill = "#fafafa",
  h_color = "#2a9d8f",
  h_size = 1.4,
  filename = "inst/logo/logo.svg"
)

message("Logo created at inst/logo/logo.png and inst/logo/logo.svg")
