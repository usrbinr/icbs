# Warehouse fulfillment data from Albert Rapp's SWD Recreation
# https://albert-rapp.de/posts/ggplot2-tips/10_recreating_swd_look/10_recreating_swd_look.html

warehouse_data <- data.frame(
  id = 1:19,
  fulfilled = c(803, 865, 795, 683, 566, 586, 510, 436, 418, 364, 379, 372, 374, 278, 286, 327, 225, 222, 200),
  accuracy = c(0.86, 0.80, 0.84, 0.82, 0.86, 0.80, 0.80, 0.93, 0.88, 0.87, 0.85, 0.85, 0.83, 0.94, 0.86, 0.78, 0.89, 0.88, 0.91),
  error = c(0.10, 0.14, 0.10, 0.14, 0.10, 0.16, 0.15, 0.06, 0.11, 0.07, 0.12, 0.13, 0.08, 0.04, 0.12, 0.12, 0.07, 0.10, 0.07),
  null = c(0.04, 0.06, 0.06, 0.04, 0.04, 0.04, 0.05, 0.01, 0.01, 0.06, 0.03, 0.02, 0.09, 0.02, 0.02, 0.10, 0.04, 0.02, 0.02)
)
