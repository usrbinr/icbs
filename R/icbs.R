
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
    
    pa=fig[[1]]
    pc=fig[[2]]
    pd=fig[[3]]
    
    pb <- ggplot() +
        # First segment: horizontal line at y = 0 (center of the graph)
        # This segment starts at x = -1 and ends at x = 0, creating a short horizontal line
        geom_segment(aes(y = 0, yend = 0, x = -1, xend = 0), col = "black") +
        
        # Second segment: vertical line from y = 3 to y = -3, passing through x = 0 (the center)
        # This line is placed vertically through the center of the plot and is long
        geom_segment(aes(y = 3, yend = -3, x = 0, xend = 0), col = "black") +
        
        # Third segment: top horizontal line starting at y = 3 (where the previous vertical line ends)
        # The line extends horizontally from x = 0 to x = 1, creating a short horizontal line at the top
        geom_segment(aes(y = 3, yend = 3, x = 0, xend = 1), col = "black") +
        
        # Fourth segment: bottom horizontal line starting at y = -3 (where the previous vertical line ends)
        # This line extends horizontally from x = 0 to x = 1, matching the length of the top horizontal segment
        geom_segment(aes(y = -3, yend = -3, x = 0, xend = 1), col = "black") +
        
        # Label: Place a label at the center of the plot (x = 0, y = 0)
        # The label uses the value of 'sign' (e.g., "+" or any other character or string)
        # The label's rounded corners (label.r) are set with a radius of 0.5 lines
        # The background fill of the label is white, and the text size is set to 10
        # The label is horizontally centered (hjust = 0.5) and vertically placed at the top (vjust = 1)
        geom_label(
            x = 0,                             # Label x position (centered horizontally)
            y = 0,                             # Label y position (centered vertically)
            label = sign,                      # The label text (passed from the 'sign' argument)
            label.r = unit(0.5, "lines"),      # Round the label corners with a radius of 0.5 lines
            fill = "white",                    # Set the label background color to white
            size = 10,                         # Set the label text size to 10
            # hjust = 0.5,                       # Horizontally center the label (align with x = 0)
            # vjust = 1,                         # Vertically align the label at the top (vjust = 1)
            data = tibble(x = "")              # Empty tibble for ggplot to process (this is a trick to add the label)
        ) +
        
        # Fix aspect ratio to make sure the x and y axes have the same scale
        coord_fixed() +
        
        # Remove all default ggplot themes (axes, gridlines, etc.) for a clean look
        theme_void()
    
    
    
    out <- pa + pb + pc + pd + plot_layout(design = layout)
    
    return(out)
    
}