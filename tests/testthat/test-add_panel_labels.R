describe("add_panel_labels", {
    # Create test patchwork plot
    p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()
    p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
        ggplot2::geom_point()
    combined <- p1 + p2

    it("returns a patchwork object", {
        result <- add_panel_labels(combined)
        expect_s3_class(result, "patchwork")
    })

    it("applies uppercase labels by default with style='upper'", {
        result <- add_panel_labels(combined, style = "upper")
        expect_s3_class(result, "patchwork")
    })

    it("applies lowercase labels with style='lower'", {
        result <- add_panel_labels(combined, style = "lower")
        expect_s3_class(result, "patchwork")
    })

    it("accepts custom labels as a character vector", {
        result <- add_panel_labels(combined, labels = c("I", "II"))
        expect_s3_class(result, "patchwork")
    })

    it("accepts custom fontface parameter", {
        result <- add_panel_labels(combined, fontface = "italic")
        expect_s3_class(result, "patchwork")
    })

    it("accepts custom size parameter", {
        result <- add_panel_labels(combined, size = 20)
        expect_s3_class(result, "patchwork")
    })

    it("accepts custom position parameters", {
        result <- add_panel_labels(
            combined,
            x = 0.5,
            y = 0.5,
            hjust = 0,
            vjust = 0
        )
        expect_s3_class(result, "patchwork")
    })

    it("errors on invalid style without labels", {
        expect_error(
            add_panel_labels(combined, style = "custom"),
            "style must be 'upper', 'lower', or provide labels directly"
        )
    })
})
