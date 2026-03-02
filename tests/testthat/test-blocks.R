describe("block functions", {
    it("title_block returns a ggplot", {
        skip_if_not_installed("marquee")
        p <- title_block("Test Title")
        expect_s3_class(p, "ggplot")
    })

    it("subtitle_block returns a ggplot", {
        skip_if_not_installed("marquee")
        p <- subtitle_block("Test Subtitle")
        expect_s3_class(p, "ggplot")
    })

    it("caption_block returns a ggplot", {
        skip_if_not_installed("marquee")
        p <- caption_block("SOURCE: Test")
        expect_s3_class(p, "ggplot")
    })

    it("legend_block returns a ggplot", {
        skip_if_not_installed("marquee")
        colors <- c("A" = "#E63946", "B" = "#457B9D")
        p <- legend_block(colors)
        expect_s3_class(p, "ggplot")
    })

    it("text_narrative returns a ggplot", {
        skip_if_not_installed("marquee")
        p <- text_narrative("Test narrative text")
        expect_s3_class(p, "ggplot")
    })
})

describe("theme functions", {
    it("theme_stwd returns a theme", {
        th <- theme_stwd()
        expect_s3_class(th, "theme")
    })
})

describe("helper functions", {
    it("list_colors returns data frame", {
        result <- list_colors(n = 5)
        expect_s3_class(result, "data.frame")
        expect_true("name" %in% names(result))
        expect_true("hex" %in% names(result))
    })

    it("list_colors filters by pattern", {
        result <- list_colors(pattern = "^blue$", n = 10)
        expect_equal(nrow(result), 1)
        expect_equal(result$name, "blue")
    })

    it("highlight_colors returns named vector", {
        cats <- c("A", "B", "C")
        result <- highlight_colors(cats, "A")
        expect_named(result, cats)
        expect_equal(result["A"], c(A = "#1E90FF"))
    })

    it("highlight_colors uses custom highlight color", {
        cats <- c("A", "B", "C")
        result <- highlight_colors(cats, c("A" = "#FF0000"))
        expect_equal(result["A"], c(A = "#FF0000"))
    })

    it("highlight_colors uses custom gray color", {
        cats <- c("A", "B", "C")
        result <- highlight_colors(cats, "A", gray_color = "#CCCCCC")
        expect_equal(unname(result["B"]), "#CCCCCC")
        expect_equal(unname(result["C"]), "#CCCCCC")
    })

    it("inline_legend returns data frame", {
        colors <- c("X" = "#FF0000", "Y" = "#00FF00")
        result <- inline_legend(colors)
        expect_s3_class(result, "data.frame")
        expect_true("label" %in% names(result))
    })

    it("inline_legend formats labels with marquee syntax", {
        colors <- c("X" = "#FF0000")
        result <- inline_legend(colors)
        # Label should contain color code in marquee format
        expect_true(grepl("#FF0000", result$label))
        expect_true(grepl("X", result$label))
    })

    it("inline_legend supports vertical orientation", {
        colors <- c("X" = "#FF0000", "Y" = "#00FF00")
        result <- inline_legend(colors, orientation = "vertical")
        expect_equal(nrow(result), 2)
    })
})

describe("block function options", {
    it("title_block respects halign parameter", {
        skip_if_not_installed("marquee")
        p_left <- title_block("Test", halign = "left")
        p_right <- title_block("Test", halign = "right")
        expect_s3_class(p_left, "ggplot")
        expect_s3_class(p_right, "ggplot")
    })

    it("text_narrative respects valign parameter", {
        skip_if_not_installed("marquee")
        p_top <- text_narrative("Test", valign = "top")
        p_bottom <- text_narrative("Test", valign = "bottom")
        expect_s3_class(p_top, "ggplot")
        expect_s3_class(p_bottom, "ggplot")
    })

    it("caption_block applies default gray color", {
        skip_if_not_installed("marquee")
        p <- caption_block("SOURCE: Test")
        # Check that it builds without error
        expect_s3_class(p, "ggplot")
    })

    it("legend_block supports vertical orientation", {
        skip_if_not_installed("marquee")
        colors <- c("A" = "#E63946", "B" = "#457B9D")
        p <- legend_block(colors, orientation = "vertical")
        expect_s3_class(p, "ggplot")
    })

    it("legend_block supports uppercase labels", {
        skip_if_not_installed("marquee")
        colors <- c("test" = "#E63946")
        p <- legend_block(colors, uppercase = TRUE)
        expect_s3_class(p, "ggplot")
    })
})

describe("detect_plot_categories", {
    it("detects fill mapping from plot level", {
        df <- data.frame(cat = c("A", "B", "C"), val = 1:3)
        p <- ggplot2::ggplot(df, ggplot2::aes(cat, val, fill = cat)) +
            ggplot2::geom_col()
        result <- stwd:::detect_plot_categories(p)
        expect_equal(result$fill_levels, c("A", "B", "C"))
        expect_equal(result$n_fill, 3)
    })

    it("detects color mapping from plot level", {
        df <- data.frame(cat = c("A", "B"), x = 1:2, y = 1:2)
        p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = cat)) +
            ggplot2::geom_point()
        result <- stwd:::detect_plot_categories(p)
        expect_equal(result$color_levels, c("A", "B"))
        expect_equal(result$n_color, 2)
    })

    it("returns NULL for plots without fill/color mappings", {
        df <- data.frame(x = 1:3, y = 1:3)
        p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
            ggplot2::geom_point()
        result <- stwd:::detect_plot_categories(p)
        expect_null(result$fill_levels)
        expect_null(result$color_levels)
        expect_equal(result$n_fill, 0)
        expect_equal(result$n_color, 0)
    })
})
