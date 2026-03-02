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

    it("highlight_colors returns named vector", {
        cats <- c("A", "B", "C")
        result <- highlight_colors(cats, "A")
        expect_named(result, cats)
        expect_equal(result["A"], c(A = "#1E90FF"))
    })

    it("inline_legend returns data frame", {
        colors <- c("X" = "#FF0000", "Y" = "#00FF00")
        result <- inline_legend(colors)
        expect_s3_class(result, "data.frame")
        expect_true("label" %in% names(result))
    })
})
