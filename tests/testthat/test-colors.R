# Tests for color functions (highlight_colors, inline_legend, list_colors)

describe("highlight_colors", {
    it("highlights specified categories", {
        categories <- c("A", "B", "C", "D")
        result <- highlight_colors(categories, "B")

        expect_equal(names(result), categories)
        expect_equal(result[["B"]], "#1E90FF")  # default highlight color
        expect_equal(result[["A"]], "#D3D3D3")  # default gray
        expect_equal(result[["C"]], "#D3D3D3")
    })

    it("highlights multiple categories with custom colors", {
        categories <- c("A", "B", "C")
        result <- highlight_colors(categories, c("A" = "#FF0000", "C" = "#00FF00"))

        expect_equal(result[["A"]], "#FF0000")
        expect_equal(result[["B"]], "#D3D3D3")  # gray
        expect_equal(result[["C"]], "#00FF00")
    })

    it("uses custom highlight color for unnamed highlights", {
        categories <- c("X", "Y", "Z")
        result <- highlight_colors(categories, "Y", highlight_color = "#E69F00")

        expect_equal(result[["Y"]], "#E69F00")
        expect_equal(result[["X"]], "#D3D3D3")
    })

    it("uses custom gray color", {
        categories <- c("A", "B")
        result <- highlight_colors(categories, "A", gray_color = "#808080")

        expect_equal(result[["B"]], "#808080")
    })

    it("handles NULL highlight_color by using default", {
        categories <- c("A", "B")
        result <- highlight_colors(categories, "A", highlight_color = NULL)

        expect_equal(result[["A"]], "#1E90FF")
    })
})

describe("inline_legend", {
    it("creates horizontal legend data frame", {
        colors <- c("Apple" = "#FF0000", "Orange" = "#FFA500")
        result <- inline_legend(colors, x = 0.5, y = 0.5)

        expect_s3_class(result, "data.frame")
        expect_equal(nrow(result), 1)
        expect_equal(result$x, 0.5)
        expect_equal(result$y, 0.5)
        # Label should contain marquee syntax with colors
        expect_true(grepl("#FF0000", result$label))
        expect_true(grepl("#FFA500", result$label))
        expect_true(grepl("Apple", result$label))
    })

    it("creates vertical legend data frame", {
        colors <- c("A" = "#FF0000", "B" = "#00FF00")
        result <- inline_legend(colors, x = 1, y = 10, orientation = "vertical")

        expect_equal(nrow(result), 2)
        expect_equal(result$x, c(1, 1))
        expect_equal(result$y, c(10, 9))  # decreases by 1
    })

    it("applies bold formatting by default", {
        colors <- c("Test" = "#000000")
        result <- inline_legend(colors, bold = TRUE)

        expect_true(grepl("\\*\\*", result$label))  # ** for bold
    })

    it("respects bold = FALSE", {
        colors <- c("Test" = "#000000")
        result <- inline_legend(colors, bold = FALSE)

        expect_false(grepl("\\*\\*", result$label))
    })

    it("uses custom separator", {
        colors <- c("A" = "#FF0000", "B" = "#00FF00")
        result <- inline_legend(colors, sep = " - ")

        expect_true(grepl(" - ", result$label))
    })
})

describe("list_colors", {
    it("returns a data frame with name and hex columns", {
        result <- list_colors(n = 5)

        expect_s3_class(result, "data.frame")
        expect_true("name" %in% names(result))
        expect_true("hex" %in% names(result))
        expect_equal(nrow(result), 5)
    })

    it("filters by pattern", {
        result <- list_colors(pattern = "red", n = 20)

        expect_true(all(grepl("red", result$name, ignore.case = TRUE)))
    })

    it("returns empty data frame for no matches", {
        result <- list_colors(pattern = "zzzznonexistent")

        expect_equal(nrow(result), 0)
    })

    it("returns hex codes starting with #", {
        result <- list_colors(n = 5)

        expect_true(all(grepl("^#", result$hex)))
    })
})
