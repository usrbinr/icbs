describe("marquee_color_labels", {
    it("formats labels with color codes", {
        labels <- c("Positive", "Negative")
        colors <- c("#006400", "#8B0000")
        result <- marquee_color_labels(labels, colors)

        expect_equal(result[1], "{#006400 Positive}")
        expect_equal(result[2], "{#8B0000 Negative}")
    })

    it("adds bold formatting when requested", {
        labels <- c("Test")
        colors <- c("red")
        result <- marquee_color_labels(labels, colors, bold = TRUE)

        expect_equal(result, "{red **Test**}")
    })

    it("adds italic formatting when requested", {
        labels <- c("Test")
        colors <- c("blue")
        result <- marquee_color_labels(labels, colors, italic = TRUE)

        expect_equal(result, "{blue *Test*}")
    })

    it("adds bold and italic formatting together", {
        labels <- c("Test")
        colors <- c("green")
        result <- marquee_color_labels(labels, colors, bold = TRUE, italic = TRUE)

        expect_equal(result, "{green ***Test***}")
    })

    it("handles multiple labels", {
        labels <- c("A", "B", "C")
        colors <- c("#FF0000", "#00FF00", "#0000FF")
        result <- marquee_color_labels(labels, colors)

        expect_length(result, 3)
        expect_equal(result[1], "{#FF0000 A}")
        expect_equal(result[2], "{#00FF00 B}")
        expect_equal(result[3], "{#0000FF C}")
    })

    it("errors when lengths don't match", {
        expect_error(
            marquee_color_labels(c("A", "B"), c("red")),
            "labels and colors must have the same length"
        )
    })

    it("handles named colors", {
        labels <- c("Good", "Bad")
        colors <- c("darkgreen", "darkred")
        result <- marquee_color_labels(labels, colors)

        expect_equal(result[1], "{darkgreen Good}")
        expect_equal(result[2], "{darkred Bad}")
    })
})

describe("geom_marquee_colored", {
    it("errors when marquee is not installed", {
        skip_if(requireNamespace("marquee", quietly = TRUE),
                "marquee is installed, skipping error test")

        df <- data.frame(x = 1, y = 1, label = "test", color = "red")
        expect_error(
            geom_marquee_colored(
                data = df,
                mapping = aes(x = x, y = y, label = label, color = color)
            ),
            "Package 'marquee' is required"
        )
    })

    it("creates a layer when marquee is available", {
        skip_if_not(requireNamespace("marquee", quietly = TRUE),
                    "marquee not installed")

        df <- data.frame(
            x = c("A", "B"),
            y = c(10, -5),
            text_color = c("#006400", "#8B0000"),
            label = c("+10", "-5")
        )

        layer <- geom_marquee_colored(
            data = df,
            mapping = aes(x = x, y = y, label = label, color = text_color)
        )

        expect_s3_class(layer, "LayerInstance")
    })

    it("errors when label aesthetic is missing", {
        skip_if_not(requireNamespace("marquee", quietly = TRUE),
                    "marquee not installed")

        df <- data.frame(x = 1, y = 1, color = "red")
        expect_error(
            geom_marquee_colored(
                data = df,
                mapping = aes(x = x, y = y, color = color)
            ),
            "mapping must include 'label' aesthetic"
        )
    })

    it("errors when color aesthetic is missing", {
        skip_if_not(requireNamespace("marquee", quietly = TRUE),
                    "marquee not installed")

        df <- data.frame(x = 1, y = 1, label = "test")
        expect_error(
            geom_marquee_colored(
                data = df,
                mapping = aes(x = x, y = y, label = label)
            ),
            "mapping must include 'color'"
        )
    })

    it("supports bold formatting", {
        skip_if_not(requireNamespace("marquee", quietly = TRUE),
                    "marquee not installed")

        df <- data.frame(
            x = "A", y = 10,
            text_color = "#006400",
            label = "+10"
        )

        layer <- geom_marquee_colored(
            data = df,
            mapping = aes(x = x, y = y, label = label, color = text_color),
            bold = TRUE
        )

        expect_s3_class(layer, "LayerInstance")
    })
})
