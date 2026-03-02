# Tests for layout functions (story_layout, estimate_layout_heights)

describe("estimate_layout_heights", {
    it("returns a list with required components", {
        result <- estimate_layout_heights(
            title = "Test Title",
            subtitle = "Test Subtitle"
        )

        expect_type(result, "list")
        expect_true("title_height" %in% names(result))
        expect_true("subtitle_height" %in% names(result))
        expect_true("caption_height" %in% names(result))
        expect_true("content_height" %in% names(result))
        expect_true("narrative_width" %in% names(result))
    })

    it("heights sum to approximately 1", {
        result <- estimate_layout_heights(
            title = "Title",
            subtitle = "Subtitle",
            caption = "Source"
        )

        total <- result$title_height + result$subtitle_height +
                 result$caption_height + result$content_height
        expect_equal(total, 1, tolerance = 0.01)
    })

    it("returns 0 height for NULL title", {
        result <- estimate_layout_heights(title = NULL)

        expect_equal(result$title_height, 0)
    })

    it("returns 0 height for NULL subtitle", {
        result <- estimate_layout_heights(subtitle = NULL)

        expect_equal(result$subtitle_height, 0)
    })

    it("gives larger height for longer title", {
        short <- estimate_layout_heights(title = "Short")
        long <- estimate_layout_heights(
            title = "This is a much longer title that should require more height to display properly"
        )

        expect_true(long$title_height >= short$title_height)
    })

    it("gives larger height for larger font size", {
        small <- estimate_layout_heights(title = "Test", title_size = 10)
        large <- estimate_layout_heights(title = "Test", title_size = 20)

        expect_true(large$title_height >= small$title_height)
    })

    it("strips marquee formatting when calculating height", {
        plain <- estimate_layout_heights(title = "Plain text")
        formatted <- estimate_layout_heights(title = "**Bold** {#FF0000 colored}")

        # Both should have similar heights since formatting is stripped
        expect_equal(plain$title_height, formatted$title_height, tolerance = 0.05)
    })

    it("echoes narrative_width parameter", {
        result <- estimate_layout_heights(narrative_width = 0.4)

        expect_equal(result$narrative_width, 0.4)
    })
})

describe("story_layout", {
    skip_if_not_installed("marquee")

    # Create a simple test plot
    test_plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3)) +
        ggplot2::geom_point(ggplot2::aes(x, y))

    it("returns a patchwork object", {
        result <- story_layout(
            plot = test_plot,
            title = "Test Title"
        )

        expect_s3_class(result, "patchwork")
    })

    it("works with title only", {
        result <- story_layout(
            plot = test_plot,
            title = "Just a Title"
        )

        expect_s3_class(result, "patchwork")
    })

    it("works with all text components", {
        result <- story_layout(
            plot = test_plot,
            title = "Title",
            subtitle = "Subtitle",
            narrative = "Some narrative text",
            caption = "Source"
        )

        expect_s3_class(result, "patchwork")
    })

    it("accepts pre-built block objects", {
        title_block_obj <- title_block("Pre-built Title")

        result <- story_layout(
            plot = test_plot,
            title = title_block_obj
        )

        expect_s3_class(result, "patchwork")
    })

    it("respects narrative_position = left", {
        result <- story_layout(
            plot = test_plot,
            title = "Title",
            narrative = "Narrative",
            narrative_position = "left"
        )

        expect_s3_class(result, "patchwork")
    })

    it("respects narrative_position = bottom", {
        result <- story_layout(
            plot = test_plot,
            title = "Title",
            narrative = "Narrative",
            narrative_position = "bottom"
        )

        expect_s3_class(result, "patchwork")
    })

    it("works with auto_heights = FALSE", {
        result <- story_layout(
            plot = test_plot,
            title = "Title",
            auto_heights = FALSE,
            title_height = 0.1
        )

        expect_s3_class(result, "patchwork")
    })
})
