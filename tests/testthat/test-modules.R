# Tests for Shiny modules
# These test the module helper functions and logic without running the full app

describe("mod_palette helpers", {
    it("get_palette_names returns character vector for valid packages", {
        # viridis is built-in, should always work
        result <- get_palette_names("viridis")
        expect_type(result, "character")
        expect_true(length(result) > 0)
        expect_true("viridis" %in% result)
    })

    it("get_palette_names returns empty for 'none'", {
        result <- get_palette_names("none")
        expect_equal(length(result), 0)
    })

    it("get_palette_names returns empty for unavailable packages", {
        result <- get_palette_names("nonexistent_package")
        expect_equal(length(result), 0)
    })

    it("get_palette_colors returns colors for viridis", {
        result <- get_palette_colors("viridis", "viridis", 5)
        expect_type(result, "character")
        expect_equal(length(result), 5)
        # Should be hex colors
        expect_true(all(grepl("^#", result)))
    })

    it("get_palette_colors handles errors gracefully", {
        # Invalid palette should return fallback colors
        result <- get_palette_colors("viridis", "nonexistent", 5)
        expect_type(result, "character")
        expect_equal(length(result), 5)
    })
})

describe("detect_plot_categories", {
    it("detects fill mapping", {
        df <- data.frame(cat = c("A", "B", "C"), val = 1:3)
        p <- ggplot2::ggplot(df, ggplot2::aes(cat, val, fill = cat)) +
            ggplot2::geom_col()

        result <- detect_plot_categories(p)

        expect_type(result, "list")
        expect_equal(result$fill_levels, c("A", "B", "C"))
        expect_equal(result$n_fill, 3)
    })

    it("detects color mapping", {
        df <- data.frame(cat = c("X", "Y"), x = 1:2, y = 1:2)
        p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = cat)) +
            ggplot2::geom_point()

        result <- detect_plot_categories(p)

        expect_equal(result$color_levels, c("X", "Y"))
        expect_equal(result$n_color, 2)
    })

    it("handles plots without mappings", {
        df <- data.frame(x = 1:3, y = 1:3)
        p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
            ggplot2::geom_point()

        result <- detect_plot_categories(p)

        expect_null(result$fill_levels)
        expect_null(result$color_levels)
        expect_equal(result$n_fill, 0)
        expect_equal(result$n_color, 0)
    })

    it("detects factor levels in order", {
        df <- data.frame(
            cat = factor(c("C", "A", "B"), levels = c("A", "B", "C")),
            val = 1:3
        )
        p <- ggplot2::ggplot(df, ggplot2::aes(cat, val, fill = cat)) +
            ggplot2::geom_col()

        result <- detect_plot_categories(p)

        # Should preserve factor level order
        expect_equal(result$fill_levels, c("A", "B", "C"))
    })
})

describe("convert_named_colors", {
    it("converts named colors to hex", {
        result <- convert_named_colors("{red text}")
        expect_true(grepl("#FF0000", result, ignore.case = TRUE))
    })

    it("preserves hex colors", {
        result <- convert_named_colors("{#E69F00 text}")
        expect_equal(result, "{#E69F00 text}")
    })

    it("handles text without colors", {
        result <- convert_named_colors("plain text")
        expect_equal(result, "plain text")
    })

    it("handles empty input", {
        expect_equal(convert_named_colors(""), "")
        expect_null(convert_named_colors(NULL))
    })

    it("converts multiple named colors", {
        result <- convert_named_colors("{red A} and {blue B}")
        expect_true(grepl("#FF0000", result, ignore.case = TRUE))
        expect_true(grepl("#0000FF", result, ignore.case = TRUE))
    })
})

describe("default_legend_colors", {
    it("has 10 colors", {
        expect_equal(length(default_legend_colors), 10)
    })

    it("all are hex colors", {
        expect_true(all(grepl("^#[A-Fa-f0-9]{6}$", default_legend_colors)))
    })
})

describe("compose_layout helper", {
    it("returns a patchwork object", {
        skip_if_not_installed("marquee")

        title_plot <- title_block("Test")
        subtitle_plot <- subtitle_block("Sub")
        content <- ggplot2::ggplot() + ggplot2::theme_void()
        caption_plot <- caption_block("Source")

        result <- compose_layout(
            title_plot, subtitle_plot, content, caption_plot,
            legend_plot = NULL, legend_pos = "above", legend_width = 0.12,
            h_title = 0.1, h_subtitle = 0.05, content_height = 0.8,
            h_legend = 0, h_caption = 0.05
        )

        expect_s3_class(result, "patchwork")
    })

    it("includes legend when provided", {
        skip_if_not_installed("marquee")

        title_plot <- title_block("Test")
        subtitle_plot <- subtitle_block("Sub")
        content <- ggplot2::ggplot() + ggplot2::theme_void()
        caption_plot <- caption_block("Source")
        legend_plot <- legend_block(c("A" = "#FF0000", "B" = "#00FF00"))

        result <- compose_layout(
            title_plot, subtitle_plot, content, caption_plot,
            legend_plot = legend_plot, legend_pos = "above", legend_width = 0.12,
            h_title = 0.1, h_subtitle = 0.05, content_height = 0.75,
            h_legend = 0.05, h_caption = 0.05
        )

        expect_s3_class(result, "patchwork")
    })
})

describe("generate_theme_code", {
    it("returns character string", {
        # Mock input object
        mock_input <- list(
            plot_theme = "stwd",
            axis_title_x_bold = FALSE,
            axis_title_x_size = 11,
            axis_title_x_align = "0.5",
            axis_title_x_angle = "0",
            axis_title_x_color = "#333333",
            axis_title_x_margin = 5,
            axis_title_y_bold = FALSE,
            axis_title_y_size = 11,
            axis_title_y_align = "0.5",
            axis_title_y_angle = "90",
            axis_title_y_color = "#333333",
            axis_title_y_margin = 5,
            axis_text_size = 10,
            axis_text_color = "#666666",
            plot_legend_pos = "right",
            show_axis_line = FALSE,
            show_ticks = FALSE,
            grid_remove_all = FALSE,
            grid_major = "both",
            grid_minor = "none",
            grid_color = "#E5E5E5"
        )

        `%||%` <- function(x, y) if (is.null(x)) y else x
        result <- generate_theme_code(mock_input, `%||%`)

        expect_type(result, "character")
        expect_true(grepl("theme_stwd", result))
    })
})
