# Tests for code generation functions
# These functions generate R code strings for story_designer's Code tab

describe("generate_theme_code", {
    
    it("generates parseable R code", {
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

        result <- generate_theme_code(mock_input)

        expect_type(result, "character")
        # Should be parseable R code
        expect_no_error(parse(text = result))
    })

    it("includes theme_stwd for stwd theme", {
        mock_input <- list(plot_theme = "stwd")
        result <- generate_theme_code(mock_input)

        expect_true(grepl("theme_stwd", result))
    })

    it("includes theme_void for void theme", {
        mock_input <- list(plot_theme = "void")
        result <- generate_theme_code(mock_input)

        expect_true(grepl("theme_void", result))
    })
})

describe("generate_grid_code", {
    
    it("returns element_blank for remove_all = TRUE", {
        mock_input <- list(grid_remove_all = TRUE)
        result <- generate_grid_code(mock_input)

        expect_true(grepl("element_blank", result))
    })

    it("handles horizontal-only major grid", {
        mock_input <- list(
            grid_remove_all = FALSE,
            grid_major = "h",
            grid_minor = "none",
            grid_color = "#E5E5E5"
        )
        result <- generate_grid_code(mock_input)

        expect_true(grepl("panel.grid.major.y", result))
        expect_true(grepl("panel.grid.major.x = element_blank", result))
    })
})

describe("generate_palette_code", {
    it("returns empty string for 'none' package", {
        result <- generate_palette_code("none", "anything", "fill", "discrete")

        expect_equal(result, "")
    })

    it("generates scale_fill_manual for discrete fill", {
        result <- generate_palette_code("viridis", "viridis", "fill", "discrete")

        expect_true(grepl("scale_fill_manual", result))
    })

    it("generates scale_color_manual for discrete color", {
        result <- generate_palette_code("viridis", "viridis", "color", "discrete")

        expect_true(grepl("scale_color_manual", result))
    })

    it("generates both scales for 'both' apply_to", {
        result <- generate_palette_code("viridis", "viridis", "both", "discrete")

        expect_true(grepl("scale_fill_manual", result))
        expect_true(grepl("scale_color_manual", result))
    })

    it("generates gradientn for continuous scales", {
        result <- generate_palette_code("viridis", "viridis", "fill", "continuous")

        expect_true(grepl("scale_fill_gradientn", result))
    })

    it("includes correct palette function for each package", {
        packages <- c("ggsci", "MetBrewer", "nord", "viridis", "wesanderson")
        for (pkg in packages) {
            result <- generate_palette_code(pkg, "test", "fill", "discrete")
            expect_true(grepl(pkg, result), info = paste("Failed for package:", pkg))
        }
    })
})

describe("generate_block_code", {
    it("generates title_block code for title type", {
        result <- generate_block_code(
            type = "title",
            text = "My Title",
            size = 14,
            halign = "left",
            lineheight = 1.1
        )

        expect_true(grepl("title_block", result))
        expect_true(grepl("My Title", result))
        expect_true(grepl("title_size = 14", result))
    })

    it("generates text_narrative code for narrative type", {
        result <- generate_block_code(
            type = "narrative",
            text = "My Narrative",
            size = 10,
            halign = "left",
            lineheight = 1.4
        )

        expect_true(grepl("text_narrative", result))
        expect_true(grepl("size = 10", result))
    })

    it("includes wrap_width when specified", {
        result <- generate_block_code(
            type = "title",
            text = "Title",
            size = 12,
            halign = "left",
            lineheight = 1.1,
            wrap_width = 40
        )

        expect_true(grepl("wrap_width = 40", result))
    })

    it("excludes wrap_width when 0", {
        result <- generate_block_code(
            type = "title",
            text = "Title",
            size = 12,
            halign = "left",
            lineheight = 1.1,
            wrap_width = 0
        )

        expect_false(grepl("wrap_width", result))
    })
})

describe("generate_legend_code", {
    it("generates legend_block code", {
        result <- generate_legend_code(
            labels = c("A", "B"),
            colors = c("#FF0000", "#00FF00"),
            halign = "right",
            orientation = "horizontal",
            sep = " | ",
            size = 10,
            bold = TRUE,
            uppercase = FALSE
        )

        expect_true(grepl("legend_block", result))
        expect_true(grepl("#FF0000", result))
        expect_true(grepl("bold = TRUE", result))
    })

    it("includes lineheight for vertical orientation", {
        result <- generate_legend_code(
            labels = c("A"),
            colors = c("#FF0000"),
            halign = "right",
            orientation = "vertical",
            sep = " | ",
            size = 10,
            bold = TRUE,
            uppercase = FALSE,
            lineheight = 1.6
        )

        expect_true(grepl("lineheight = 1.6", result))
    })
})

describe("generate_composition_code", {
    it("generates patchwork composition code", {
        heights <- list(title = 0.1, subtitle = 0.06, legend = 0.04, caption = 0.05)

        result <- generate_composition_code(
            heights = heights,
            narrative_width = 0.35,
            legend_enabled = FALSE
        )

        expect_true(grepl("plot_layout", result))
        expect_true(grepl("final <-", result))
    })

    it("includes legend in composition when enabled", {
        heights <- list(title = 0.1, subtitle = 0.06, legend = 0.04, caption = 0.05)

        result <- generate_composition_code(
            heights = heights,
            narrative_width = 0.35,
            legend_enabled = TRUE,
            legend_pos = "above"
        )

        expect_true(grepl("legend_plot", result))
    })

    it("handles different legend positions", {
        heights <- list(title = 0.1, subtitle = 0.06, legend = 0.04, caption = 0.05)

        for (pos in c("above", "below", "left", "right")) {
            result <- generate_composition_code(
                heights = heights,
                narrative_width = 0.35,
                legend_enabled = TRUE,
                legend_pos = pos
            )
            expect_type(result, "character")
        }
    })
})
