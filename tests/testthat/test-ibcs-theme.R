describe("ibcs_colors", {
    it("returns a list of color values", {
        cols <- ibcs_colors()
        expect_type(cols, "list")
        expect_true("actual" %in% names(cols))
        expect_true("plan" %in% names(cols))
        expect_true("variance_positive" %in% names(cols))
        expect_true("variance_negative" %in% names(cols))
    })

    it("returns valid hex color values", {
        cols <- ibcs_colors()
        expect_match(cols$actual, "^#[0-9A-Fa-f]{6}$")
        expect_match(cols$variance_positive, "^#[0-9A-Fa-f]{6}$")
    })
})

describe("theme_ibcs", {
    it("returns a ggplot theme object", {
        thm <- theme_ibcs()
        expect_s3_class(thm, "theme")
    })

    it("accepts custom base_size", {
        thm <- theme_ibcs(base_size = 14)
        expect_s3_class(thm, "theme")
    })

    it("can toggle gridlines", {
        thm_no_grid <- theme_ibcs(grid_x = FALSE, grid_y = FALSE)
        expect_s3_class(thm_no_grid, "theme")

        thm_grid <- theme_ibcs(grid_x = TRUE, grid_y = TRUE)
        expect_s3_class(thm_grid, "theme")
    })

    it("works with ggplot", {
        p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) +
            ggplot2::geom_col() +
            theme_ibcs()
        expect_s3_class(p, "ggplot")
    })
})
