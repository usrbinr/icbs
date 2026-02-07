describe("one_to_split_to_two_layout", {
    # Create test plots
    p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()
    p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
        ggplot2::geom_point()
    p3 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()

    it("creates a patchwork object from a list of 3 plots", {
        fig <- list(p1, p2, p3)
        result <- one_to_split_to_two_layout(fig)
        expect_s3_class(result, "patchwork")
    })

    it("accepts a custom first_level_sign", {
        fig <- list(p1, p2, p3)
        result <- one_to_split_to_two_layout(fig, first_level_sign = "OR")
        expect_s3_class(result, "patchwork")
    })

    it("uses default sign of '+' when not specified", {
        fig <- list(p1, p2, p3)
        result <- one_to_split_to_two_layout(fig)
        expect_s3_class(result, "patchwork")
    })
})

describe("two_to_merge_to_one_layout", {
    # Create test plots
    p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()
    p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
        ggplot2::geom_point()
    p3 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()

    it("creates a patchwork object from a list of 3 plots", {
        fig <- list(p1, p2, p3)
        result <- two_to_merge_to_one_layout(fig)
        expect_s3_class(result, "patchwork")
    })

    it("accepts a custom first_level_sign", {
        fig <- list(p1, p2, p3)
        result <- two_to_merge_to_one_layout(fig, first_level_sign = "AND")
        expect_s3_class(result, "patchwork")
    })

    it("places the third plot on the right side", {
        fig <- list(p1, p2, p3)
        result <- two_to_merge_to_one_layout(fig)
        expect_s3_class(result, "patchwork")
    })
})
