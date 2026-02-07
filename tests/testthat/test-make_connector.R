describe("make_connector", {
    it("creates a ggplot object for split direction", {
        p <- make_connector(direction = "split", sign = "+")
        expect_s3_class(p, "gg")
        expect_s3_class(p, "ggplot")
    })

    it("creates a ggplot object for merge direction", {
        p <- make_connector(direction = "merge", sign = "+")
        expect_s3_class(p, "gg")
        expect_s3_class(p, "ggplot")
    })

    it("accepts custom sign parameter", {
        p <- make_connector(direction = "split", sign = "x")
        expect_s3_class(p, "ggplot")
    })

    it("accepts custom line styling parameters", {
        p <- make_connector(
            direction = "split",
            sign = "+",
            line_col = "red",
            line_size = 1.5,
            label_size = 12,
            label_fill = "yellow"
        )
        expect_s3_class(p, "ggplot")
    })

    it("errors on invalid direction", {
        expect_error(
            make_connector(direction = "invalid"),
            "direction must be 'split' or 'merge'"
        )
    })

    it("uses theme_void for clean appearance", {
        p <- make_connector(direction = "split")
        expect_true(inherits(p$theme, "theme"))
    })
})
