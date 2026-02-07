describe("scale_fill_ibcs_scenario", {
    it("returns a ggplot scale object", {
        sc <- scale_fill_ibcs_scenario()
        expect_s3_class(sc, "Scale")
    })

    it("works with ggplot", {
        df <- data.frame(
            scenario = c("AC", "PL", "PY", "FC"),
            value = c(100, 95, 90, 105)
        )
        p <- ggplot2::ggplot(df, ggplot2::aes(x = scenario, y = value, fill = scenario)) +
            ggplot2::geom_col() +
            scale_fill_ibcs_scenario()
        expect_s3_class(p, "ggplot")
    })
})

describe("scale_color_ibcs_scenario", {
    it("returns a ggplot scale object", {
        sc <- scale_color_ibcs_scenario()
        expect_s3_class(sc, "Scale")
    })
})

describe("scale_fill_ibcs_variance", {
    it("returns a ggplot scale object", {
        sc <- scale_fill_ibcs_variance()
        expect_s3_class(sc, "Scale")
    })

    it("works with variance data", {
        df <- data.frame(
            category = c("A", "B"),
            variance = c("positive", "negative"),
            value = c(10, -5)
        )
        p <- ggplot2::ggplot(df, ggplot2::aes(x = category, y = value, fill = variance)) +
            ggplot2::geom_col() +
            scale_fill_ibcs_variance()
        expect_s3_class(p, "ggplot")
    })
})
