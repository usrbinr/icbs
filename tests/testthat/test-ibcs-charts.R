describe("ibcs_variance_chart", {
    df <- data.frame(
        category = c("Product A", "Product B", "Product C", "Product D"),
        actual = c(120, 85, 95, 110),
        plan = c(100, 90, 100, 100)
    )

    it("creates a ggplot object", {
        p <- ibcs_variance_chart(df, x = category, actual = actual, reference = plan)
        expect_s3_class(p, "ggplot")
    })

    it("can hide variance bars", {
        p <- ibcs_variance_chart(
            df,
            x = category,
            actual = actual,
            reference = plan,
            show_variance_bars = FALSE
        )
        expect_s3_class(p, "ggplot")
    })

    it("can hide variance percentages", {
        p <- ibcs_variance_chart(
            df,
            x = category,
            actual = actual,
            reference = plan,
            show_variance_pct = FALSE
        )
        expect_s3_class(p, "ggplot")
    })

    it("supports horizontal orientation", {
        p <- ibcs_variance_chart(
            df,
            x = category,
            actual = actual,
            reference = plan,
            horizontal = TRUE
        )
        expect_s3_class(p, "ggplot")
    })

    it("can sort by actual values", {
        p <- ibcs_variance_chart(
            df,
            x = category,
            actual = actual,
            reference = plan,
            sort_by = "actual"
        )
        expect_s3_class(p, "ggplot")
    })

    it("can sort by variance", {
        p <- ibcs_variance_chart(
            df,
            x = category,
            actual = actual,
            reference = plan,
            sort_by = "variance"
        )
        expect_s3_class(p, "ggplot")
    })
})

describe("ibcs_waterfall", {
    df <- data.frame(
        category = c("Revenue", "COGS", "Gross Profit", "OpEx", "Net Income"),
        value = c(1000, -400, NA, -350, NA),
        type = c("start", "negative", "subtotal", "negative", "end")
    )

    it("creates a ggplot object with type column", {
        p <- ibcs_waterfall(df, x = category, y = value, type = type)
        expect_s3_class(p, "ggplot")
    })

    it("creates a ggplot object inferring types", {
        df2 <- data.frame(
            category = c("Start", "Add", "Subtract", "End"),
            value = c(100, 50, -30, 120)
        )
        p <- ibcs_waterfall(df2, x = category, y = value)
        expect_s3_class(p, "ggplot")
    })

    it("can hide connectors", {
        p <- ibcs_waterfall(
            df,
            x = category,
            y = value,
            type = type,
            show_connectors = FALSE
        )
        expect_s3_class(p, "ggplot")
    })

    it("can hide value labels", {
        p <- ibcs_waterfall(
            df,
            x = category,
            y = value,
            type = type,
            show_values = FALSE
        )
        expect_s3_class(p, "ggplot")
    })

    it("accepts custom fill colors", {
        p <- ibcs_waterfall(
            df,
            x = category,
            y = value,
            type = type,
            fill_positive = "blue",
            fill_negative = "orange",
            fill_total = "black"
        )
        expect_s3_class(p, "ggplot")
    })
})

describe("ibcs_bar_chart", {
    df <- data.frame(
        month = rep(c("Jan", "Feb", "Mar"), each = 2),
        scenario = rep(c("AC", "PL"), 3),
        value = c(100, 95, 110, 105, 120, 115)
    )

    it("creates a ggplot object", {
        p <- ibcs_bar_chart(df, x = month, y = value, scenario = scenario)
        expect_s3_class(p, "ggplot")
    })

    it("accepts custom width", {
        p <- ibcs_bar_chart(df, x = month, y = value, scenario = scenario, width = 0.5)
        expect_s3_class(p, "ggplot")
    })

    it("handles all scenario types", {
        df2 <- data.frame(
            month = rep("Jan", 4),
            scenario = c("AC", "PL", "PY", "FC"),
            value = c(100, 95, 90, 105)
        )
        p <- ibcs_bar_chart(df2, x = month, y = value, scenario = scenario)
        expect_s3_class(p, "ggplot")
    })
})
