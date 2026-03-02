# Module: Export and Validation
# Handles export settings, actual size validation, and code generation

#' Export Module UI
#' @param id Module namespace ID
#' @noRd
mod_export_ui <- function(id) {
    ns <- shiny::NS(id)

    bslib::card_body(
        class = "p-3",
        shiny::div(
            class = "row g-3 mb-3",
            shiny::div(
                class = "col-auto",
                shiny::selectInput(ns("format"), "Format", width = "100px",
                    choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                    selected = "png")
            ),
            shiny::div(
                class = "col-auto",
                shiny::numericInput(ns("width"), "Width (in)", value = 12,
                    min = 4, max = 24, step = 0.5, width = "100px")
            ),
            shiny::div(
                class = "col-auto",
                shiny::numericInput(ns("height"), "Height (in)", value = 9,
                    min = 3, max = 18, step = 0.5, width = "100px")
            ),
            shiny::div(
                class = "col-auto",
                shiny::numericInput(ns("dpi"), "DPI", value = 150,
                    min = 72, max = 600, step = 10, width = "80px")
            )
        ),
        shiny::div(
            class = "d-flex gap-2 mb-3",
            shiny::actionButton(ns("validate"), "Validate Actual Size",
                class = "btn-success", icon = shiny::icon("search-plus")),
            shiny::actionButton(ns("popout"), "Pop Out",
                class = "btn-outline-secondary", icon = shiny::icon("external-link-alt")),
            shiny::downloadButton(ns("download"), "Download", class = "btn-primary")
        ),
        shiny::uiOutput(ns("info")),
        shiny::div(
            class = "bg-light p-2 rounded mb-3",
            shiny::div(
                class = "d-flex justify-content-between align-items-center mb-1",
                shiny::tags$small(class = "text-muted fw-bold", "Quarto code chunk options:"),
                shiny::tags$button(
                    id = ns("copy_quarto"),
                    class = "btn btn-sm btn-outline-secondary py-0",
                    onclick = paste0(
                        "navigator.clipboard.writeText(document.getElementById('",
                        ns("quarto_text"), "').innerText).then(function() { ",
                        "document.getElementById('", ns("copy_quarto"), "').innerHTML = '<i class=\"fa fa-check\"></i> Copied'; ",
                        "setTimeout(function() { document.getElementById('", ns("copy_quarto"),
                        "').innerHTML = '<i class=\"fa fa-copy\"></i> Copy'; }, 1500); });"
                    ),
                    shiny::icon("copy"), " Copy"
                )
            ),
            shiny::uiOutput(ns("quarto_opts"))
        ),
        shiny::uiOutput(ns("actual_size_preview"))
    )
}

#' Export Module Server
#' @param id Module namespace ID
#' @param build_layout Reactive returning the composed patchwork layout
#' @return List with export dimensions reactives
#' @noRd
mod_export_server <- function(id, build_layout) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        `%||%` <- function(x, y) if (is.null(x)) y else x

        # Actual size preview data
        actual_size_data <- shiny::reactiveVal(NULL)

        # Export info display
        output$info <- shiny::renderUI({
            w <- input$width %||% 12
            h <- input$height %||% 9
            dpi <- input$dpi %||% 150
            fmt <- toupper(input$format %||% "png")

            shiny::div(
                class = "text-muted small mb-2",
                shiny::strong(fmt), " output: ", w, '" x ', h, '" @ ', dpi, ' DPI = ',
                shiny::strong(round(w * dpi), " x ", round(h * dpi), " px")
            )
        })

        # Quarto chunk options
        output$quarto_opts <- shiny::renderUI({
            w <- input$width %||% 12
            h <- input$height %||% 9
            dpi <- input$dpi %||% 150

            shiny::tags$pre(
                id = ns("quarto_text"),
                class = "mb-0 small",
                style = "background: white; padding: 8px; border-radius: 4px; font-family: monospace;",
                paste0("#| fig-width: ", w, "\n#| fig-height: ", h, "\n#| fig-dpi: ", dpi)
            )
        })

        # Validate actual size
        shiny::observeEvent(input$validate, {
            shiny::validate(
                shiny::need(build_layout, "Layout not ready - check your inputs")
            )

            shiny::withProgress(message = "Rendering at actual size...", {
                w <- input$width %||% 12
                h <- input$height %||% 9
                dpi <- input$dpi %||% 150

                temp_file <- tempfile(fileext = ".png")
                tryCatch({
                    ggplot2::ggsave(temp_file, build_layout(),
                        width = w, height = h, dpi = dpi, bg = "white")
                    img_data <- base64enc::base64encode(temp_file)
                    actual_size_data(list(
                        data = img_data,
                        px_width = w * dpi,
                        px_height = h * dpi
                    ))
                }, error = function(e) {
                    shiny::showNotification(
                        paste("Error rendering:", e$message),
                        type = "error"
                    )
                }, finally = {
                    unlink(temp_file)
                })
            })
        })

        # Actual size preview UI
        output$actual_size_preview <- shiny::renderUI({
            img_info <- actual_size_data()

            if (is.null(img_info)) {
                return(shiny::div(
                    class = "mt-3 p-4 text-center text-muted border rounded bg-light",
                    shiny::icon("search-plus", class = "fa-2x mb-2"),
                    shiny::p("Click 'Validate Actual Size' to preview at true export dimensions")
                ))
            }

            shiny::div(
                class = "mt-3 border rounded",
                shiny::div(
                    class = "bg-success text-white p-2 d-flex justify-content-between align-items-center",
                    shiny::span(
                        shiny::icon("check-circle"), " Actual Size ",
                        shiny::span(class = "badge bg-light text-dark",
                            paste0(img_info$px_width, "x", img_info$px_height, "px"))
                    ),
                    shiny::actionButton(ns("close_actual"), "Close", class = "btn-sm btn-light")
                ),
                shiny::div(
                    class = "p-1 text-center small text-muted bg-light",
                    "This is EXACTLY what your export will look like. Scroll to inspect."
                ),
                shiny::div(
                    style = "max-height: 600px; overflow: auto; border: 2px solid #28a745;",
                    shiny::tags$img(
                        src = paste0("data:image/png;base64,", img_info$data),
                        style = "max-width: none;"
                    )
                )
            )
        })

        # Close actual size preview
        shiny::observeEvent(input$close_actual, {
            actual_size_data(NULL)
        })

        # Pop out preview
        shiny::observeEvent(input$popout, {
            img_info <- actual_size_data()

            if (is.null(img_info)) {
                # Generate if not already done
                shiny::withProgress(message = "Rendering at actual size...", {
                    w <- input$width %||% 12
                    h <- input$height %||% 9
                    dpi <- input$dpi %||% 150

                    temp_file <- tempfile(fileext = ".png")
                    tryCatch({
                        ggplot2::ggsave(temp_file, build_layout(),
                            width = w, height = h, dpi = dpi, bg = "white")
                        img_data <- base64enc::base64encode(temp_file)
                        actual_size_data(list(
                            data = img_data,
                            px_width = w * dpi,
                            px_height = h * dpi
                        ))
                    }, finally = {
                        unlink(temp_file)
                    })
                })
                img_info <- actual_size_data()
            }

            shiny::showModal(shiny::modalDialog(
                title = paste0("Actual Size Preview (", img_info$px_width, "x", img_info$px_height, "px)"),
                size = "xl",
                easyClose = TRUE,
                shiny::div(
                    style = "overflow: auto; max-height: 70vh;",
                    shiny::tags$img(
                        src = paste0("data:image/png;base64,", img_info$data),
                        style = "max-width: none;"
                    )
                ),
                footer = shiny::modalButton("Close")
            ))
        })

        # Download handler
        output$download <- shiny::downloadHandler(
            filename = function() {
                fmt <- input$format %||% "png"
                paste0("story_layout_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt)
            },
            content = function(file) {
                shiny::req(build_layout)

                w <- input$width %||% 12
                h <- input$height %||% 9
                dpi <- input$dpi %||% 150
                fmt <- input$format %||% "png"

                if (fmt == "pdf") {
                    ggplot2::ggsave(file, build_layout(), width = w, height = h, device = "pdf", bg = "white")
                } else if (fmt == "svg") {
                    ggplot2::ggsave(file, build_layout(), width = w, height = h, device = "svg", bg = "white")
                } else {
                    ggplot2::ggsave(file, build_layout(), width = w, height = h, dpi = dpi, bg = "white")
                }
            }
        )

        # Return export settings
        list(
            width = shiny::reactive(input$width %||% 12),
            height = shiny::reactive(input$height %||% 9),
            dpi = shiny::reactive(input$dpi %||% 150),
            format = shiny::reactive(input$format %||% "png")
        )
    })
}
