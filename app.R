# app.R
# Grade comparison Shiny app
# Upload a CSV with columns: grader, grade
# Toggle undergraduate / masters band thresholds
# Downloads a PDF report

library(shiny)
library(bslib)

source("R/functions.R")


# UI -------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Grade Distribution Report",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 300,

    # --- File upload --------------------------------------------------------
    fileInput(
      "csv_file",
      label    = "Upload grades CSV",
      accept   = ".csv",
      multiple = FALSE
    ),
    helpText("Required columns: ", code("grader"), ", ", code("grade")),

    hr(),

    # --- Study level toggle -------------------------------------------------
    radioButtons(
      "study_level",
      label   = "Study level",
      choices = c("Undergraduate" = "undergraduate",
                  "Masters"       = "masters"),
      selected = "undergraduate",
      inline   = TRUE
    ),
    helpText(
      uiOutput("fail_threshold_text")
    ),

    hr(),

    # --- Grader filter (rendered dynamically after upload) ------------------
    uiOutput("grader_filter_ui"),

    hr(),

    # --- Download -----------------------------------------------------------
    downloadButton(
      "download_report",
      label = "Download PDF Report",
      class = "btn-primary w-100"
    )
  ),

  # Main panel ---------------------------------------------------------------
  uiOutput("main_content")
)


# Server ---------------------------------------------------------------------

server <- function(input, output, session) {

  # --- Helper: current level string ----------------------------------------
  level <- reactive(input$study_level)

  # --- Informational text below the toggle ----------------------------------
  output$fail_threshold_text <- renderUI({
    if (level() == "masters") {
      tags$span(style = "color:#555;",
                "Fail: < 50",    tags$br(),
                "Pass: 50-59",   tags$br(),
                "Merit: 60-69",  tags$br(),
                "Distinction: 70+")
    } else {
      tags$span(style = "color:#555;",
                "Fail: < 40",  tags$br(),
                "3rd: 40-49",  tags$br(),
                "2:2: 50-59",  tags$br(),
                "2:1: 60-69",  tags$br(),
                "1st: 70+")
    }
  })

  # --- Reactive: load CSV (re-runs only when a new file is uploaded) --------
  grades_raw <- reactive({
    req(input$csv_file)
    withCallingHandlers(
      tryCatch(
        # Load without band classification — we apply that separately so the
        # level toggle can update bands without re-reading from disk
        {
          df <- tryCatch(
            read.csv(input$csv_file$datapath, stringsAsFactors = FALSE),
            error = function(e) stop("Could not read CSV: ", conditionMessage(e))
          )
          required <- c("grader", "grade")
          missing  <- setdiff(required, colnames(df))
          if (length(missing) > 0)
            stop("Missing column(s): ", paste(missing, collapse = ", "))
          if (!is.numeric(df$grade))
            stop("The 'grade' column must be numeric.")
          df[!is.na(df$grade), ]
        },
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = NULL)
          NULL
        }
      ),
      warning = function(w) {
        showNotification(conditionMessage(w), type = "warning")
        invokeRestart("muffleWarning")
      }
    )
  })

  # --- Reactive: apply band classification (re-runs on level toggle too) ----
  grades <- reactive({
    req(grades_raw())
    reclassify_bands(grades_raw(), level())
  })

  # --- Reactive: grader names -----------------------------------------------
  grader_names <- reactive({
    req(grades())
    sort(unique(grades()$grader))
  })

  # --- Dynamic grader checkboxes -------------------------------------------
  output$grader_filter_ui <- renderUI({
    req(grader_names())
    tagList(
      strong("Show graders"),
      checkboxGroupInput(
        "selected_graders",
        label    = NULL,
        choices  = grader_names(),
        selected = grader_names()
      )
    )
  })

  # --- Reactive: filtered data ----------------------------------------------
  grades_filtered <- reactive({
    req(grades(), input$selected_graders)
    dplyr::filter(grades(), grader %in% input$selected_graders)
  })

  # --- Main content ---------------------------------------------------------
  output$main_content <- renderUI({
    if (is.null(input$csv_file)) {
      div(
        class = "text-center mt-5 text-muted",
        tags$i(class = "fa fa-upload fa-3x mb-3"),
        h4("Upload a CSV to get started"),
        p("The file needs a ", code("grader"), " column and a numeric ",
          code("grade"), " column.")
      )
    } else {
      navset_card_tab(
        nav_panel("Distributions", plotOutput("grade_plot", height = "auto")),
        nav_panel("Band Summary",  tableOutput("band_table")),
        nav_panel("Statistics",    tableOutput("stats_table"))
      )
    }
  })

  # --- Plots ----------------------------------------------------------------
  output$grade_plot <- renderPlot({
    req(grades_filtered())
    validate(need(nrow(grades_filtered()) > 0, "No data for selected graders."))
    build_grade_plots(grades_filtered(), level = level())
  },
  width  = 600,
  height = function() {
    n <- length(input$selected_graders %||% 1)
    280 * (n + 1)
  })

  # --- Band table -----------------------------------------------------------
  output$band_table <- renderTable({
    req(grades_filtered())
    validate(need(nrow(grades_filtered()) > 0, "No data for selected graders."))
    build_band_table(grades_filtered(), level = level())
  },
  striped = TRUE, hover = TRUE, bordered = TRUE)

  # --- Stats table ----------------------------------------------------------
  output$stats_table <- renderTable({
    req(grades_filtered())
    validate(need(nrow(grades_filtered()) > 0, "No data for selected graders."))
    build_summary_stats(grades_filtered())
  },
  striped = TRUE, hover = TRUE, bordered = TRUE)

  # --- PDF download ---------------------------------------------------------
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("grade_report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      req(grades_filtered())
      validate(need(nrow(grades_filtered()) > 0, "No data to report."))

      withProgress(message = "Rendering PDF...", value = 0.5, {
        tryCatch(
          render_pdf_report(
            grades      = grades_filtered(),
            level       = level(),
            output_path = file
          ),
          error = function(e) {
            showNotification(
              paste("PDF render failed:", conditionMessage(e)),
              type     = "error",
              duration = NULL
            )
          }
        )
      })
    }
  )
}


# Run ------------------------------------------------------------------------
shinyApp(ui, server)
