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
        helpText("File should be a .csv with two columns."),
    helpText("Required columns: ", code("grader"), ", ", code("grade")),
   helpText("Please anonymise graders by assigning numbers or using initials. Do not include any identifiable student or module information."),
    hr(),

    # --- Study level toggle -------------------------------------------------
    radioButtons(
      "study_level",
      label    = "Study level",
      choices  = c("Undergraduate" = "undergraduate",
                   "Masters"       = "masters"),
      selected = "undergraduate",
      inline   = TRUE
    ),
    helpText(
      uiOutput("fail_threshold_text")
    ),

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
                "Fail: < 50",   tags$br(),
                "Pass: 50-59",  tags$br(),
                "Merit: 60-69", tags$br(),
                "Distinction: 70+")
    } else {
      tags$span(style = "color:#555;",
                "Fail: < 40", tags$br(),
                "3rd: 40-49", tags$br(),
                "2:2: 50-59", tags$br(),
                "2:1: 60-69", tags$br(),
                "1st: 70+")
    }
  })

  # --- Reactive: load CSV --------------------------------------------------
  grades_raw <- reactive({
    req(input$csv_file)
    withCallingHandlers(
      tryCatch(
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

  # --- Reactive: apply band classification ---------------------------------
  grades <- reactive({
    req(grades_raw())
    reclassify_bands(grades_raw(), level())
  })

  # --- Reactive: number of graders (for plot height) -----------------------
  n_graders <- reactive({
    req(grades())
    length(unique(grades()$grader))
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
        nav_panel("Distributions",
                  div(style = "display:flex; align-items:flex-start; gap:16px; max-width:900px;",
                      # Left: All Graders — fixed height, wider to accommodate legend
                      div(style = "flex:0 0 auto; width:440px;",
                          plotOutput("combined_plot", height = "320px")),
                      # Right: per-grader — narrower (no legend), taller scroll window
                      div(style = "flex:1 1 auto; overflow-y:auto; max-height:560px;",
                          plotOutput("grader_plots", height = "auto"))
                  )),
        nav_panel("Band Summary",  tableOutput("band_table")),
        nav_panel("Statistics",    tableOutput("stats_table"))
      )
    }
  })

  # --- Combined plot (left, fixed height) -----------------------------------
  output$combined_plot <- renderPlot({
    req(grades())
    validate(need(nrow(grades()) > 0, "No data to display."))
    build_combined_plot(grades(), level = level())
  }, width = 400, height = 300)

  # --- Per-grader plots (right, scrollable) ---------------------------------
  output$grader_plots <- renderPlot({
    req(grades())
    validate(need(nrow(grades()) > 0, "No data to display."))
    build_grader_plots(grades(), level = level())
  },
  width  = function() 400,
  height = function() {
    req(n_graders())
    250 * n_graders()
  })

  # --- Band table -----------------------------------------------------------
  output$band_table <- renderTable({
    req(grades())
    validate(need(nrow(grades()) > 0, "No data to display."))
    build_band_table(grades(), level = level())
  },
  striped = TRUE, hover = TRUE, bordered = TRUE)

  # --- Stats table ----------------------------------------------------------
  output$stats_table <- renderTable({
    req(grades())
    validate(need(nrow(grades()) > 0, "No data to display."))
    build_summary_stats(grades())
  },
  striped = TRUE, hover = TRUE, bordered = TRUE)

  # --- PDF download ---------------------------------------------------------
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("grade_report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      req(grades())
      validate(need(nrow(grades()) > 0, "No data to report."))

      withProgress(message = "Rendering PDF...", value = 0.5, {
        tryCatch(
          render_pdf_report(
            grades      = grades(),
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