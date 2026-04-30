# functions.R
# Grade comparison functions for Shiny app
# All grade logic lives here; sourced by app.R

library(dplyr)
library(tidyr)
library(ggplot2)
library(see)
library(patchwork)
library(purrr)


# Band classification --------------------------------------------------------

# Undergraduate: Fail <40, 3rd 40-49, 2:2 50-59, 2:1 60-69, 1st 70+
# Masters:       Fail <50, Pass 50-59, Merit 60-69, Distinction 70+
UG_BAND_LEVELS <- c("Fail", "3rd", "2:2", "2:1", "1st")
PG_BAND_LEVELS <- c("Fail", "Pass", "Merit", "Distinction")

#' Assign grade bands.
#' @param grade  Numeric vector of grades
#' @param level  "undergraduate" or "masters"
assign_band <- function(grade, level = c("undergraduate", "masters")) {
  level <- match.arg(level)

  if (level == "undergraduate") {
    factor(
      case_when(
        grade < 40  ~ "Fail",
        grade < 50  ~ "3rd",
        grade < 60  ~ "2:2",
        grade < 70  ~ "2:1",
        grade >= 70 ~ "1st",
        TRUE        ~ NA_character_
      ),
      levels = UG_BAND_LEVELS
    )
  } else {
    factor(
      case_when(
        grade < 50  ~ "Fail",
        grade < 60  ~ "Pass",
        grade < 70  ~ "Merit",
        grade >= 70 ~ "Distinction",
        TRUE        ~ NA_character_
      ),
      levels = PG_BAND_LEVELS
    )
  }
}

#' Return the correct ordered band level vector for a given study level.
band_levels <- function(level = c("undergraduate", "masters")) {
  if (match.arg(level) == "undergraduate") UG_BAND_LEVELS else PG_BAND_LEVELS
}

#' Re-assign bands on an already-loaded data frame when the level toggle changes.
#' Avoids re-reading the CSV from disk.
reclassify_bands <- function(grades, level = c("undergraduate", "masters")) {
  grades$band <- assign_band(grades$grade, match.arg(level))
  grades
}


# Data ingestion & validation ------------------------------------------------

#' Read and validate a grades CSV.
#' Expected columns: grader (character), grade (numeric)
#' @param path   Path to CSV file
#' @param level  "undergraduate" or "masters"
load_grades <- function(path, level = c("undergraduate", "masters")) {
  level <- match.arg(level)

  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE),
    error = function(e) stop("Could not read CSV file: ", conditionMessage(e))
  )

  required <- c("grader", "grade")
  missing  <- setdiff(required, colnames(df))
  if (length(missing) > 0) {
    stop("CSV is missing required column(s): ", paste(missing, collapse = ", "),
         ". Expected columns: grader, grade.")
  }

  if (!is.numeric(df$grade)) {
    stop("The 'grade' column must be numeric.")
  }

  if (any(is.na(df$grade))) {
    warning("Removed ", sum(is.na(df$grade)), " row(s) with missing grades.")
    df <- df[!is.na(df$grade), ]
  }

  df$band <- assign_band(df$grade, level)
  df
}


# Band count table -----------------------------------------------------------

#' Count grades per band for one grader's data frame.
#' All bands for the given level appear even if count is zero.
count_bands <- function(dat, level = c("undergraduate", "masters")) {
  lvls       <- band_levels(match.arg(level))
  full_bands <- data.frame(band = factor(lvls, levels = lvls))

  dat |>
    count(band, .drop = FALSE) |>
    right_join(full_bands, by = "band") |>
    mutate(
      n     = coalesce(n, 0L),
      pct   = round(n / sum(n) * 100, 0),
      label = paste0(n, " (", pct, "%)")
    ) |>
    select(band, label)
}

#' Wide band-summary table: combined column first, then one column per grader.
build_band_table <- function(grades, level = c("undergraduate", "masters")) {
  level        <- match.arg(level)
  grader_names <- sort(unique(grades$grader))

  combined <- count_bands(grades, level) |>
    rename(`All Graders` = label)

  per_grader <- grader_names |>
    set_names() |>
    map(\(g) count_bands(filter(grades, grader == g), level) |>
              rename(!!g := label))

  reduce(c(list(combined), per_grader), left_join, by = "band") |>
    rename(Band = band)
}


# Summary statistics ---------------------------------------------------------

build_summary_stats <- function(grades) {
  bind_rows(mutate(grades, grader = "All Graders"), grades) |>
    group_by(grader) |>
    summarise(
      N      = n(),
      Mean   = round(mean(grade), 1),
      Median = round(median(grade), 1),
      SD     = round(sd(grade), 1),
      Min    = min(grade),
      Max    = max(grade),
      .groups = "drop"
    ) |>
    rename(Grader = grader)
}


# Plotting -------------------------------------------------------------------

# Named colour map — covers all band names across both UG and PG
# Fail always red; passing bands progress through yellow -> green -> blue
BAND_COLOUR_MAP <- c(
  "Fail"        = as.character(okabeito_colors()[6]),
  "3rd"         = as.character(okabeito_colors()[4]),
  "2:2"         = as.character(okabeito_colors()[3]),
  "Pass"        = as.character(okabeito_colors()[3]),
  "2:1"         = as.character(okabeito_colors()[2]),
  "Merit"       = as.character(okabeito_colors()[2]),
  "1st"         = as.character(okabeito_colors()[5]),
  "Distinction" = as.character(okabeito_colors()[5])
)

plot_grades <- function(dat, lab, ymax, xmin = 0, xmax = 100,
                        level = c("undergraduate", "masters"),
                        show_legend = TRUE) {
  lvls    <- band_levels(match.arg(level))
  colours <- BAND_COLOUR_MAP[lvls]

  p <- ggplot(dat, aes(x = grade, fill = band)) +
    geom_histogram(binwidth = 1) +
    scale_y_continuous(
      limits = c(0, ymax),
      breaks = seq(0, ymax, by = max(1, floor(ymax / 10)))
    ) +
    scale_x_continuous(
      limits = c(xmin - 1, xmax + 1),
      breaks = seq(xmin, xmax, by = 5),
      oob    = scales::oob_keep
    ) +
    scale_fill_manual(values = colours, drop = FALSE) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(size = 18, face = "bold"),
      axis.title        = element_text(size = 15),
      axis.text         = element_text(size = 13),
      legend.title      = element_text(size = 14),
      legend.text       = element_text(size = 13)
    ) +
    labs(title = lab, x = "Grade", y = "Count", fill = "Band")

  if (!show_legend) p <- p + theme(legend.position = "none",
                                    plot.margin = margin(t = 5, r = 5, b = 20, l = 5))
  p
}

build_combined_plot <- function(grades, level = c("undergraduate", "masters")) {
  level <- match.arg(level)

  ymax <- grades |>
    count(grade) |> pull(n) |> max() |>
    (\(x) ceiling(x / 5) * 5)()

  xmin <- floor(min(grades$grade)  / 5) * 5
  xmax <- ceiling(max(grades$grade) / 5) * 5

  plot_grades(grades, lab = "All Graders",
              ymax = ymax, xmin = xmin, xmax = xmax,
              level = level, show_legend = TRUE) +
    theme(legend.position = "bottom")
}

build_grader_plots <- function(grades, level = c("undergraduate", "masters")) {
  level        <- match.arg(level)
  grader_names <- sort(unique(grades$grader))

  ymax <- grades |>
    count(grade) |> pull(n) |> max() |>
    (\(x) ceiling(x / 5) * 5)()

  xmin <- floor(min(grades$grade)  / 5) * 5
  xmax <- ceiling(max(grades$grade) / 5) * 5

  grader_names |>
    map(\(g) plot_grades(filter(grades, grader == g),
                         lab = g, ymax = ymax, xmin = xmin, xmax = xmax,
                         level = level, show_legend = FALSE)) |>
    wrap_plots(ncol = 1)
}

# Keep build_grade_plots for PDF (combined layout in one figure)
build_grade_plots <- function(grades, level = c("undergraduate", "masters")) {
  level        <- match.arg(level)
  grader_names <- sort(unique(grades$grader))

  ymax <- grades |>
    count(grade) |> pull(n) |> max() |>
    (\(x) ceiling(x / 5) * 5)()

  xmin <- floor(min(grades$grade)  / 5) * 5
  xmax <- ceiling(max(grades$grade) / 5) * 5

  combined_plot <- plot_grades(grades, lab = "All Graders",
                               ymax = ymax, xmin = xmin, xmax = xmax,
                               level = level, show_legend = TRUE) +
    theme(legend.position = "bottom")

  grader_plots <- grader_names |>
    map(\(g) plot_grades(filter(grades, grader == g),
                         lab = g, ymax = ymax, xmin = xmin, xmax = xmax,
                         level = level, show_legend = FALSE))

  right_col <- wrap_plots(grader_plots, ncol = 1)

  combined_plot | right_col
}


# PDF report -----------------------------------------------------------------

#' Render a PDF report to `output_path`.
#'
#' Uses pdflatex (not xelatex) because shinyapps.io does not have system fonts
#' available to fontspec/xelatex. pdflatex works with TinyTeX out of the box.
#'
#' All required LaTeX packages are pre-installed in global.R so this function
#' never triggers a mid-render tlmgr download.
#'
#' @param grades       Filtered grades data frame (with band column)
#' @param level        "undergraduate" or "masters"
#' @param output_path  Destination .pdf file path
render_pdf_report <- function(grades, level = c("undergraduate", "masters"),
                              output_path) {
  level       <- match.arg(level)
  level_label <- if (level == "masters") "Masters" else "Undergraduate"

  fig_height <- max(4, 3.5 * length(unique(grades$grader)))   # right col drives height

  # Render into a session-specific temp dir so concurrent users don't collide
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  tmp_rmd <- file.path(tmp_dir, "report.Rmd")

  writeLines(c(
    '---',
    'title: "Grade Distribution Report"',
    paste0('subtitle: "', level_label, ' --- ', format(Sys.Date(), "%d %B %Y"), '"'),
    'output:',
    '  pdf_document:',
    '    latex_engine: pdflatex',
    '    toc: false',
    '    keep_tex: false',
    'geometry: "margin=2cm"',
    'fontsize: 11pt',
    '---',
    '',
    '```{r setup, include=FALSE}',
    'knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)',
    '```',
    '',
    '## Grade Distributions',
    '',
    paste0('```{r distributions, fig.width=10, fig.height=', fig_height, ', out.width="100%"}'),
    'print(build_grade_plots(report_data, level = report_level))',
    '```',
    '',
    '\\newpage',
    '',
    '## Band Summary',
    '',
    '```{r band_table}',
    'knitr::kable(',
    '  build_band_table(report_data, level = report_level),',
    '  booktabs = TRUE, linesep = ""',
    ')',
    '```',
    '',
    '## Descriptive Statistics',
    '',
    '```{r stats_table}',
    'knitr::kable(',
    '  build_summary_stats(report_data),',
    '  booktabs = TRUE, linesep = ""',
    ')',
    '```'
  ), tmp_rmd)

  render_env <- list2env(
    list(
      report_data         = grades,
      report_level        = level,
      build_grade_plots   = build_grade_plots,
      build_band_table    = build_band_table,
      build_summary_stats = build_summary_stats
    ),
    parent = globalenv()
  )

  rmarkdown::render(
    input       = tmp_rmd,
    output_file = "report.pdf",
    output_dir  = tmp_dir,
    envir       = render_env,
    quiet       = FALSE
  )

  file.copy(file.path(tmp_dir, "report.pdf"), output_path, overwrite = TRUE)

  unlink(tmp_dir, recursive = TRUE)
}