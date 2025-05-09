make_table <- function(data, tbl_title = NULL, tbl_subtitle = NULL, footnotes = NULL, rowname_highlight = NULL){

  # Basic table with title and subtitle
  if(!is.null(rowname_highlight)){
    gt_tbl <- data |>
      dplyr::group_by(sex) |>
      gt::gt(rowname_col = rowname_highlight)
  } else {
    gt_tbl <- gt::gt(data)
  }

  gt_tbl <- gt_tbl |>
    gt::sub_missing(columns = tidyselect::where(is.numeric),
                    missing_text = "---")
  gt_tbl <- gt_tbl |>
    gt::tab_header(title = gt::md(glue::glue("**{tbl_title}**")),
                   subtitle = tbl_subtitle)

  if(!is.null(footnotes)){
    gt_tbl <- gt_tbl |>
      gt::tab_source_note(source_note = footnotes)
  }

  gt_tbl <- gt_tbl |>
    gt::fmt_percent(columns = c("prev_rate", "ci_results_lower", "ci_results_upper"),
                    decimals = 2) |>
    gt::cols_merge(columns = c("ci_results_lower", "ci_results_upper"),
                   pattern = "<<[{1} - {2}]>>") |>
    gt::tab_style(style = gt::cell_text(weight = "bold"),
                  locations = gt::cells_body(columns = prev_rate))

}
