#' Render Module Slides
#'
#' @param dir The directory where the module is located
#' @param qmd_name The name of Quarto file. By default, assumes that it is the same
#'   as `dir` with a `.qmd` extension.
#' @param render Logical. Render the Quarto file?
#'
#' @return Invisibly, the path to the rendered PDF
#' @export
render_slides <- function(dir, qmd_name = NULL, render = TRUE) {
  if (is.null(qmd_name)) qmd_name <- path(dir, paste0(fs::path_file(dir), ".qmd"))

  if (has_no_slides(dir, path_file(qmd_name))) {
    usethis::ui_info("No slides found in {dir}")
    return(invisible())
  }
  if (render) {
    usethis::ui_done("Rendering {usethis::ui_path(path_file(qmd_name))}")
    suppressMessages(
      suppressWarnings(
        quarto::quarto_render(qmd_name, quiet = TRUE)
      )
    )
  }

  html_name <- normalizePath(path_ext_set(qmd_name, ".html"))

  usethis::ui_done("Rendering {usethis::ui_path('slides.pdf')}")
  pdf_name <- suppressMessages(
    renderthis::to_pdf(html_name, path(dir, "slides.pdf"))
  )

  invisible(pdf_name)
}

has_no_slides <- function(dir, qmd_name) {
  slides <- qmd_name %in% path_file(dir_ls(dir, type = "file"))
  !slides
}
