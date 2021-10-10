#' Render Module Slides
#'
#' @param dir The directory where the module is located
#' @param rmd_name The name of Rmd file. By default, assumes that it is the same
#'   as `dir` with a `.Rmd` extension.
#' @param render Logical. Knit the Rmd file?
#'
#' @return Invisibly, the path to the rendered PDF
#' @export
render_slides <- function(dir, rmd_name = NULL, render = TRUE) {
  if (is.null(rmd_name)) rmd_name <- path(dir, paste0(dir, ".Rmd"))

  if (has_no_slides(dir, path_file(rmd_name))) {
    usethis::ui_info("No slides found in {dir}")
    return(invisible())
  }
  if (is.null(rmd_name)) rmd_name <- path(dir, paste0(dir, ".Rmd"))
  if (render) {
    usethis::ui_done("Rendering {usethis::ui_path(rmd_name)}")
    suppressMessages(
      suppressWarnings(
        rmarkdown::render(rmd_name, quiet = TRUE, envir = new.env())
      )
    )
  }

  html_name <- normalizePath(path_ext_set(rmd_name, ".html"))

  usethis::ui_done("Rendering {usethis::ui_path(path(dir, 'slides.pdf'))}")
  pdf_name <- suppressMessages(
    xaringanBuilder::build_pdf(html_name, path(dir, "slides.pdf"))
  )

  invisible(pdf_name)
}

has_no_slides <- function(dir, rmd_name) {
  slides <- rmd_name %in% path_file(dir_ls(dir, type = "file"))
  !slides
}
