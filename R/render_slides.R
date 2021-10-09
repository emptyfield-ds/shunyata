#' Title
#'
#' @param dir
#' @param render
#'
#' @return
#' @export
#'
#' @examples
render_slides <- function(dir, rmd_name = NULL, render = TRUE) {
  if (is.null(rmd_name)) rmd_name <- fs::path(dir, paste0(dir, ".Rmd"))

  if (has_no_slides(dir, path_file(rmd_name))) {
    usethis::ui_info("No slides found in {dir}")
    return(invisible())
  }
  if (is.null(rmd_name)) rmd_name <- fs::path(dir, paste0(dir, ".Rmd"))
  if (render) {
    usethis::ui_done("Rendering {usethis::ui_path(rmd_name)}")
    suppressMessages(
      suppressWarnings(
        rmarkdown::render(rmd_name, quiet = TRUE, envir = new.env())
      )
    )
  }

  html_name <- normalizePath(fs::path_ext_set(rmd_name, ".html"))
  file_name <- paste0("file://", html_name)

  usethis::ui_done("Rendering {usethis::ui_path(fs::path(dir, 'slides.pdf'))}")
  pdf_name <- suppressMessages(
    xaringanBuilder::build_pdf(file_name, fs::path(dir, "slides.pdf"))
  )

  invisible(pdf_name)
}

has_no_slides <- function(dir, rmd_name) {
  slides <- rmd_name %in% fs::path_file(fs::dir_ls(dir, type = "file"))
  !slides
}
