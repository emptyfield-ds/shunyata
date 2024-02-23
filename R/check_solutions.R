#' Check solutions
#'
#' Check solutions hosted in `emptyfield-ds/solutions`
#'
#' @param .file The path of the files to check. If `NULL`, searches directory
#'   for relevant files.
#'
#' @export
check_solutions <- function(.file = NULL) {
  if (is.null(.file)) {
    .file <- fs::dir_ls(regexp = "qmd$", recurse = TRUE, type = "file")
  }

  expect_error_free(purrr::walk(.file, render_temp))
  usethis::ui_todo("Manually check project-based modules such as targets and renv")
}


#' @export
#' @rdname check_solutions
check_shiny_solutions <- function(.file = NULL) {
  if (is.null(.file)) {
    .file <- c("projects/shiny_challenge_solution/")
  }

  expect_error_free(purrr::walk(.file, test_shiny_app))
}

test_shiny_app <- function(app_dir) {
  usethis::ui_done("Testing {usethis::ui_path(app_dir)}")
  suppressMessages(
    suppressPackageStartupMessages({
      shiny::runTests(app_dir)
    })
  )
}

expect_error_free <- function(...) {
  testthat::expect_error(..., regexp = NA)
}

render_temp <- function(.file) {
  usethis::ui_done("Rendering {usethis::ui_path(.file)}")
  .file_name <- fs::path_ext_remove(fs::path_file(.file))
  .temp_path <- fs::path_temp(fs::path_ext_remove(.file))
  withr::defer(fs::dir_delete(.temp_path))
  fs::dir_create(.temp_path)

  quarto::quarto_render(
    .file,
    output_file = fs::path(.temp_path, .file_name),
    quiet = TRUE
  )
}
