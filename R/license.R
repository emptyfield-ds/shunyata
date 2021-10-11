#' Add CC-by-A License to Repo `README`
#'
#' `use_edu_cca_license()` adds a Creative Commons Attribution 4.0 International
#' License to the `README` of an educational repository.
#'
#' @param proj The directory of the project, by default the usethis project.
#'
#' @return Invisibly, the path to the `README`
#' @export
use_edu_cca_license <- function(proj = usethis::proj_get()) {
  readme <- usethis::proj_path("README.Rmd")
  stopifnot(file_exists(readme))

  readme_vec <- readLines(readme, encoding = "UTF-8")

  usethis::ui_done("Adding license to {usethis::ui_path('README.Rmd')}")

  writeLines(
    c(
      readme_vec,
      "",
      "## License",
      "[![forthebadge](https://forthebadge.com/images/badges/cc-by.svg)](https://creativecommons.org/licenses/by/4.0/)",
      "",
      "The materials in these repository are open source and free to use with attribution. This work is licensed under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/)."
    ),
    con = readme
  )

  usethis::ui_todo("Re-knit {usethis::ui_path('README.Rmd')}")

  invisible(readme)
}
