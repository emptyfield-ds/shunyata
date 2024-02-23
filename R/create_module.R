#' Create a new teaching module
#'
#' Create a new directory with R Markdown files for slides, exercises, and solutions.
#'
#' @param module_dir The directory name. Also the file name of the slides file.
#' @param module_title The title of the module
#' @param module_subtitle The subtitle of the module
#' @param include_exercises Include `exercises.qmd`?
#' @param include_solutions  Include `solutions.qmd`?
#' @param include_img Include an `img/` folder?
#'
#' @return `module_dir`, invisibly.
#' @export
create_module <- function(module_dir, module_title, module_subtitle = "", include_exercises = TRUE, include_solutions = TRUE, include_img = TRUE) {
  if (dir_exists(module_dir)) {
    usethis::ui_info("{usethis::ui_path(module_dir)} already exists")
    if (usethis::ui_yeah("Delete directory?")) {
      dir_delete(module_dir)
    } else {
      return(invisible(module_dir))
    }
  }

  usethis::ui_done("Creating {usethis::ui_path(module_dir)}")
  dir_create(module_dir)
  usethis::with_project(path_dir(module_dir), {
    module_data <- list(module_title = module_title)
    use_course_template("slides.qmd", path(path_file(module_dir), paste0(path_file(module_dir), ".qmd")), data = c(module_data, module_subtitle = module_subtitle))
    usethis::use_github_file("malcolmbarrett/kakashi", path = "kakashi.css", save_as = "theme.css")
    if (include_exercises) use_course_template("exercises.qmd", path(path_file(module_dir), "exercises.qmd"), data = module_data)
    if (include_solutions) use_course_template("solutions.qmd", path(path_file(module_dir), "solutions.qmd"), data = module_data)
    if (include_img) dir_create(path(path_file(module_dir), "img"))
  }, force = TRUE)

  invisible(module_dir)
}
