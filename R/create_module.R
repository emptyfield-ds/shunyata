#' Create a new teaching module
#'
#' Create a new directory with R Markdown files for slides, exercises, and solutions.
#'
#' @param module_dir The directory name. Also the file name of the slides file.
#' @param module_title The title of the module
#' @param module_subtitle The subtitle of the module
#' @param include_exercises Include `exercises.Rmd`?
#' @param include_solutions  Include `solutions.Rmd`?
#' @param include_img Include an `img/` folder?
#'
#' @return `module_dir`, invisibly.
#' @export
create_module <- function(module_dir, module_title, module_subtitle = "", include_exercises = TRUE, include_solutions = TRUE, include_img = TRUE) {
  if (dir_exists(module_dir)) {
    if (usethis::ui_yeah("Delete directory {module_dir}?")) {
      dir_delete(module_dir)
    } else {
      return(invisible(module_dir))
    }
  }

  usethis::ui_done("Creating {usethis::ui_path(module_dir)}")
  dir_create(module_dir)

  module_data <- list(module_title = module_title)
  use_course_template("slides.Rmd", path(module_dir, paste0(module_dir, ".Rmd")), data = c(module_data, module_subtitle = module_subtitle))
  if (include_exercises) use_course_template("exercises.Rmd", path(module_dir, "exercises.Rmd"), data = module_data)
  if (include_solutions) use_course_template("solutions.Rmd", path(module_dir, "solutions.Rmd"), data = module_data)
  if (include_img) dir_create(path(module_dir, "img"))

  invisible(module_dir)
}

#' Sanitize module
#'
#' Remove files and directories that aren't needed for the student copy
#'
#' @param module The name of the module
#' @param new_path Where to copy the sanitized directory
#' @param rm_files Which files to remove
#' @param rm_dir Which directories to remove
#' @param overwrite Overwrite the sanitized module if it already exists?
#'
#' @return invisibly, the path of the new module
#' @export
sanitize_module <- function(module, new_path = NULL, rm_files = c("theme.css", "kakashi.css", "solutions.Rmd"), rm_dir = c("img", "libs"), overwrite = FALSE) {
  usethis::ui_done("Cloning {usethis::ui_path(module)}")
  dir <- file_temp("sanitize_")
  withr::defer(dir_delete(dir))
  if (!dir_exists(dir)) dir_create(dir)
  temp_module <- dir_copy(module, dir, overwrite = overwrite)

  usethis::ui_done("Sanitizing {usethis::ui_path(module)}")
  # remove files
  clean_module <- path_file(module)
  rm_files <- c(paste0(clean_module, ".Rmd"), paste0(clean_module, ".html"), rm_files)
  rm_files <- stringr::str_remove(rm_files, "^[0-9]+-")
  purrr::walk(rm_files, file_delete_safe, temp_module)
  # remove directories
  rm_dir <- c(paste0(stringr::str_remove(clean_module, "^[0-9]+-"), "_files"), rm_dir)
  purrr::walk(rm_dir, dir_delete_safe, temp_module)

  if (has_dehydrated_rproj(temp_module)) {
    rproj_file <- path_file(dir_ls(temp_module, regexp = "dehydrated_Rproj$"))
    usethis::ui_done("Rehydrating {usethis::ui_path(rproj_file)}")
    rehydrate_rproj(temp_module)
  }

  new_module_path <- path(new_path, module)
  usethis::ui_done("Copying to {usethis::ui_path(new_module_path)}")
  if (dir_exists(new_path) && overwrite) dir_delete(new_path)
  dir_copy(temp_module, new_path, overwrite = overwrite)


  invisible(new_module_path)
}

file_delete_safe <- function(path, dir) {
  if (file_exists(path(dir, path))) {
    usethis::ui_done("Removing {usethis::ui_path(path)}")
    file_delete(path(dir, path))
  }

  invisible(path)
}

dir_delete_safe <- function(path, dir) {
  if (dir_exists(path(dir, path))) {
    usethis::ui_done("Removing {usethis::ui_path(path)}")
    dir_delete(path(dir, path))
  }

  invisible(path)
}
