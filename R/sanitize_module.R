#' Sanitize module
#'
#' Remove files and directories that aren't needed for the student copy
#'
#' @param module The name of the module
#' @param new_path Where to copy the sanitized directory
#' @param rm_files Which files to remove
#' @param rm_dir Which directories to remove
#' @param overwrite Overwrite the sanitized module if it already exists?
#' @param build_slides Logical. Render the slides?
#' @inheritParams render_slides
#'
#' @return invisibly, the path of the new module
#' @export
sanitize_module <- function(module, new_path = NULL, rm_files = c("theme.css", "kakashi.css", "solutions.Rmd"), rm_dir = c("img", "libs"), overwrite = FALSE, build_slides = TRUE, rmd_name = NULL, ...) {
  usethis::ui_done("Cloning {usethis::ui_path(module)}")
  dir <- file_temp("sanitize_")
  withr::defer(dir_delete(dir))
  if (!dir_exists(dir)) dir_create(dir)
  temp_module <- dir_copy(module, dir, overwrite = overwrite)

  if (build_slides) {
    if (is.null(rmd_name)) rmd_name <- path_ext_set(path(temp_module, path_file(module)), "Rmd")
    render_slides(temp_module, rmd_name = rmd_name)
  }

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

  if (!is.null(new_path)) {
    usethis::ui_done("Copying to {usethis::ui_path(new_path)}")
    if (dir_exists(new_path) && !overwrite) {
      usethis::ui_stop("{new_path} exists already. Set `overwrite = TRUE` to continue.")
    }

    dir_copy(temp_module, new_path, overwrite = overwrite)

    return(invisible(new_path))
  }

  invisible(temp_module)
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
