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
sanitize_module <- function(module, new_path = NULL, rm_files = c("theme.css", "kakashi.css", "solutions.qmd"), rm_dir = c("img", "libs", "_extensions"), overwrite = FALSE, build_slides = TRUE, qmd_name = NULL) {
  usethis::ui_done("Copying {usethis::ui_path(module)} to temporary directory")
  dir <- path_temp("sanitize", path_file(module))
  withr::defer(dir_delete(dir))
  if (!dir_exists(dir)) dir_create(dir)
  temp_module <- dir_copy(module, dir, overwrite = overwrite)

  if (build_slides) {
    if (dir_exists("_extensions")) {
      dir_copy("_extensions", dir)
    }
    if (is.null(qmd_name)) qmd_name <- path_ext_set(path(temp_module, path_file(module)), "qmd")
    render_slides(temp_module, qmd_name = qmd_name)
  }

  usethis::ui_done("Sanitizing {usethis::ui_path(path_file(module))}")

  # remove files
  clean_module <- path_file(module)
  rm_files <- c(paste0(clean_module, ".qmd"), paste0(clean_module, ".html"), rm_files)
  rm_files <- stringr::str_remove(rm_files, "^[0-9]+-")
  purrr::walk(rm_files, file_delete_safe, temp_module)

  # remove directories
  rm_dir <- c(paste0(stringr::str_remove(clean_module, "^[0-9]+-"), "_files"), rm_dir)
  purrr::walk(rm_dir, dir_delete_safe, temp_module)

  handle_rstudio_proj(temp_module)

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

handle_rstudio_proj <- function(module_dir) {
  if (has_dehydrated_rproj(module_dir)) {
    rehydrate_rproj(module_dir)
  } else if (!has_rproj(module_dir)) {
    old_proj <- usethis::proj_set(module_dir, force = TRUE)
    withr::defer(usethis::proj_set(old_proj))
    usethis::use_rstudio()
  }

  invisible()
}

has_rproj <- function(module_dir) {
  x <- dir_ls(module_dir, type = "file")
  any(grepl("Rproj$", x, ignore.case = TRUE))
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
