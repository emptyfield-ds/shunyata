#' Copy a teaching package to a temporary directory and open it in RStudio
#'
#' @param path The root path where the teaching material is
#' @param module The R Package teaching module to open
#' @param open Open in RStudio?
#'
#' @return Invisibly, the temporary directory where the module package has been copied
#' @export
open_temp_module <- function(module, github = TRUE, open = interactive()) {
  dir <- path_temp("temp_module", path_file(module))
  if (dir_exists(dir)) dir_delete(dir)
  dir_create(dir)
  if (github) {
    module_dir <- clone_module(dir)
  } else {
    usethis::ui_done(
      "Copying {usethis::ui_path(path_file(module))} to temporary directory"
    )
    module_dir <- dir_copy(path(module), dir)
    handle_rstudio_proj(module_dir)
  }

  if (open) usethis::proj_activate(module_dir)

  invisible(module_dir)
}

#' Dehydrate and rehydrate rproj files for safe project hygiene
#'
#' @param dir The project directory
#'
#' @return a filename of either `.Rproj` or `.dehydrated_Rproj`
#' @export
dehydrate_rproj <- function(dir) {
  x <- dir_ls(dir, regexp = "Rproj$")
  usethis::ui_done("Dehydrating {usethis::ui_path(x)}")
  file_move(
    x,
    path_ext_set(path_ext_remove(x), "dehydrated_Rproj")
  )
}

#' @rdname dehydrate_rproj
#' @export
rehydrate_rproj <- function(dir) {
  x <- dir_ls(dir, regexp = "dehydrated_Rproj$")
  usethis::ui_done("Rehydrating {usethis::ui_path(x)}")
  file_move(
    x,
    path_ext_set(path_ext_remove(x), "Rproj")
  )
}

has_dehydrated_rproj <- function(dir) {
  x <- dir_ls(dir, type = "file")
  any(grepl("dehydrated_Rproj$", x, ignore.case = TRUE))
}
