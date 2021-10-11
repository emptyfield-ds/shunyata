#' Copy a teaching package to a temporary directory and open it in RStudio
#'
#' @param path The root path where the teaching material is
#' @param module The R Package teaching module to open
#' @param open Open in RStudio?
#'
#' @return Invisibly, the temporary directory where the module package has been copied
#' @export
open_r_pkg_module <- function(path = path_warehouse(), module = c("setup", "write_code", "document", "test", "teach", "add_files"), open = interactive()) {
  module <- match.arg(module)
  module <- paste0("r_packages_", module)
  dir <- file_temp()
  dir_create(dir)
  dir_copy(path(normalizePath(path), module), dir)
  temp_pkg_dir <- path(dir, module)

  rehydrate_rproj(temp_pkg_dir)

  if (open) usethis::proj_activate(temp_pkg_dir)

  invisible(temp_pkg_dir)
}

#' Dehydrate and rehydrate rproj files for safe project hygiene
#'
#' @param dir The project directory
#'
#' @return a filename of either `.Rproj` or `.dehydrated_Rproj`
#' @export
dehydrate_rproj <- function(dir) {
  x <- dir_ls(dir, regexp = "Rproj$")
  file_move(
    x,
    path_ext_set(path_ext_remove(x), "dehydrated_Rproj")
  )
}

#' @rdname dehydrate_rproj
#' @export
rehydrate_rproj <- function(dir) {
  x <- dir_ls(dir, regexp = "dehydrated_Rproj$")
  file_move(
    x,
    path_ext_set(path_ext_remove(x), "Rproj")
  )
}

has_dehydrated_rproj <- function(dir) {
  x <- dir_ls(dir, type = "file")
  any(grepl("dehydrated_Rproj$", x, ignore.case = TRUE))
}
