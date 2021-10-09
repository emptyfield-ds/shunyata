#' Copy a teaching package to a temporary directory and open it in RStudio
#'
#' @param path The root path where the teaching material is
#' @param module The R Package teaching module to open
#' @param open Open in RStudio?
#'
#' @return Invisibly, the temporary directory where the module package has been copied
#' @export
temp_pkg <- function(path = ".", module = c("setup", "write_code", "document", "test", "teach", "add_files"), open = interactive()) {
  module <- match.arg(module)
  module <- paste0("r_packages_", module)
  dir <- fs::file_temp()
  fs::dir_create(dir)
  fs::dir_copy(fs::path(normalizePath(path), module), dir)
  temp_pkg_dir <- fs::path(dir, module)

  rehydrate_rproj(temp_pkg_dir)

  if (open) usethis::proj_activate(temp_pkg_dir)

  invisible(temp_pkg_dir)
}

#' Dehydrate and rehydrate rproj files for safe project hygiene
#'
#' @param x the Rproj file
#'
#' @return a filename of either `.Rproj` or `.dehydrated_Rproj`
#' @export
dehydrate_rproj <- function(dir) {
  x <- fs::dir_ls(dir, regexp = "Rproj$")
  fs::file_move(
    x,
    fs::path_ext_set(fs::path_ext_remove(x), "dehydrated_Rproj")
  )
}

#' @rdname dehydrate_rproj
#' @export
rehydrate_rproj <- function(dir) {
  x <- fs::dir_ls(dir, regexp = "dehydrated_Rproj$")
  fs::file_move(
    x,
    fs::path_ext_set(fs::path_ext_remove(x), "Rproj")
  )
}

has_dehydrated_rproj <- function(dir) {
  x <- fs::dir_ls(dir, type = "file")
  any(grepl("dehydrated_Rproj$", x, ignore.case = TRUE))
}
