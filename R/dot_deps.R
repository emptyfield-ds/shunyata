#' Manage module dependencies
#'
#' `read_deps()` pulls the package dependencies in `.deps` for `modules` from GitHub
#' @param deps A vector of dependencies
#' @param path A path to the module
#' @param modules A vector of modules
#'
#' @return A vector of module dependencies
#'
#' @export
write_deps <- function(deps, path = ".") {
  stopifnot(length(deps) > 0)
  usethis::ui_done("Writing {usethis::ui_path('.deps')}")
  writeLines(deps, path(path, ".deps"))
}

#' @export
#' @rdname write_deps
detect_deps <- function(path) {
  sort(unique((renv::dependencies(path, errors = "ignore")$Package)))
}

#' @export
#' @rdname write_deps
read_deps <- function(modules) {
  sort(unique(purrr::flatten_chr(purrr::map(modules, read_deps_from_repo))))
}

read_deps_from_repo <- function(module) {
  check_exists_on_gh(module)
  url <- glue::glue("https://raw.githubusercontent.com/emptyfield-ds/{module}/HEAD/.deps")
  readLines(url, encoding = "UTF-8", warn = FALSE)
}
