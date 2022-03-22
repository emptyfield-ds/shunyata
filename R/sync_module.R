#' Sync a module to GitHub
#'
#' `sync_module()` takes a raw teaching module, renders slides, sanitizes, and
#' syncs with the existing GitHub repo for the module. Use with
#' [`path_warehouse()`].
#'
#' @param .dir The path to the module
#'
#' @export
sync_module <- function(.dir) {
  check_exists_on_gh(path_file(.dir))
  temp_dir <- fs::path_temp(path_file(.dir))
  withr::defer(dir_delete(temp_dir))

  clone_module(temp_dir)
  sanitize_module(.dir, new_path = temp_dir, overwrite = TRUE)
  commit_changes(temp_dir)

  invisible()
}

#' Write a path to the teaching warehouse
#'
#' `path_warehouse()` uses the `shunyata.teaching_warehouse` option to assemble
#' an path to a given module.
#'
#' @param ... Additional path details passed to [`fs::path()`].
#'
#' @return A path
#' @export
#'
#' @examples
#' path_warehouse("purrr_basics")
path_warehouse <- function(...) {
  path(getOption("shunyata.teaching_warehouse"), ...)
}

clone_module <- function(temp_dir) {
  usethis::ui_done("Cloning from GitHub")
  module <- path_file(temp_dir)
  dir <- suppressMessages(gert::git_clone(
    glue::glue("https://github.com/emptyfield-ds/{module}.git"),
    path = temp_dir
  ))

  sanitize_files <- c(
    dir_ls(dir, regexp = "sanitize"),
    dir_ls(dir, regexp = "cheatsheet_rmarkdown-2.0"),
    dir_ls(dir, regexp = "cheatsheet_data-visualization-2.1")
  )
  if (length(sanitize_files) > 0) {
    usethis::ui_done("Deleting outdated files")
    file_delete(sanitize_files)
  }

  dir
}

commit_changes <- function(repo) {
  usethis::ui_done("Commiting and pushing changes")
  suppressMessages(gert::git_add(".", repo = repo))
  suppressMessages(
    gert::git_commit(glue("Render module: {Sys.time()}"), repo = repo)
  )
  suppressMessages(gert::git_push(repo = repo))
  remind_pull_cloud(repo = repo)
}

remind_pull_cloud <- function(repo) {
  module <- path_file(repo)
  query <- glue::glue("/repos/emptyfield-ds/{module}")
  rstudio_cloud_url <- gh::gh(query)$homepage
  if (is.null(rstudio_cloud_url)) {
    return(invisible())
  }

  usethis::ui_todo(
    "Pull changes on RStudio Cloud: \\
    {usethis::ui_value(rstudio_cloud_url)}"
  )

  utils::browseURL(rstudio_cloud_url)

  usethis::ui_todo(
    "Do you need to update {usethis::ui_value('emptyfield-ds/solutions')}?"
  )
}
