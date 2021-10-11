#' Title
#'
#' @param .dir
#'
#' @return
#' @export
#'
#' @examples
sync_module <- function(.dir) {
  check_exists_on_gh(path_file(.dir))
  temp_dir <- fs::path_temp(path_file(.dir))
  withr::defer(dir_delete(temp_dir))

  clone_module(temp_dir)
  sanitize_module(.dir, new_path = temp_dir, overwrite = TRUE)
  commit_changes(temp_dir)

  invisible()
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
path_warehouse <- function(...) {
  path(getOption("shunyata.teaching_warehouse"), ...)
}

clone_module <- function(temp_dir) {
  module <- path_file(temp_dir)

  usethis::ui_done("Cloning from GitHub")
  module <- path_file(temp_dir)
  suppressMessages(gert::git_clone(
    glue::glue("https://github.com/emptyfield-ds/{module}.git"),
    path = temp_dir
  ))
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
}
