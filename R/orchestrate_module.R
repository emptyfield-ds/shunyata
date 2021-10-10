sync_module <- function(.dir) {
  module <- path_file(.dir)
  temp_dir <- fs::path_temp(.dir)

  clone_module(module, temp_dir)
  sanitize_module(.dir, new_path = temp_dir, overwrite = TRUE)
  commit_changes(temp_dir)

  invisible(temp_dir)
}

clone_module <- function(module, temp_dir) {
  gert::git_clone(
    glue::glue("https://github.com/emptyfield-ds/{module}.git"),
    path = temp_dir
  )
}

commit_changes <- function(repo) {
  gert::git_add(".", repo = repo)
  gert::git_commit(glue::glue("Automatic update ({Sys.Date()})"), repo = repo)
  gert::git_push(repo = repo)
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
