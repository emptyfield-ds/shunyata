
set_remote <- function(module) {
  module <- glue::glue("https://github.com/emptyfield-ds/{module}.git")
  # TODO: check if exists
  gert::git_remote_add(module)
}

force_push <- function() {
  msg <- glue("Render module: {Sys.time()}")
  usethis::ui_done("Force pushing")
  gert::git_add(".")
  gert::git_commit(msg)
  gert::git_push(force = TRUE)
  code_for_cloud <- glue(
    "git fetch
    git reset origin/{usethis::git_branch_default()} --hard
    git pull
    "
  )
  usethis::ui_todo("Update git on RStudio Cloud")
  usethis::ui_code_block(code_for_cloud)
}
