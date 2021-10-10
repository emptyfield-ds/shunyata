copy_module <- function(dir) {
  new_dir <- path_temp(dir)
  if (dir_exists(new_dir)) dir_delete(new_dir)
  usethis::ui_done("Copying {ui_path(dir)} to {ui_path(new_dir)}")
  dir_copy(dir, new_dir)
}

set_local_proj <- function(path = getwd()) {
  current_proj <- usethis::proj_get()
  withr::defer(usethis::proj_set(current_proj))
  usethis::proj_set(path, force = TRUE)
  usethis::use_rstudio()
}

normalize_module <- function(module) {
  destdir <- path_temp(module)
  new_module <- copy_module(module)
  withr::with_dir(new_module, {
    render_slides(new_module, path_ext_set(module, "Rmd"))
    sanitize_module(new_module, destdir, overwrite = TRUE)
  })

  dir_delete(new_module)

  withr::with_dir(destdir, {
    set_local_proj(destdir)
    gert::git_init()
    set_remote(module)
    force_push()
  })

  usethis::proj_activate(destdir)
}
