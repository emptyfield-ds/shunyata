#' Open rendered, hostes slides
#'
#' @param .module the name of the module
#'
#' @export
browse_slides <- function(.module) {
  url <- glue::glue("https://r-teaching-warehouse.netlify.app/{.module}/{.module}.html")
  browseURL(url)
}
