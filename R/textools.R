#' Convert plot to tikz and write to file
#'
#' \code{save_tikz()} is a function that converts an R plot to tikz code and
#' saves it to a file. This is a function that is part of the process of easily
#' getting R code/objects into a form that can be used in Rmarkdown as well as
#' transferred to programs such as Overleaf.
#' Robust function: TRUE
#'
#' @param fig Plot object (ggplot etc.)
#' @param path Character, file name to store plot (.tex file)
#'
#' @return Message that file is written and the figure path invisibly
#' @export



save_tikz <- function(fig = NULL, path = NULL, sanitize = TRUE) {

  if (is.null(fig)) {
    usethis::ui_stop("fig is NULL, provide figure to save")
  }

  if (is.null(path)) {
    usethis::ui_stop("path is NULL, don't know where to save figure file")
  }

  if (fs::path_file(fs::path_ext_remove(path)) == path) {
    path <- fs::path("_figures",
                     fs::path_ext_remove(path), ext = "tex")
  }

  if (fs::path_ext(path) == "") {
    usethis::ui_info("No file extension given, adding .tex")
    path <- fs::path_ext_set(path, "tex")
  }

  tikzDevice::tikz(file = path, width = 6.4, height = 3.54, sanitize = sanitize,
                   verbose = FALSE)
  print(fig)
  dev.off()

  usethis::ui_done(paste0("Figure is saved to: ", path))

  invisible(path)

}
