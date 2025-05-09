#' Check if a directory is empty (and create it if it doesn't exist)
#'
#' @description
#' This function checks whether a specified directory is empty.
#' If the directory does not exist, it will be created.
#' @details
#' It also checks whether the directory is newly created or already existed.
#'
#' @param dir_path A character string specifying the path to the directory.
#'
#' @return A logical value: `TRUE` if the directory is empty (or newly created), `FALSE` otherwise.
#' @author Martin Jung
#' @author Contributors: ChatGPT
#' @examples
#' # Check and create a directory
#' is_empty <- check_or_create_empty_dir("my_test_folder")
#' print(is_empty)
misc_emptyfolder <- function(dir_path) {
  # Check if directory exists
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    return(TRUE)  # Newly created, so it's empty
  }

  # Get the contents of the directory
  files <- list.files(path = dir_path, all.files = TRUE, no.. = TRUE)

  # Check if directory is empty
  return(length(files) == 0)
}
