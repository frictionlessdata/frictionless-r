#' Custom method for base append to retain custom S3 classes
#'
#' When base::append is used on a datapackage object, it loses it's custom s3
#' class. This is a custom method, that retains this s3 class.
#'
#' @inheritParams base::append
#'
#' @return Data Package
#' @method append datapackage
#' @export
#' @examples
#' p <- create_package()
#' class(p)
#' p_appended <- append(p, c(new_record_to_append = "value to append"))
#' class(p_appended)
append.datapackage <- function(x, values, after = length(x)){
  # First store the class of the datapackage object, store it explicitly in case
  # it has more attributes than just list and datapackage.
  datapackage_class <- class(x)

  # Append the object
  datapackage_appended <- append(x, values, after)

  # Reset the class back to how we found it
  datapackage_appended <-
    structure(datapackage_appended, class = datapackage_class)

  # Return the datapackage with the correct class
  return(datapackage_appended)
}

