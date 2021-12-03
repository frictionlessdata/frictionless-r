#' Example Data Package
#'
#' Example Tabular
#' [Data Package](https://specs.frictionlessdata.io/data-package/) with dummy
#' camera trap data organized in 3 Data Resources:
#' - `deployments`: data stored in `deployments.csv`.
#' - `observations`: data stored in `observations_1.csv` and
#' `observations_2.csv`.
#' - `media`: data stored in `data` property.
#'
#' @source <https://github.com/frictionlessdata/frictionless-r/tree/main/inst/extdata>
#' @examples
#' \dontrun{
#' # File was created with
#' example_package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#' save(example_package, file = "data/example_package.rda")
#' }
"example_package"
