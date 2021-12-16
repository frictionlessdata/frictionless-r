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
#' # example_package.rda was created with the following code. Note that it must
#' # be created using a URL, otherwise all Data Resource paths will point to
#' # local paths that won't work for other users. One can load locally using:
#' # read_package(system.file("extdata", "datapackage.json", package = "frictionless"))
#' example_package <- read_package(file.path(
#'   "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
#'   "main/inst/extdata/datapackage.json"
#' ))
#' save(example_package, file = "data/example_package.rda")
#' }
"example_package"
