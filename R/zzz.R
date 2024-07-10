# It is recommended that functions in a package are not memoised at build-time,
# but when the package is loaded. This is advice from the memoise documentation.

.onLoad <- function(libname, pkgname) {
  fromJSON_cache <<- memoise::memoise(jsonlite::fromJSON)
  yaml.load_file_cache <<- memoise::memoise(yaml::yaml.load_file)
}
