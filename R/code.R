#' Search and stuff
#'
#' @export
#' @param query Query terms
#' @param visualize (logical) return url or make interactive viz
#' @examples
#' peasy()
#' peasy(query = "water")
#' peasy(query = "water") # choose lakes, then
#' peasy(query = "political")
#'
#' # just get URL back
#' peasy(query = "political", visualize = FALSE)

peasy <- function(query = NULL, visualize = TRUE) {
  file <- fuzzy_search(query)
  url <- make_url(file)
  message(url)
  if (visualize) {
    viz(url)
  } else {
    return(url)
  }
}

make_url <- function(x) {
  num <- as.numeric(strextract(x, "[0-9]+"))
  # type <- sub("\\.zip", "", strsplit(x, "_")[[1]][3])
  type <- gsub("\\.zip|ne_[0-9]{1,3}m_", "", x)
  cp <- physcult[[x]]
  ne_file_name(scale = num, type = type, category = cp, full_url = TRUE)
}

fuzzy_search <- function(x) {
  l1 <- find_fail(x, names(info))
  l2 <- which_one("Select one: ", opts = names(info[[l1]]))
  files <- info[[l1]][[l2]]
  which_one("Select one: ", opts = info[[l1]][[l2]])
}

find_fail <- function(x, y) {
  if (x %in% y) {
    x
  } else {
    stop(x, " not found, try again",
         call. = FALSE)
  }
}

viz <- function(x) {
  geojsonio::map_leaf(as.location(fetch(x)), basemap = "OpenStreetMap")
}

fetch <- function(url) {
  tmp <- tryCatch(geojsonio::file_to_geojson(url, output = 'shp_local'),
           error = function(e) e)
  if (is(tmp, "error")) {
    stop("Resource not found, try again", call. = FALSE)
  } else {
    tmp
  }
}

which_one <- function(..., opts) {
  cat(paste0(..., collapse = ""))
  opts[menu(opts)]
}

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
