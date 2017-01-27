#' @title Iterate over lens urls and return a data.frame
#' @description Used internally in lens_search. Takes a set of urls generared by lens_urls() and uses lapply to fetch the raw data and converts it to a data.frame using lens_parse(). The lists of data.frames are bound using dplyr::bind_rows().
#' @param x A vector of urls (see lens_urls)
#' @param timer (numeric) Delay between each request to the Lens. 20 seconds as the default which may be a little slow.
#' @param f function from lensr to use within iterate e.g. lens_parse.
#' @return a data.frame
#' @export
#' @examples \dontrun{df <- lens_iterate_new(three_urls, lens_parse, timer = 10)}
lens_iterate <- function(x, f, timer = 20) {
  lens_results <- lapply(x, dot_every(1, delay_by(timer, f)))
  lens_results
}
