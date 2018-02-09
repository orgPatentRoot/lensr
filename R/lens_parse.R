#' @title Parse data from lens_search to a data.frame or tibble
#' @description Used internally to parse the results from lens_search.
#' @param data html from the lens retrieved using rvest read_html
#' @note By default Lens returns publication numbers as e.g. "WO 2015/199614 A1"
#'   with spaces between the country code and the kind code. This format is not
#'   used by any other database and is not used by the Lens itself for its new upload
#'   service where the format is WO_2015/199614_A1. To promote interoperability
#'   between databases Lens parse removes the spaces and also the "/" in Lens
#'   numbers. Note that the impact of the removal of "/" on retrieval from other
#'   databases requires exploration. In order to permit the upload of patent
#'   numbers back into the Lens the spaces in the Lens publication numbers are
#'   replaced with "_" as in "WO_2015/199614_A1".
#' @return a tibble.
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom tibble as_tibble
#' @importFrom stats na.omit
#' @examples \dontrun{df <- lens_urls("drones")}
#' @examples \dontrun{data <- lens_urls(synbio, boolean = "OR") %>% lens_parse()}
lens_parse <- function(data){
  html <- xml2::read_html(data)

  # extract publications, remove any blankrow
  publication_numbers_lens <- rvest::html_nodes(html, ".link") %>%
    rvest::html_text() %>%
    stringr::str_trim(side = "both") %>%
    stringr::str_replace_all(" ", "_")
  publication_numbers_lens <- publication_numbers_lens[publication_numbers_lens != ""]

  # reformat for lens upload, create link, create numbers
  publication_reformat <- stringr::str_replace_all(publication_numbers_lens, "/", "_")
  publication_link <- paste0("https://www.lens.org/lens/patent/", publication_reformat)
  publication_numbers <- publication_reformat %>%
     stringr::str_replace_all("_", "")
  # lens_id
  lens_id <- rvest::html_nodes(html, ".lens-id a") %>%
    rvest::html_text() %>%
    as.character() %>%
    paste0("https://", .)
  # # # # # applicants
  # # # # # applicants <- rvest::html_nodes(html, ".header-meta li:nth-child(5)") %>%
  # # # # #   rvest::html_text() %>%
  # # # # #   stringr::str_split(":")
  # # # # # applicants <- lapply(applicants, "[[", 2) %>%
  # # # # #   stringr::str_trim(side = "both")  %>%
  # # # # #   as.character()
  # titles
  titles <- rvest::html_nodes(html, "h3 a") %>%
    rvest::html_text() %>%
    as.character()
  # document type
  doc_type <- rvest::html_nodes(html, ".doc-type") %>%
    rvest::html_text() %>%
    stringr::str_replace_all("       \n     \t\t", "") %>%
    stringr::str_replace_all("\n    \t", "") %>%
    stringr::str_replace_all("Doc Type:", "") %>%
    stringr::str_trim(side = "both") %>%
    as.character()
  # publication_date
  publication_date <- rvest::html_nodes(html, ".header-meta li:nth-child(1)") %>%
    rvest::html_text() %>%
    stringr::str_subset("Published") %>%
    stringr::str_replace_all("Published:\t\t\t\t\t    ", "") %>%
    stringr::str_trim(side = "both") %>%
    as.character()
  # # # # #look into family_link in underlying html to follow
  # family_count
  # avoid mixed character strings in selector
  family_count <- rvest::html_nodes(html, ".header-meta li:nth-child(2) a") %>%
    rvest::html_text()
  detect_char <- stringr::str_detect(family_count, "[[:alpha:]]+")
  family_count <- tibble::tibble(family_count, detect_char) %>%
    dplyr::filter(detect_char == "FALSE") %>%
    dplyr::select_("family_count")
  length_family <- length(family_count)
  # citing count
  citing_count <- rvest::html_nodes(html, ".header-meta li:nth-child(3) a") %>%
    rvest::html_text()
  detect_char <- stringr::str_detect(citing_count, "[[:alpha:]]+")
  citing_count <- tibble::tibble(citing_count, detect_char) %>%
    dplyr::filter(detect_char == "FALSE") %>%
    dplyr::select_("citing_count")
  # #full text is .meta-full-text
  # works for cases where full text is true but not where missing
  # full_text <- rvest::html_nodes(html, ".meta-full-text") %>%
  # # # #   rvest::html_text()
  # # # # Ok but some do not have full text., needs a TRUE/FALSE column
  # # # #full_text <- stringr::str_detect(full_text, "Has Full Text")
  # # #
  # # # #   # public collection is .public-collection-icon .meta-collection
  # # # #
  # # # #   #
  # # #
  # # # # args <- publication_numbers_lens, publication_link, publication_numbers, lens_id, titles, doc_type, publication_date, family_count, citing_count

  df <- data.frame(publication_numbers_lens, publication_link, publication_numbers, lens_id, titles, doc_type, publication_date, family_count, citing_count, stringsAsFactors = FALSE)
}
