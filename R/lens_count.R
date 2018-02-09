#' @title Retrieve patent counts from a query to the Lens
#' @description Used for testing only. Returns the results from a query to the Lens. Useful for working out the overall number of results from a query for refinement or download.
#' @param query One or more terms to search in the patent database.
#' @param boolean Select the type of boolean ("OR" or "AND") where using multiple search terms.
#' @param type Either fulltext (default), title, abstract, claims, or title &
#'   abstract & claims (tac). Quoted.
#' @param applicant An applicant name or vector of applicant names
#' @param applicant_boolean "AND" or "OR".
#' @param inventor An inventor name or vector of inventor names.
#' @param inventor_boolean "AND" or "OR".
#' @param publn_date_start The publication date as the starting point for searching. (YYYYMMDD)
#' @param publn_date_end The publication date to search until (YYYYMMDD).
#' @param filing_date_start The filing data as the starting point for searching (YYYYMMDD).
#' @param filing_date_end The filing date to search until (YYYYMMDD).
#' @param jurisdiction Limit the search to a single jurisdiction (default is
#'   all) e.g. "US" or choose inbuilt group "main" for the United States (US),
#'   European Patent Office (EP), Japan (JP) or the World Intellectual Property
#'   Organization (WO) for the Patent Cooperation Treaty.
#' @param families Set to TRUE to retrieve only counts of patent families "NULL" (default) or FALSE will return publications and families.
#' @param timer set the time between calls in seconds when using a vector of search terms. Default = 20 seconds.
#' @details Where a boolean operator ("OR" or "AND") for multiple search terms is not specified, the function will treat each as a seperate search term and return a data.frame with rows for each term. If the operator is specified the combined results are returned.
#' @note For consistency, the output of the function is always a length 3 tibble (data frame) listing publications, families and the search query. Where only families are requested (families = TRUE), publications will be recorded as "NA" in the output. Where, on rare occassions involving very low results, the Lens returns results only and no families the result is equivalent to families and publications will be copied across to families and a message provided.
#' @return data.frame
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @examples \dontrun{lens_count("drones")
#' lens_count("synthetic biology", type = "tac")
#' lens_count(c("drone", "drones"), timer = 5) %>% print()
#' lens_count(c("drone", "drones"), boolean = "AND", timer = 5) %>% print()
#' lens_count(synbio, boolean = "OR", type = "tac") %>% print()
#' lens_count(c("gene drive", "gene drives"), boolean = "OR", families = TRUE, timer = 10) %>% print()}
#' @examples \dontrun{lens_count(c("gene drive", "gene drives"), boolean = "OR", families = FALSE, timer = 10) %>% print()}
lens_count <- function(query, boolean = "NULL", type = "NULL", applicant = NULL, applicant_boolean = "NULL", inventor = NULL, inventor_boolean = "NULL", publn_date_start = NULL, publn_date_end = NULL, filing_date_start = NULL, filing_date_end = NULL, jurisdiction = "NULL", families = "NULL", timer = 20){
# send to lens_urls to build url
  myquery <- lens_urls( query, boolean = boolean, type = type, applicant = applicant, applicant_boolean, inventor = inventor, inventor_boolean, publn_date_start = publn_date_start, publn_date_end = publn_date_end, filing_date_start = filing_date_start, filing_date_end = filing_date_end, jurisdiction = jurisdiction, families = families)
  print(myquery)
  # need to detect if only families are requested (families = TRUE) as will return only one number not two (this will be families as .resultCount). For consistency of subsetting elsewhere publications as "NA" added to tibble below.
  restrict_families <- stringr::str_detect(myquery, "&f=true")
  # return from lens_urls will always be length 1 but user may be seeking table of results on each term in vector. Enabled by length_query and if statements.
  length_query <- length(query)
  #length 1 search term or where boolean converts to length 1.
  if (length_query == 1 || boolean == "OR" || boolean == "AND") { # boolean converts strings to length 1. Where + 1 test if boolean OR or AND have been selected and proceed as for ==1.
    restrict_families <- stringr::str_detect(myquery, "&f=true")
    if (restrict_families == TRUE) {
      html <- xml2::read_html(myquery)
      publications <- "NA"
      families <- rvest::html_nodes(html, ".resultCount") %>%
        rvest::html_text() %>%
        stringr::str_extract_all("[[:digit:]]+") %>%
        as.numeric()
      search <- rvest::html_nodes(html, "#previousSearchText") %>%
          rvest::html_text() %>%
          as.character()
      df <- tibble::tibble(publications, families, search)
    }
    if (restrict_families == FALSE) {
      html <- xml2::read_html(myquery)
      detect_families <- stringr::str_detect(html, "</a> families[)]")
       publications <- rvest::html_nodes(html, ".resultCount") %>%
         rvest::html_text() %>%
         stringr::str_extract_all("[[:digit:]]+") %>%
         as.numeric()
       families <- rvest::html_nodes(html, ".breadnum:nth-child(4)") %>%
         rvest::html_text() %>%
         stringr::str_extract_all("[[:digit:]]+") %>%
         as.numeric()
       # Where results returned are small only results and no families will appear in html.
         if (detect_families == FALSE) {
           message("families not present, copying publications to families. If numbers are over 10 you may wish to check the query.")
           families <- publications
         }
      search <- rvest::html_nodes(html, "#previousSearchText") %>%
       rvest::html_text() %>%
       as.character()
      df <- tibble::tibble(publications, families, search)
    }
  }
    #cases where a boolean not used returns data frame with one row per term.
  if (length_query > 1) {
    if (boolean == "NULL") {
      df <- lens_iterate(query, lens_count, timer) %>%
        dplyr::bind_rows()
    }
}
  # # Unclear whether below is needed in light of above
  # function(df){
  #   find_zero <- df$families %in% 0
  #   df[find_zero, "families"] <- df[find_zero, "publications"]
  #   df
  #   # thanks Roman Lustrik http://stackoverflow.com/questions/22814515
  # df
  #myquery
  closeAllConnections()
  df
}