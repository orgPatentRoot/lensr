#' Search using keyword combinations in clauses (internal)
#' Used internally
#' @param query a search term or set of search terms
#' @param boolean AND/OR
#' @param q1 a second set of search terms
#' @param families Whether to restrict the search to families only.
#' @param stemming Whether to stem the terms, default is false
#' @return a url
#' @export
#'
#' @examples \dontrun{
#' # search for terms using AND
#' lens_query_("kenya", "AND", "crater lake"),
#' # restrict results to families
#' lens_query_("kenya", "AND", "crater lake", families = TRUE)}
lens_query_ <- function(query, boolean, q1, stemming = FALSE, families = "NULL"){
  # uses lens_urls_, a limited version of lens_urls
  query <- lens_urls_(query, boolean = boolean)
  q1 <- lens_urls_(query=q1, boolean = boolean)
  connector <- "+%26%26+"
  tmpbaseurl <- "https://www.lens.org/lens/search?q="
  openbr <- "%28"
  closebr <- "%29"
  # out <- paste0(quote, query, quote, connector, openbr, q1, closebr)
  out <- paste0(tmpbaseurl, query, connector, openbr, q1, closebr)
  # the above is what I want as a query. I now need to add in the controls at the end of the query

  if(families == TRUE){
    families_string <- "&f=true"
    out <- paste0(out, families_string)
  }
  if(stemming == FALSE){
  stemming <- "&st=false"
  out <- paste0(out, stemming)
  } else (out)

}
# this is OK
# https://www.lens.org/lens/search?q=%22synthetic+biology%22+%26%26+%28%22kenya%22%29
# https://www.lens.org/lens/search?q=%22synthetic+biology%22+%7C%7C+%22synthetic+genomics%22+%26%26+%28%22kenya%22%29

# testthat stemming works

#lens_query_("kenya", "AND", "crater lake") %>% print()
#[1] "https://www.lens.org/lens/search?q=%22kenya%22+%26%26+%28%22crater+lake%22%29&st=false"

#testthat families control works.
#lens_query_("kenya", "AND", "crater lake", families = TRUE) %>% print()
#[1] "https://www.lens.org/lens/search?q=%22kenya%22+%26%26+%28%22crater+lake%22%29&f=true&st=false"


# What is needed is

#1. to be able to add the base url
#2. to be able to add the other controls