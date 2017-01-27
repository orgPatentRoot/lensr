#' @title Search the Lens Patent Database
#' @description This function allows for the construction of complex queries to search and retrieve data from the Lens. The default search groups documents by family and will return up to 50 results per page. The maximum number of results that can be retrieved is 500 (10 pages). For larger result sets use the free Lens online Collection facility to download upto 10,000 records. See details for information on the use of ranking and date measures to sort the data.
#' @param query One or more terms to search in the patent database.
#' @param boolean Select the type of boolean ("OR" or "AND") where using multiple search terms.
#' @param type Either fulltext (default), title, abstract, claims, or "tac" for 'title or abstract or claims'.
#'   abstract and claims (tac). Quoted.
#' @param applicant An applicant name or vector of applicant names
#' @param applicant_boolean "AND" or "OR".
#' @param inventor An inventor name or vector of inventor names.
#' @param inventor_boolean "AND" or "OR".
#' @param publn_date_start Publication date limit to start at as YYYMMDD (numeric).
#' @param publn_date_end Publication date limit to end at as YYYMMDD (numeric).
#' @param filing_date_start Filing date limit to start at as YYYMMDD (numeric).
#' @param filing_date_end Filing date limit to end at as YYYMMDD (numeric).
#' @param rank_citing Whether to sort the Lens results by the top citing
#'   (descending). Useful for retrieving important documents. See details.
#' @param rank_family Whether to sort the Lens results by the number of family
#'   members (descending). Useful for retrieving important documents. See
#'   details.
#' @param rank_sequences Rank results on whether the documents contain a dna or amino
#'   acid sequence. See details.
#' @param rank_earliest_publn Sort results by the earliest publication date.
#'   See details.
#' @param rank_latest_publn Sort results by the latest publication date.
#'   Useful for retrieving the most recent documents. See details.
#' @param rank_earliest_filing Sort results by the earliest publication date. See
#'   details.
#' @param rank_latest_filing Sort results by the latest filing (priority) date.
#'   Useful for identifying the latest filings (note that they are the latest
#'   filings that have a publication). See details.
#' @param jurisdiction Limit the search to a single jurisdiction (default is
#'   all) e.g. "US" or choose inbuilt group "main" for the United States (US),
#'   European Patent Office (EP), Japan (JP) or the World Intellectual Property
#'   Organization (WO) for the Patent Cooperation Treaty.
#' @param families Either return the publication count and family numbers or if
#'   TRUE (default) return the patent families (deduplicates a set of
#'   publications to the first publication of the root "priority" or first
#'   filing).
#' @param results The number of results to return, either 50 or 500 (maximum).
#' @param timer Where retrieving over 50 results, the delay between sending requests to the Lens (default is 20 seconds, used internally by ops_iterate()).
#' @param stemming Word stemming is set to FALSE by default.
#' @details Ranking: Only one ranking measure may be used per query. For example, it is
#'   possible to rank by family scores but not family scores and latest
#'   publications or earliest publications.

#' @section Suggested Workflow: A suggested work flow is to
#'   retrieve the latest publications, then rank by family and then rank_citing.
#'   This will allow the most recent and the most important documents to be
#'   retrieved in three steps for a given query.
#'
#'   @section Inventor Names: In patent documents the convention is to list the surname (family name) and then the first names (given names). Name reversals can and do occur but normal practice is to use <surname, first name then initial> e.g. "Kirk James T" rather than "James T Kirk". However, be warned that there will be variations.
#' @return a data.frame or tibble
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @examples \dontrun{lens_search("synthetic biology", timer = 30)
#'
#' # Use boolean terms OR or AND to connect phrases
#' lens_search(synbio, boolean = "OR", timer = 30)
#' lens_search(synbio, boolean = "AND", timer = 30)
#'
#' # search title, abstract, title, abstract or claims and rank by family size
#' lens_search(synbio, boolean = "OR", type = "title", rank_family = TRUE, timer = 30)
#' lens_search(synbio, boolean = "OR", type = "abstract", rank_family = TRUE, timer = 30)
#' lens_search(synbio, boolean = "OR", type = "tac", rank_family = TRUE, timer = 30)
#'
#' # search title or abstract or claims and rank by citing
#' lens_search(synbio, boolean = "OR", type = "tac", rank_citing = TRUE, timer = 30)
#'
#' # search for keyword, inventor and applicant names
#' lens_search(query = "synthetic genomics", inventor = "Venter Craig", applicant = "Synthetic Genomics")
#' lens_search(query = "synthetic genomics", inventor = "Venter Craig")
#' lens_search(query = "synthetic genomics", applicant = "Synthetic Genomics")}
lens_search <- function(query, boolean = "NULL", type = "NULL", applicant = NULL, applicant_boolean = "NULL", inventor = NULL, inventor_boolean = "NULL", publn_date_start = NULL, publn_date_end = NULL, filing_date_start = NULL, filing_date_end = NULL, rank_family = "NULL", rank_citing = "NULL", rank_sequences = "NULL", rank_latest_publn = "NULL", rank_earliest_publn = "NULL", rank_latest_filing = "NULL", rank_earliest_filing = "NULL", jurisdiction = "NULL", families = TRUE, results = NULL, timer = 20, stemming = FALSE){
  # To do: replace args with args
  # Note that families is set to TRUE by default
    out <- lens_urls(query, boolean = boolean, type = type, applicant = applicant, applicant_boolean = applicant_boolean, inventor = inventor, inventor_boolean = inventor_boolean, publn_date_start = publn_date_start, publn_date_end = publn_date_end, filing_date_start = filing_date_start, filing_date_end = filing_date_end, rank_family = rank_family, rank_citing = rank_citing, rank_sequences = rank_sequences, rank_latest_publn = rank_latest_publn, rank_earliest_publn = rank_earliest_publn, rank_latest_filing = rank_latest_filing, rank_earliest_filing = rank_earliest_filing, jurisdiction = jurisdiction, families = families, results = results, stemming = stemming)
 length_out <- length(out)
 if(length_out > 1){
   out <- lens_iterate(out, lens_parse, timer) %>%
     dplyr::bind_rows() # bind list to df
 } else {out <- lens_iterate(out, lens_parse, timer)
 out
 #<- out[[1]] # return df
 }
 closeAllConnections()
 out[[1]]
}