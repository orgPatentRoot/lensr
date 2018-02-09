#' @title Search the Lens using International Patent Classification (IPC) codes
#' @description Search the Lens using individual or combinations of IPC Codes. Note this function is experimental. IPC based searching is not convincing in the Lens at present (for example try entering medical preparation into the full text search field and the code A61K subclass (for medical preparations) and the result is 79). Further investigation and clarification is required.
#' @param ipc An ipc code or vector of codes (character)
#' @param ipc_boolean "AND" or "OR" (quoted)
#' @details IPC codes on the group level must include a forward slash e.g.
#'   A61K31/00 or C12N15/82 or the Lens will not display results. A forward
#'   slash is not necessary on the subclass level e.g. A61K or C12N.
#' @return a url
#' @export
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_c
#' @examples \dontrun{lens_ipcs(A61K31/00)}
#' @examples \dontrun{lens_ipcs(c("A61K31/00", "C12N15/82"), ipc_boolean = "AND")}
#' @examples \dontrun{lens_ipcs(c("A61K31/00", "C12N15/82"), ipc_boolean = "OR")}
#' @examples \dontrun{lens_ipcs(c("A61K", "C12N"), ipc_boolean = "OR")}
lens_ipcs <- function(ipc, ipc_boolean = "NULL"){
  baseurl <- "https://www.lens.org/lens/search?q="
  start <- "classification_ipcr%3A"
  #end <- ""
  #slash <- "%5C%2F"
  andlink <- "+%26%26+classification_ipcr%3A"
  orlink <- "+%7C%7C+classification_ipcr%3A"
  ipc_length <- length(ipc)
  ipc_test <- stringr::str_detect(ipc, "/")
  #test if ipc at group and subgroup level contains /. Not working at present
  if (ipc_length > 4 && ipc_test == FALSE) {
    ipc_length
    ipc_test
    message("ipc at group or subgroup level must contain a forward slash /")
  }
  if (ipc_length == 1) {
    baseurl
    start
    ipc <- stringr::str_replace_all(ipc, "/", "%5C%2F")
    query <- paste0(baseurl, start, ipc)
  }
  if (ipc_length > 1) {
    query <- stringr::str_replace_all(ipc, "/", "%5C%2F")
  }
  if (ipc_boolean == "OR") {
    baseurl
    start
    orlink
    query <- stringr::str_c(query, collapse = orlink)
    query <- paste0(baseurl, start, query)
    }
  if (ipc_boolean == "AND") {
    baseurl
    start
    andlink
    query <- stringr::str_c(query, collapse = andlink)
    query <- paste0(baseurl, start, query)
  }
  query
}