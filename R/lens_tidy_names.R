#' @title Tidy column names for a dataset imported from the Lens
#' @description Converts column names to lower case, spaces to underscores and removes brackets from datasets downloaded from the Lens patent database.
#' @param df A data frame imported from a download of a Lens Collection.
#'
#' @return a data.frame
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom dplyr %>%
#' @examples \dontrun{lens_tidy_names(synbio, boolean = "OR", type = "title", rank_family = TRUE)}
lens_tidy_names <- function(df){
  names(df) <- tolower(names(df))
  names(df) <- stringr::str_replace_all(names(df), " ", "_") %>%
    stringr::str_replace_all("-", "_") %>%
    stringr::str_replace_all("[(]", "") %>%
    stringr::str_replace_all("[])]", "")
  df
}
