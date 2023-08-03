#' Fetch All Records in an Airtable
#'
#' Airtable limits the number of records that can be pulled from a base to 100.
#' This function pulls records based on a query then checks if there is an
#' offset value. While there is an offset value, it uses that value to generate
#' the next query, thus moving down the records until all records have been
#' fetched from the database.
#'
#' @section Adding airtable API key to your environment:
#' Airtable requires an api key to fetch data.
#' Generate the airtable API key from your [Airtable account](http://airtable.com/account) page.
#'
#' \code{airtabler} functions will read the API key from
#' environment variable \code{AIRTABLE_API_KEY}. To start R session with the
#' initialized environment variable create an \code{.Renviron} file in your
#' home directory with a line like this:
#'
#' \code{AIRTABLE_API_KEY=your_api_key_here}
#'
#' You can use \code{usethis::edit_r_environ()} to open and edit your .Renviron
#' file.
#'
#' Also consider using the `dotenv` package with a .env file for storing
#' sensitive variables. Remember add to.gitignore or encrypt the .env file to
#' avoid sharing sensitive variables.
#'
#' @param base String. ID for the base or app to be fetched
#' @param table_name String. Name of the table to be fetched from the base
#' @param ... Additional arguments to pass to [air_get()]. \code{view} is a
#' commonly used additional argument.
#'
#' @return dataframe
#' @export fetch_all
#'
#' @examples
#' # Each base has a fully described API
#' # app_id <- "appVjIfAo8AJlfTkx" # ID for the base we are fetching.
#' # Note that you can pass a `view` argument to air_get or fetch_all to get only
#' # a view of a table (say, only validated records, or some other filtered view),
#' # e.g.,
#' # bats <- fetch_all(app_id, "images", view = "Status View")
#' # talks <- fetch_all(app_id, "images")
#'
fetch_all <- function(base, table_name, ...) {
  out <- list()
  out[[1]] <- airtabler::air_get(base, table_name, combined_result = FALSE,...)
  if(length(out[[1]]) == 0){
    emptyTableMessage <- glue::glue("The queried view for {table_name} in {base} is empty")
    warning(emptyTableMessage)
    return(emptyTableMessage)
  } else {
    offset <- airtabler::get_offset(out[[1]])
    while (!is.null(offset)) {
      out <- c(out, list(airtabler::air_get(base, table_name, combined_result = FALSE, offset = offset, ...)))
      offset <- airtabler::get_offset(out[[length(out)]])
    }
    out <- dplyr::bind_rows(out)
    cbind(id = out$id, out$fields, createdTime = out$createdTime,
          stringsAsFactors = FALSE)
  }
}
