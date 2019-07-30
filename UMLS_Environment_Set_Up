
library(openxlsx)
library(rUMLS)
library(httr)
library('xml2')
library(rvest)
library(dplyr)
library(data.tree)
library(stringr)
library(data.table)
library(stringi)
library(gsubfn)


authBaseURL <- "https://utslogin.nlm.nih.gov"
authEndpoint <- "/cas/v1/tickets"
restBaseURL <- "https://uts-ws.nlm.nih.gov/"


umls_env <- new.env(parent = emptyenv())

#' Get a TGT token from the UMLS API.
#'
#' TGT tokens are used to generate service tickets that allow for requests to be ran against the UMLS API.
#'
#' @param name The UMLS username.
#' @param pass The UMLS password.
#'
#' @return Returns nothing, adding the TGT token to the environment.
get_TGT <- function(name = NULL, pass = NULL) {
  TGT <- umls_env$TGT
  if (is.null(TGT) && !is.null(name) && !is.null(pass)) {
    params <- list(username = name, password = pass)
    authURL <- paste0(authBaseURL, authEndpoint)
    r <- POST(url = authURL, body = params, encode = "form")
    if (status_code(r) == 201) {
      htmlResponse <- read_html(r)
      TGT <- html_attr(html_nodes(htmlResponse, "form"), "action")
      set_TGT(TGT)
      message("Authenticated.")
    } else {
      stop("Error authenticating.")
    }
  } else if (is.null(TGT) && (is.null(name) | !is.null(pass))) {
    stop("No username/password provided.")
  }
  
  TGT
}

#' @rdname get_TGT
set_TGT <- function(value) {
  umls_env$TGT <- value
}

#' @rdname get_TGT
reset_TGT <- function() {
  set_TGT(NULL)
}

#' Get a service ticket.
#'
#' @param TGT The TGT token for the current session.
#'
#' @return Returns nothing, adding the service token to the environment.
get_service_ticket <- function(TGT) {
  r <- POST(url = TGT, body = list(service = "http://umlsks.nlm.nih.gov"), encode = "form")
  htmlResponse <- read_html(r)
  ticketValue <- html_text(html_nodes(htmlResponse, "body"))
  set_service_ticket(ticketValue)
}

#' @rdname get_service_ticket
set_service_ticket <- function(value) {
  umls_env$service_ticket <- value
}

#' Authenticate against UMLS.
#'
#' @export
auth_UMLS <- function() {
  creds <- UMLS_creds()
  invisible(get_TGT(creds$user, creds$pass))
}

#' Get or set UMLS credentials.
#'
#' \code{UMLS_creds} sets or gets the UMLS credentials for the current session.
#'
#' If the environment vars \code{UMLS_USER, UMLS_PASS} have been set in \code{.Renviron}, they will be loaded. Otherwise, you will be prompted to enter a username/password.
#'
#' @export
UMLS_creds <- function(force = FALSE) {
  user <- Sys.getenv("UMLS_USER")
  pass <- Sys.getenv("UMLS_PASS")
  if (!identical(user, "") && !identical(pass, "") && !force)
    return(list(user = user, pass = pass))
  
  if (!interactive()) {
    stop("Please set env vars UMLS_USER, UMLS_PASS to your UMLS username/password respectively.", call. = FALSE)
  }
  
  message("Couldn't find env vars UMLS_USER, UMLS_PASS. See ?UMLS_creds for more details.")
  message("Please enter your username and press enter:")
  user <- readline(": ")
  message("Please enter your password and press enter:")
  pass <- readline(": ")
  if (identical(user, "") | identical(pass, "")) {
    stop("User/password entry failed", call. = FALSE)
  }
  
  message("Updating env vars.")
  Sys.setenv(UMLS_USER = user, UMLS_PASS = pass)
  
  list(user = user, pass = pass)
}

# Parser functions for various response types.
parse_results <- function(result) {
  if(status_code(result) != 200){
    NULL
  } else {
    resContent <- content(result)
    results <- resContent$result
    if (length(results) == 0) {
      NULL
    } else {
      results
    }
  }
}


exhaust_search <- function(FUN = searchFunction, PARSER = parseFunction, ...) {
  results <- list()
  curPage <- 1
  keepSearching <- TRUE
  while (keepSearching == TRUE) {
    curResult <- PARSER(FUN(..., pageNumber = curPage))
    if (!is.null(curResult)) {
      results <- c(results, list(curResult))
      curPage <- curPage + 1
    } else {
      keepSearching <- FALSE
    }
  }
  unlist(results, recursive = F)
}



search_UMLS_page_CTOS <- function(search, inputType = "sourceUi", includeObsolete = FALSE, includeSuppressible = FALSE, sabs = NULL, searchType = "words",
                                  pageNumber = 1, pageSize = 25) {
  ticket <- get_service_ticket(get_TGT())
  params <- list(ticket = ticket, string = search, includeObsolete = includeObsolete, includeSuppressible = includeSuppressible, sabs = sabs,
                 searchType = searchType, pageNumber = pageNumber, pageSize = pageSize, inputType = inputType)
  r <- GET(restBaseURL, path = "rest/search/current", query = params)
  r
}


#Function to get CUI by Condition Indication
search_UMLS_CTOS <- function(search, inputType = "sourceUi", includeObsolete = FALSE, includeSuppressible = FALSE, sabs = NULL, searchType = "words") {
  results <- exhaust_search(FUN = search_UMLS_page_CTOS, PARSER = parse_search, search = search, inputType = inputType, includeObsolete = includeObsolete,
                            includeSuppressible = includeSuppressible, sabs = sabs, searchType = searchType)
  results
}




#' @rdname get_concept_atoms
get_concept_atoms_page_CTOS <- function(CUI, sabs = NULL, ttys = NULL, language = NULL, includeObsolete = FALSE, includeSuppressible = FALSE, pageNumber = 1,
                                        pageSize = 25) {
  ticket <- get_service_ticket(get_TGT())
  
  params = list(ticket = ticket, sabs = sabs, pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/atoms"), query = params)
  r
}


#Function to get ICD10CM AND ICDNAME By CUI
get_concept_atoms_CTOS <- function(CUI, sabs = NULL, ttys = NULL, language = NULL, includeObsolete = FALSE, includeSuppressible = FALSE) {
  exhaust_search(FUN = get_concept_atoms_page_CTOS, PARSER = parse_results, CUI = CUI, sabs = sabs, ttys = ttys, language = language, includeObsolete = includeObsolete,
                 includeSuppressible = includeSuppressible)
}




parse_search <- function(result) {
  resContent <- content(result)
  results <- resContent$result$results
  if (results[[1]]$ui == "NONE") {
    NULL
  } else {
    results
  }
}

#' Get the Metathesaurus Concept-Concept relationships for a given CUI.
#'
#' @param CUI
#'
#' @return A list of Concept-Concept relationships. These are of UMLS class \code{ConceptRelation}.
#' @export
#'
#' @examples
#' # Get relationships for concept C0011884
#' relations <- get_concept_rels("C0011884")
get_concept_rels_CTOS <- function(CUI) {
  exhaust_search(FUN = get_concept_rels_page_CTOS, PARSER = parse_results, CUI = CUI)
}

get_concept_rels_page_CTOS <- function(CUI, pageNumber = 1, pageSize = 25) {
  params = list(ticket = get_service_ticket(get_TGT()), pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/relations"), query = params)
  r
}






get_TGT("username","Password")
get_service_ticket(umls_env$TGT)
auth_UMLS()

