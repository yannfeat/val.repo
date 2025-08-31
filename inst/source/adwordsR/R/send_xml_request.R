#' @title Send XML Request
#' @description Send XML Get Request to get a XML response.
#' @param adwordsService The Adwords service that is being requested
#' @param xmlEnvelope The full XML request to be sent to the Adwords API
#' @param saveNewToken Option to save the new access token if access token has been refreshed. (Optional)
#' @param addGitignore Option to add new access token to Gitignore if required. (Optional)
#' @param credentials The list that contains the access token. This does not need to be valid.
#' @param apiVersion The version of the API being requested. Defaults to v201806.
#' @details Please note that sending your XML request to the API does check and refresh the token automatically, if necessary.
#' @return Returns the XML output from the API request.
#' @export
getXmlRequest <- function(adwordsService, xmlEnvelope, credentials, saveNewToken = NULL, addGitignore = NULL, apiVersion = NULL) {
	credentials <- checkAdwordsToken(credentials, saveNewToken, addGitignore)
	
	if (is.null(apiVersion)) {
    apiVersion <- "v201806"
	}
	
	if (adwordsService %in% c("ManagedCustomerService")) {
		wsdlNamespace <- "mcm"
	} else if (adwordsService %in% c("TargetingIdeaService")) {
		wsdlNamespace <- "o"
	}
	
	RCurl::getURL(paste0('https://adwords.google.com/api/adwords/', 
											 wsdlNamespace,
											 '/', 
											 apiVersion, 
											 '/', 
											 adwordsService, 
											 '/'),
								httpheader = c(Authorization = paste(credentials$accessToken$token_type, credentials$accessToken$access_token)),
								postfields = xmlEnvelope,
								verbose = FALSE
	)
}
