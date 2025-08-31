#' @title Build XML Envelope
#' @description Build the XML Envelope that contains the Header and Body needed for the SOAP request.
#' @inheritParams buildXmlBody
#' @inheritParams buildXmlHeader
#' @inheritParams TargetingIdeaService_get
#' @return Returns the fully built XML text ready for sending to the API.
#' @details The only required parameters are as follows: \emph{myMcc}, \emph{userAgent}, \emph{developerToken}, \emph{adwordsService}.
#' @examples
#' buildXmlEnvelope(myMcc = "123-456-7890", 
#'                  userAgent = "myUserAgent", 
#'                  developerToken = "myD3v3l0p3r70k3n",
#'                  adwordsService = "ManagedCustomerService", 
#'                  ManagedCustomerService_Selector_fields = c("Name", "CustomerId"),
#'                  apiVersion = "v201806")
#' @export
buildXmlEnvelope <- function(myMcc, 
														 userAgent, 
														 developerToken, 
														 adwordsService, 
														 ManagedCustomerService_Selector_fields = NULL,
														 categoryProductsAndServices = NULL,
														 competition = NULL,
														 ideaTextFilter_included = NULL,
														 ideaTextFilter_excluded = NULL,
														 includeAdultContent = NULL,
														 language = NULL,
														 location = NULL,
														 network_GoogleSearch = NULL,
														 network_SearchNetwork = NULL,
														 relatedToQuery = NULL,
														 relatedToUrl = NULL,
														 searchVolumeMinimum = NULL,
														 searchVolumeMaximum = NULL,
														 seedAdGroupId = NULL,
														 requestType = NULL,
														 attributeTypes = NULL,
														 pagingStartIndex = NULL, 
														 pagingNumberResults = NULL,
														 apiVersion = NULL) {
	if (is.null(apiVersion)) {
    apiVersion <- "v201806"
  }
	
	currentlySupportedServices <- c("ManagedCustomerService", 
																	"TargetingIdeaService")
	if (adwordsService %in% currentlySupportedServices) {
		if (adwordsService %in% c("ManagedCustomerService")) {
			wsdlNamespace <- "mcm"
		} else if (adwordsService %in% c("TargetingIdeaService")) {
			wsdlNamespace <- "o"
		}
		
		paste0(
			'<env:Envelope xmlns:xsd="http://www.w3.org/2001/XMLSchema" 
			xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
			xmlns:wsdl="https://adwords.google.com/api/adwords/', 
			wsdlNamespace,
			'/', 
			apiVersion, 
			'" xmlns:env="http://schemas.xmlsoap.org/soap/envelope/" 
			xmlns:cm="https://adwords.google.com/api/adwords/cm/', 
			apiVersion,
			'" xmlns:o="https://adwords.google.com/api/adwords/o/', 
			apiVersion,
			'">\n',
			buildXmlHeader(myMcc, userAgent, developerToken, apiVersion),
			buildXmlBody(adwordsService, 
									 ManagedCustomerService_Selector_fields, 
									 categoryProductsAndServices,
									 competition,
									 ideaTextFilter_included,
									 ideaTextFilter_excluded,
									 includeAdultContent,
									 language,
									 location,
									 network_GoogleSearch,
									 network_SearchNetwork,
									 relatedToQuery,
									 relatedToUrl,
									 searchVolumeMinimum,
									 searchVolumeMaximum,
									 seedAdGroupId,
									 requestType,
									 attributeTypes,
									 pagingStartIndex, 
									 pagingNumberResults,
									 apiVersion),
			'\n</env:Envelope>'
		)
	} else {
		stop(paste0(adwordsService, " is not currently supported."))
	}
}

#' @title Build XML Header
#' @description Build the XML Header for the XML Envelope.
#' @param myMcc The Manager Account (MCC) needed to access the Adwords API
#' @param userAgent The user agent required for the header of the XML request
#' @param developerToken The developerToken required for accessing the API. Found in the API settings of the Adwords Account.
#' @param apiVersion Version of the API being used.
#' @return Returns the XML Header for the Envelope
#' @examples
#' buildXmlHeader("123-456-7890", "myUserAgent", "myD3v3l0p3r70k3n", "v201806")
#' @export
buildXmlHeader <- function(myMcc, userAgent, developerToken, apiVersion) {
	paste0(
		'<env:Header>\n
		<wsdl:RequestHeader xmlns="https://adwords.google.com/api/adwords/cm/', apiVersion, '">\n
		<clientCustomerId>', myMcc, '</clientCustomerId>\n
		<userAgent>', userAgent, '</userAgent>\n
		<developerToken>', developerToken, '</developerToken>\n
		</wsdl:RequestHeader>\n
		</env:Header>'
	)
}

#' @title Build XML Body
#' @description Build the XML Body for the XML Envelope.
#' @param adwordsService The chosen Adwords Service for the request
#' @param ManagedCustomerService_Selector_fields The requested fields from the ManagedCustomerService, such as CustomerId.
#' @param apiVersion Version of the API being used.
#' @inheritParams TargetingIdeaService_get
#' @return Returns the XML Body for the Envelope
#' @examples
#' buildXmlBody("ManagedCustomerService", 
#'              c("Name", "CustomerId"), 
#'              apiVersion = "v201806")
#' @export
buildXmlBody <- function(adwordsService, 
												 ManagedCustomerService_Selector_fields = NULL, 
												 categoryProductsAndServices = NULL,
												 competition = NULL,
												 ideaTextFilter_included = NULL,
												 ideaTextFilter_excluded = NULL,
												 includeAdultContent = NULL,
												 language = NULL,
												 location = NULL,
												 network_GoogleSearch = NULL,
												 network_SearchNetwork = NULL,
												 relatedToQuery = NULL,
												 relatedToUrl = NULL,
												 searchVolumeMinimum = NULL,
												 searchVolumeMaximum = NULL,
												 seedAdGroupId = NULL,
												 requestType = NULL,
												 attributeTypes = NULL,
												 pagingStartIndex = NULL, 
												 pagingNumberResults = NULL,
												 apiVersion) {
	if (adwordsService == "ManagedCustomerService") {
		xmlBody <- paste0('<env:Body>\n',
											ManagedCustomerService_get(ManagedCustomerService_Selector_fields, 
																								 apiVersion),
											'\n</env:Body>'
											)
	} else if (adwordsService == "TargetingIdeaService") {
		xmlBody <- paste0('<env:Body>\n',
											TargetingIdeaService_get(categoryProductsAndServices,
																							 competition,
																							 ideaTextFilter_included,
																							 ideaTextFilter_excluded,
																							 includeAdultContent,
																							 language,
																							 location,
																							 network_GoogleSearch,
																							 network_SearchNetwork,
																							 relatedToQuery,
																							 relatedToUrl,
																							 searchVolumeMinimum,
																							 searchVolumeMaximum,
																							 seedAdGroupId,
																							 requestType,
																							 attributeTypes,
																							 pagingStartIndex,
																							 pagingNumberResults,
																							 apiVersion),
											'\n</env:Body>'
											)
	}
	xmlBody
}
