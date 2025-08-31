#' @title Customer Manager Service Get Request
#' @description ManagedCustomerService.get    
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/ManagedCustomerService}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param ManagedCustomerService_Selector_fields The fields required from the ManagedCustomerService API
#' @param apiVersion Version of the API for the request. Argument is passed through from buildXmlEnvelope
#' @return Returns the XML text for the .get request
#' @examples
#' ManagedCustomerService_get(c("Name", "CustomerId"), "v201806")
#' @export
ManagedCustomerService_get <- function(ManagedCustomerService_Selector_fields, apiVersion) {
	paste0(
		'<get xmlns="https://adwords.google.com/api/adwords/mcm/', apiVersion, '">\n',
		ManagedCustomerService_serviceSelector(ManagedCustomerService_Selector_fields, apiVersion),
		'\n</get>'
	)
}

#' @title Customer Manager Service Selector
#' @description ManagedCustomerService.serviceSelector
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/ManagedCustomerService.Selector}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @inheritParams ManagedCustomerService_get
#' @return Returns the XML text for the Service Selector
#' @details This is used to specify the type of information to return.
#' @examples
#' ManagedCustomerService_get(c("Name", "CustomerId"), "v201806")
#' @export
ManagedCustomerService_serviceSelector <- function(ManagedCustomerService_Selector_fields, apiVersion) {
	if (all(ManagedCustomerService_Selector_fields %in% c("AccountLabels", "CanManageClients", "CurrencyCode", "CustomerId", "DateTimeZone", "Name", "TestAccount"))){
		fields <- paste0(lapply(ManagedCustomerService_Selector_fields, function(x){
														paste0("<cm:fields>", x, "</cm:fields>")
														}), collapse = "\n")
		paste0(
			'<serviceSelector xmlns:cm="https://adwords.google.com/api/adwords/cm/', apiVersion, '">\n',
			fields,
			'\n</serviceSelector>'
		)
	} else {
		x <- ManagedCustomerService_Selector_fields
		stop("Invalid Fields: ", paste0(x[!(x %in% c("AccountLabels", "CanManageClients", "CurrencyCode", "CustomerId", "DateTimeZone", "Name", "TestAccount"))], collapse = ", "))
	}

}
