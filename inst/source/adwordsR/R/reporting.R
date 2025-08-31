#' @title AWQL Statement
#' @description Build the AWQL statement before posting to the API
#' @param reportType The report being queries.
#' @param startDate The start date for the request.
#' @param endDate The end date for the request.
#' @param attributes The attributes of the report.
#' @param segment The segment of the report.
#' @param metrics The metrics of the report.
#' @param where The conditions of the report. (Optional) 
#' @details Arguments attributes, segment and metrics are not currently validated so any of these can be used for a request.
#' @return Returns the statement that will be posted to the Adwords API.
#' @examples 
#' buildAwqlStatement("KEYWORDS_PERFORMANCE_REPORT", "2018-01-01", "2018-01-31", 
#'                attributes = "AdGroupName",
#'                segment = "Month",
#'                metrics = "Clicks")
#' @export
#' 
buildAwqlStatement <- function(reportType, startDate, endDate, attributes = NULL, segment = NULL, metrics = NULL, where = NULL) {
	message("Attributes, Segments and Metrics will be validated in future versions.")
	statementParameters <- paste(c(attributes, segment, metrics), collapse = ",")
	dates <- gsub("-", "", c(startDate, endDate))
	
	adwordsQuery <- paste0("__rdquery=SELECT+", statementParameters,
												 "+FROM+", reportType,
												 ifelse(is.null(where), "", paste0("+WHERE+", where)),
												 ifelse(reportType == "LABEL_REPORT", "", paste0("+DURING+", dates[[1]], ",", dates[[2]])),
												 "&__fmt=CSV")
	if (reportType == "LABEL_REPORT") {
		message("LABEL_REPORT does not support dates")
	}
	adwordsQuery
}

#' @title Retrieve Adwords Data
#' @description Retrieve the Adwords Data from the Adwords API
#' @inheritParams buildAwqlStatement
#' @param clientCustomerId The Client Customer Id to retrieve data for. This should be in the format ("123-456-7890").
#' @param credentials The credentials for accessing the Adwords API.
#' @param apiVersion The API version that will be accessed. Defaults to "v201806".
#' @param useRawEnumValues Use specified column headers used with the request, or the Adwords Display Names for column headers. Defaults to TRUE.
#' @param includeZeroImpressions Whether Entries with zero impressions should be included in the data. Defaults to FALSE.
#' @param useRequestedHeaders Use the headers used to retrieve the Adwords data or the Display names.
#' @return Returns Data for the requested report.
#' @export
getReportData <- function(reportType, startDate, endDate, clientCustomerId, credentials, attributes = NULL, segment = NULL, metrics = NULL, where = NULL, apiVersion = NULL, useRawEnumValues = NULL, includeZeroImpressions = NULL, useRequestedHeaders = NULL) {
	statement <- buildAwqlStatement(reportType, startDate, endDate, attributes, segment, metrics, where)
	statementParameters <- c(attributes, segment, metrics)
	
	skipReportHeader <- TRUE
	skipColumnHeader <- FALSE
	skipReportSummary <- TRUE
	useRawEnumValues <- ifelse(is.null(useRawEnumValues), FALSE, useRawEnumValues)
	includeZeroImpressions <- ifelse(is.null(includeZeroImpressions), FALSE, includeZeroImpressions)
	
	if (is.null(apiVersion)) {
		apiVersion <- "v201806"
	}
	
	credentials <- checkAdwordsToken(credentials)
	adwordsData <- RCurl::getURL(paste0("https://adwords.google.com/api/adwords/reportdownload/", apiVersion),
															 httpheader = c("Authorization" = paste(credentials$accessToken$token_type, credentials$accessToken$access_token),
															 							 "developerToken" = credentials$adwordsDeveloperToken,
															 							 "clientCustomerId" = clientCustomerId,
															 							 "skipReportHeader" = skipReportHeader,
															 							 "skipColumnHeader" = skipColumnHeader,
															 							 "skipReportSummary" = skipReportSummary,
															 							 "useRawEnumValues" = useRawEnumValues,
															 							 "includeZeroImpressions" = includeZeroImpressions),
															 postfields = statement,
															 verbose = FALSE,
															 ssl.verifypeer = TRUE)
	
	adwordsData <- utils::read.csv(text = adwordsData, header = T)
	if ("Day" %in% colnames(adwordsData)) {
		adwordsData$Day <- as.Date(adwordsData$Day)
	}
	
	if (is.null(useRequestedHeaders)) {
		message("By default, requested column names are going to be used.")
		names(adwordsData) <- statementParameters
	} else if (useRequestedHeaders == TRUE) {
		names(adwordsData) <- statementParameters
	} else if (useRequestedHeaders == FALSE) {
		message("Display names used for column headers.")
	}
	
	i <- sapply(adwordsData, is.factor)
	adwordsData[i] <- lapply(adwordsData[i], as.character)
	
	adwordsData
}




