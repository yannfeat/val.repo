#' @title TargetingIdeaService - Paging Request
#' @description TargetingIdeaService.Paging 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.Paging}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param pagingStartIndex Index of the first result to return. (Optional)
#' @param pagingNumberResults Maximum number of results to return. This figure is limited to 700. (Optional)
#' @details \emph{pagingNumberResults} is limited to 700. Any figure over 700 will be reduced to 700.
#' @param apiVersion Version of the API for the request
#' @return Returns the XML text for paging
#' @export
TargetingIdeaService_paging <- function(pagingStartIndex, 
																				pagingNumberResults, 
																				apiVersion){
	if (is.null(pagingStartIndex)) {
		pagingStartIndex <- 0
	}
	
	if (is.null(pagingNumberResults)) {
		pagingNumberResults <- 700
	}
	
	if (pagingNumberResults > 700) {
		pagingNumberResults <- 700
	}
	
	xmlPaging <- paste0('<paging xmlns:cm="https://adwords.google.com/api/adwords/cm/', 
												apiVersion, 
												'" xsi:type="cm:Paging">
												<cm:startIndex>',
												pagingStartIndex,
												'</cm:startIndex>
												<cm:numberResults>',
												pagingNumberResults,
												'</cm:numberResults>
											</paging>'
											)

	xmlPaging
}

#' @title TargetingIdeaService - Attribute Types
#' @description TargetingIdeaService.AttributeType 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.AttributeType}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param attributeTypes Vector of Attributes Types.
#' @return Returns the XML text for attributeTypes
#' @export
TargetingIdeaService_requestedAttributeTypes <- function(attributeTypes){
	if (all(attributeTypes %in% c("CATEGORY_PRODUCTS_AND_SERVICES",
																"COMPETITION", 
																"EXTRACTED_FROM_WEBPAGE", 
																"IDEA_TYPE",
																"KEYWORD_TEXT", 
																"SEARCH_VOLUME", 
																"AVERAGE_CPC",
																"TARGETED_MONTHLY_SEARCHES"))) {
		xmlRequestedAttributeTypes <- paste0('<requestedAttributeTypes>', 
																				 attributeTypes, 
																				 '</requestedAttributeTypes>', 
																				 collapse="")
		xmlRequestedAttributeTypes
	} else {
		stop("Invalid Fields: ", paste0(attributeTypes[!(attributeTypes %in% c("CATEGORY_PRODUCTS_AND_SERVICES",
																																					 "COMPETITION", 
																																					 "EXTRACTED_FROM_WEBPAGE", 
																																					 "IDEA_TYPE", 
																																					 "KEYWORD_TEXT", 
																																					 "SEARCH_VOLUME", 
																																					 "AVERAGE_CPC",
																																					 "TARGETED_MONTHLY_SEARCHES"
																																					 )
																																					)],
																		collapse = ", "))
	}
}


#' @title TargetingIdeaService - Request Types
#' @description TargetingIdeaService.RequestType 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.RequestType}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param requestType Represents the type of the request. 
#' @details The \emph{Request Type} is limited to "IDEAS" and "STATS".
#' @details "IDEAS" is currently unsupported.
#' @return Returns the XML text for request type.
#' @export
TargetingIdeaService_requestType <- function(requestType){
	if (requestType %in% c("IDEAS", "STATS") & 
			length(requestType) == 1) {
		if (requestType == "IDEAS") {
			stop("Request Type 'IDEAS' is currently unsupported.")
		} else {
			xmlRequestTypes <- paste0('<requestType>',
																requestType,
																'</requestType>'
																)
			xmlRequestTypes
		}
	} else {
		if (length(requestType) != 1) {
			stop("Invalid number of request types.")
		} else {
			stop(paste0("Invalid Fields: ", requestType, collapse = ", "))
		}
	}
}

#' @title TargetingIdeaService - Idea Types
#' @description TargetingIdeaService.IdeaType 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.IdeaType}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @details Idea Type is currently limited to "KEYWORD" only.
#' @return Returns the XML text for Idea Type. By default, the Idea Type will be "KEYWORD" as this is the only option.
#' @export
TargetingIdeaService_ideaType <- function(){
	ideaType <- "KEYWORD"
	xmlIdeaType <- paste0('<ideaType>',
													ideaType,
												'</ideaType>'
												)
	xmlIdeaType
}

#' @title TargetingIdeaService - Related to Query Search Parameter
#' @description TargetingIdeaService.RelatedToQuerySearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.RelatedToQuerySearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param relatedToQuery The vector of queries to post to the API as a Search Parameter.
#' @return Returns the XML text for RelatedToQuerySearchParameter Search Parameter
#' @export
TargetingIdeaService_relatedToQuerySearchParameter <- function(relatedToQuery){
	xmlListOfQueries <- paste0('<queries>', relatedToQuery, '</queries>', collapse = "")
	xmlRelatedToQuerySearchParameter <- paste0('<searchParameters xsi:type="RelatedToQuerySearchParameter">',
																						 xmlListOfQueries,
																						 '</searchParameters>')
	xmlRelatedToQuerySearchParameter
}

#' @title TargetingIdeaService - Category Products and Services Search Parameter
#' @description TargetingIdeaService.CategoryProductsAndServicesSearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.CategoryProductsAndServicesSearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param categoryProductsAndServices A keyword category ID (Integer) in the "Products and Services" taxonomy that all search results should belong to.
#' @details Idea Type supported: KEYWORD
#' @details Request Type supported: IDEAS
#' @return Returns the XML text for CategoryProductsAndServicesSearchParameter Search Parameter
#' @usage TargetingIdeaService_categoryProductsAndServicesSearchParameter(
#' categoryProductsAndServices)
#' @export
TargetingIdeaService_categoryProductsAndServicesSearchParameter <- function(categoryProductsAndServices){
	xmlCategoryId <- paste0('<categoryId>', 
													categoryProductsAndServices, 
													'</categoryId>', 
													collapse = "")
	xmlCategoryProductsAndServicesSearchParameter <- paste0('<searchParameters xsi:type="CategoryProductsAndServicesSearchParameter">',
																														xmlCategoryId,
																													'</searchParameters>')
	xmlCategoryProductsAndServicesSearchParameter
}

#' @title TargetingIdeaService - Competition Search Parameter
#' @description TargetingIdeaService.CompetitionSearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.CompetitionSearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param competition Vector of Levels of competition that should be included in the results
#' @details LOW - competition rate [0.0000, 0.3333]
#' @details MEDIUM - competition rate (0.3333, 0.6667]
#' @details HIGH - competition rate (0.6667, 1.0000]
#' @return This is not currently supported.
#' @export
TargetingIdeaService_competitionSearchParameter <- function(competition){
	competitionLevels <- c("LOW", "MEDIUM", "HIGH")
	if (all(competition %in% competitionLevels)) {
		stop("Competition Search Parameter is currently unsupported.")
		
		#xmlLevels <- paste0('<levels>', TargetingIdeaService_competition, '</levels>', collapse = "")
		#xmlCompetitionSearchParameter <- paste0('<searchParameters xsi:type="CompetitionSearchParameter">',
		#																				xmlLevels,
		#																				'</searchParameters>')
		#xmlCategoryProductsAndServicesSearchParameter
	} else {
		stop("Invalid levels present.")
	}
}

#' @title TargetingIdeaService - Indea Text Filter Search Parameter
#' @description TargetingIdeaService.IdeaTextFilterSearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.IdeaTextFilterSearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param ideaTextFilter_included The vector of strings that should be included
#' @param ideaTextFilter_excluded The vector of strings that should be excluded
#' @details A maximum 200 included/excluded strings can be used
#' @return Returns the XML text for IdeaTextFilterSearchParameter Search Parameter
#' @export
TargetingIdeaService_ideaTextFilterSearchParameter <- function(ideaTextFilter_included, 
																															 ideaTextFilter_excluded){
	if (is.null(ideaTextFilter_included)) {
		xmlListOfIncluded <- paste0("")
	} else {
		xmlListOfIncluded <- paste0('<included>', 
																ideaTextFilter_included, 
																'</included>', 
																collapse = "")
	}
	
	if (is.null(ideaTextFilter_excluded)) {
		xmlListOfExcluded <- paste0("")
	} else {
		xmlListOfExcluded <- paste0('<excluded>', 
																ideaTextFilter_excluded, 
																'</excluded>', 
																collapse = "")
	}

	xmlIdeaTextFilterSearchParameter <- paste0('<searchParameters xsi:type="IdeaTextFilterSearchParameter">',
																						 	xmlListOfIncluded,
																						 	xmlListOfExcluded,
																						 '</searchParameters>')
	xmlIdeaTextFilterSearchParameter
}

#' @title TargetingIdeaService - Include Adult Content Search Parameter
#' @description TargetingIdeaService.IncludeAdultContentSearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.IncludeAdultContentSearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @details If used, this function specifies whether adult content should be returned.
#' @return Returns the XML text for IncludeAdultContentSearchParameter Search Parameter if Adult content is requested to be included.
#' @export
TargetingIdeaService_includeAdultContentSearchParameter <- function(){
	xmlIncludeAdultContent <- paste0('<searchParameters xsi:type="IncludeAdultContentSearchParameter"></searchParameters>')
	xmlIncludeAdultContent
}

#' @title TargetingIdeaService - Language Search Parameter
#' @description TargetingIdeaService.LanguageSearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.LanguageSearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param language The Language (or Criterion ID) of the desired languages being targeted in the results.
#' @param apiVersion The API version.
#' @details Multiple languages can be chosen, as a list. This must be in vector format.
#' @return Returns the XML text for LanguageSearchParameter Search Parameter
#' @export
TargetingIdeaService_languageSearchParameter <- function(language, apiVersion){
	xmlListOfLanguages <- paste0('<cm:id>', language, '</cm:id>', collapse = "")
	xmlLanguage <- paste0('<searchParameters xsi:type="LanguageSearchParameter">
													<languages xmlns:cm="https://adwords.google.com/api/adwords/cm/', 
													apiVersion, 
													'" xsi:type="cm:Language">',
													xmlListOfLanguages,
													'</languages>
												</searchParameters>'
												)
	xmlLanguage
}

#' @title TargetingIdeaService - Location Search Parameter
#' @description TargetingIdeaService.LocationSearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.LocationSearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param location The location (or Criterion ID) of the desired locations being targeted in the results. 
#' @param apiVersion The API version.
#' @details Multiple locations can be chosen.
#' @return Returns the XML text for LocationSearchParameter Search Parameter
#' @export
TargetingIdeaService_locationSearchParameter <- function(location, apiVersion){
	xmlListOfLocations <- paste0('<cm:id>', location, '</cm:id>', collapse = "")
	xmlLocation <- paste0('<searchParameters xsi:type="LocationSearchParameter">
													<locations xmlns:cm="https://adwords.google.com/api/adwords/cm/', 
													apiVersion, 
													'" xsi:type="cm:Location">',
													xmlListOfLocations,
													'</locations>
												</searchParameters>'
												)
	xmlLocation
}

#' @title TargetingIdeaService - Network Search Parameter
#' @description TargetingIdeaService.NetworkSearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.NetworkSearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param network_GoogleSearch Whether results should be targeting Google Search
#' @param network_SearchNetwork Whether results should be targeting the Google Search Network (AFS)
#' @param apiVersion The API version
#' @return Returns the XML text for NetworkSearchParameter Search Parameter
#' @export
TargetingIdeaService_networkSearchParameter <- function(network_GoogleSearch,
																												network_SearchNetwork,
																												apiVersion){
	
	if(is.null(network_GoogleSearch)){
		network_GoogleSearch <- "true"
	} else if (network_GoogleSearch == TRUE) {
		network_GoogleSearch <- "true"
	} else if (network_GoogleSearch == FALSE) {
		network_GoogleSearch <- "false"
	}
	
	if(is.null(network_SearchNetwork)){
		network_SearchNetwork <- "false"
	} else if (network_SearchNetwork == TRUE) {
		network_SearchNetwork <- "true"
	} else if (network_SearchNetwork == FALSE) {
		network_SearchNetwork <- "false"
	}
	
	xmlNetworkSearchParameters <- paste0('<searchParameters xsi:type="NetworkSearchParameter">
																					<networkSetting xmlns:cm="https://adwords.google.com/api/adwords/cm/', 
																					apiVersion, 
																					'" xsi:type="cm:NetworkSetting">
																						<cm:targetGoogleSearch>', 
																			 			network_GoogleSearch,
																						'</cm:targetGoogleSearch>
																						<cm:targetSearchNetwork>', 
																						network_SearchNetwork, 
																						'</cm:targetSearchNetwork>
																					</networkSetting>
																			 </searchParameters>')
	xmlNetworkSearchParameters
} 



#' @title TargetingIdeaService - Related to URL Search Parameter
#' @description TargetingIdeaService.RelatedToUrlSearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.RelatedToUrlSearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param relatedToUrl The vector of URLs to post to the API, which results will be related to. 
#' @details For KEYWORD queries, only one URL may be submitted.
#' @return Returns the XML text for RelatedToUrlSearchParameter Search Parameter
#' @export
TargetingIdeaService_relatedToUrlSearchParameter <- function(relatedToUrl){
	xmlListOfUrls <- paste0('<urls>', relatedToUrl, '</urls>', collapse = "")
	xmlRelatedToUrlSearchParameter <- paste0('<searchParameters xsi:type="RelatedToUrlSearchParameter">',
																						 xmlListOfUrls,
																					'</searchParameters>')
	xmlRelatedToUrlSearchParameter
}


#' @title TargetingIdeaService - Search Volume Search Parameter
#' @description TargetingIdeaService.SearchVolumeSearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.SearchVolumeSearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @param searchVolumeMinimum The minimum Search Volume that should be targeted.
#' @param searchVolumeMaximum The maximum Search Volume that should be targeted.
#' @details Using both parameters returns results with search volume in this regon.
#' @return Returns the XML text for SearchVolumeSearchParameter Search Parameter
#' @export
TargetingIdeaService_searchVolumeSearchParameter <- function(searchVolumeMinimum,
																														 searchVolumeMaximum){
	if (is.null(searchVolumeMinimum)) {
		xmlMinimum <- ""
	} else {
		xmlMinimum <- paste0('<minimum>', searchVolumeMinimum, '</minimum>', collapse = "")
	}
	
	if (is.null(searchVolumeMaximum)) {
		xmlMaximum <- ""
	} else {
		xmlMaximum <- paste0('<maximum>', searchVolumeMaximum, '</maximum>', collapse = "")
	}
	
	xmlSearchVolumeSearchParameter <- paste0('<searchParameters xsi:type="SearchVolumeSearchParameter">
																					 	<operation>',
																					 	xmlMinimum,
																					 	xmlMaximum,
																					 	'</operation>
																					 </searchParameters>')
	xmlSearchVolumeSearchParameter
}


#' @title TargetingIdeaService - Seed Ad Group Id Search Parameter
#' @description TargetingIdeaService.SeedAdGroupIdSearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.SeedAdGroupIdSearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @return Returns the XML text for SeedAdGroupIdSearchParameter Search Parameter
#' @param seedAdGroupId The Adgroup ID that should be used as a seed for generating new ideas.
#' @export
TargetingIdeaService_seedAdGroupIdSearchParameter <- function(seedAdGroupId){
	xmlAdGroupId <- paste0('<categoryId>', seedAdGroupId, '</categoryId>', collapse = "")
	xmlSeedAdGroupIdSearchParameter <- paste0('<searchParameters xsi:type="SeedAdGroupIdsSearchParameter">',
																						xmlAdGroupId,
																						'</searchParameters>')
	xmlSeedAdGroupIdSearchParameter
}

#' @title TargetingIdeaService - Search Parameter
#' @description TargetingIdeaService.SeedAdGroupIdSearchParameter 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.SearchParameter}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @inheritParams TargetingIdeaService_categoryProductsAndServicesSearchParameter
#' @inheritParams TargetingIdeaService_competitionSearchParameter
#' @inheritParams TargetingIdeaService_ideaTextFilterSearchParameter
#' @inheritParams TargetingIdeaService_relatedToQuerySearchParameter
#' @inheritParams TargetingIdeaService_relatedToUrlSearchParameter
#' @inheritParams TargetingIdeaService_searchVolumeSearchParameter
#' @inheritParams TargetingIdeaService_seedAdGroupIdSearchParameter
#' @param language The Language (or Criterion ID) that all results should be targeted against. 
#' @param location The Location (or Criterion ID) that all results should be targeted against. 
#' @param network_GoogleSearch Whether results should be targeting Google Search
#' @param network_SearchNetwork Whether results should be targeting the Google Search Network (AFS)
#' @param includeAdultContent TRUE or FALSE option for whether results would include the targeting of Adult Content.
#' @param apiVersion The version of the API
#' @return Returns the XML text for all of the requested Search Parameters
#' @export
TargetingIdeaService_searchParameters <- function(categoryProductsAndServices,
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
																									apiVersion){
	
	if (is.null(categoryProductsAndServices)) {
		xmlCategoryProductsAndServices <- ""
	} else {
		xmlCategoryProductsAndServices <- 
			TargetingIdeaService_categoryProductsAndServicesSearchParameter(categoryProductsAndServices)
	}
	
	if (is.null(competition)) {
		xmlCompetition <- ""
	} else {
		message("Competition currently unsupported")
		xmlCompetition <- ""
	}
	
	if (!is.null(ideaTextFilter_included) | !is.null(ideaTextFilter_excluded)) {
		xmlIdeaTextFilter <- TargetingIdeaService_ideaTextFilterSearchParameter(
			ideaTextFilter_included = ideaTextFilter_included,
			ideaTextFilter_excluded = ideaTextFilter_excluded
		)
	} else {
		xmlIdeaTextFilter <- ""
	}
	
	if (is.null(includeAdultContent)) {
		xmlIncludeAdultContent <- ""
	} else if (includeAdultContent == TRUE) {
		xmlIncludeAdultContent <- TargetingIdeaService_includeAdultContentSearchParameter()
	} else {
		xmlIncludeAdultContent <- ""
	}
	
	if (is.null(language)) {
		language <- "1000"
		xmlLanguage <- TargetingIdeaService_languageSearchParameter(language, 
																																apiVersion)
	} else {
		xmlLanguage <- TargetingIdeaService_languageSearchParameter(language, 
																																apiVersion)
	}
	
	if (is.null(location)) {
		location <- "2826"
		xmlLocation <- TargetingIdeaService_locationSearchParameter(location,
																																apiVersion)
	} else {
		xmlLocation <- TargetingIdeaService_locationSearchParameter(location,
																																apiVersion)
	}
	
	if (is.null(network_GoogleSearch) & is.null(network_SearchNetwork)) {
		xmlNetworks <- ""
	} else {
		xmlNetworks <- TargetingIdeaService_networkSearchParameter(network_GoogleSearch,
																															 network_SearchNetwork,
																															 apiVersion)
	}
	
	if (is.null(relatedToQuery)) {
		xmlQueries <- ""
	} else {
		xmlQueries <- TargetingIdeaService_relatedToQuerySearchParameter(relatedToQuery)
	}
	
	if (is.null(relatedToUrl)) {
		xmlUrls <- ""
	} else {
		xmlUrls <- TargetingIdeaService_relatedToUrlSearchParameter(relatedToUrl)
	}
	
	if (is.null(searchVolumeMinimum) &
			is.null(searchVolumeMaximum)) {
		xmlSearchVolume <- ""
	} else {
		xmlSearchVolume <- TargetingIdeaService_searchVolumeSearchParameter(searchVolumeMinimum,
																																				searchVolumeMaximum)
	}
	
	if (is.null(seedAdGroupId)) {
		xmlSeedAdGroup <- ""
	} else {
		xmlSeedAdGroup <- TargetingIdeaService_seedAdGroupIdSearchParameter(seedAdGroupId)
	}
	
	xmlSearchParameters <- paste0(xmlCategoryProductsAndServices,
																xmlCompetition,
																xmlIdeaTextFilter,
																xmlIncludeAdultContent,
																xmlLanguage,
																xmlLocation,
																xmlNetworks,
																xmlQueries,
																xmlUrls,
																xmlSearchVolume,
																xmlSeedAdGroup)
	
	xmlSearchParameters
}


#' @title TargetingIdeaService - Selector
#' @description TargetingIdeaService.TargetingIdeaSelector 
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService.TargetingIdeaSelector}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @return Returns the XML text for Targeting Idea Service Selector
#' @inheritParams TargetingIdeaService_categoryProductsAndServicesSearchParameter
#' @inheritParams TargetingIdeaService_competitionSearchParameter
#' @inheritParams TargetingIdeaService_ideaTextFilterSearchParameter
#' @inheritParams TargetingIdeaService_relatedToQuerySearchParameter
#' @inheritParams TargetingIdeaService_relatedToUrlSearchParameter
#' @inheritParams TargetingIdeaService_searchVolumeSearchParameter
#' @inheritParams TargetingIdeaService_seedAdGroupIdSearchParameter
#' @inheritParams TargetingIdeaService_requestType
#' @inheritParams TargetingIdeaService_requestedAttributeTypes
#' @inheritParams TargetingIdeaService_paging
#' @param language The Language (or Criterion ID) that all results should be targeted against. 
#' @param location The Location (or Criterion ID) that all results should be targeted against. 
#' @param network_GoogleSearch Whether search should be targeting Google Search
#' @param network_SearchNetwork Whether search should be targeting the Google Search Network (AFS)
#' @param includeAdultContent TRUE or FALSE option for including the targeting of Adult Content.
#' @details The main descriptor for choosing specified criteria.
#' @export
TargetingIdeaService_selector <- function(categoryProductsAndServices,
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
																					apiVersion){
	
	xmlSelector <- paste0('<selector>',
												TargetingIdeaService_searchParameters(categoryProductsAndServices = categoryProductsAndServices,
																															competition = competition,
																															ideaTextFilter_included = ideaTextFilter_included,
																															ideaTextFilter_excluded = ideaTextFilter_excluded,
																															includeAdultContent = includeAdultContent,
																															language = language,
																															location = location,
																															network_GoogleSearch = network_GoogleSearch,
																															network_SearchNetwork = network_SearchNetwork,
																															relatedToQuery = relatedToQuery,
																															relatedToUrl = relatedToUrl,
																															searchVolumeMinimum = searchVolumeMinimum,
																															searchVolumeMaximum = searchVolumeMaximum,
																															seedAdGroupId = seedAdGroupId,
																															apiVersion = apiVersion),
												TargetingIdeaService_ideaType(),
												TargetingIdeaService_requestType(requestType),
												TargetingIdeaService_requestedAttributeTypes(attributeTypes),
												TargetingIdeaService_paging(pagingStartIndex, 
																										pagingNumberResults,
																										apiVersion),
												'</selector>'
												)
	
	xmlSelector
	
}


#' @title TargetingIdeaService - Get
#' @description TargetingIdeaService#get
#' @references Documentation content adapted from \href{https://developers.google.com/adwords/api/docs/reference/v201802/TargetingIdeaService}{Google Adwords API documentation} under license from \href{https://creativecommons.org/licenses/by/3.0/}{CC BY 3.0}
#' @inheritParams TargetingIdeaService_categoryProductsAndServicesSearchParameter
#' @inheritParams TargetingIdeaService_competitionSearchParameter
#' @inheritParams TargetingIdeaService_ideaTextFilterSearchParameter
#' @inheritParams TargetingIdeaService_relatedToQuerySearchParameter
#' @inheritParams TargetingIdeaService_relatedToUrlSearchParameter
#' @inheritParams TargetingIdeaService_searchVolumeSearchParameter
#' @inheritParams TargetingIdeaService_seedAdGroupIdSearchParameter
#' @inheritParams TargetingIdeaService_requestType
#' @inheritParams TargetingIdeaService_requestedAttributeTypes
#' @inheritParams TargetingIdeaService_paging
#' @param language The Language (or Criterion ID) that all results should be targeted against. 
#' @param location The Location (or Criterion ID) that all results should be targeted against. 
#' @param network_GoogleSearch Whether search should be targeting Google Search
#' @param network_SearchNetwork Whether search should be targeting the Google Search Network (AFS)
#' @param includeAdultContent TRUE or FALSE option for including the targeting of Adult Content.
#' @return Returns the XML text for Targeting Idea Service Selector
#' @details The main request that returns a page of ideas that match the query described by the specified TargetingIdeaSelector.
#' @export
TargetingIdeaService_get <- function(categoryProductsAndServices,
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
																		 apiVersion){
	
	xmlGet <- paste0('<get xmlns="https://adwords.google.com/api/adwords/o/', apiVersion,'">',
									 TargetingIdeaService_selector(categoryProductsAndServices = categoryProductsAndServices,
									 															competition = competition,
									 															ideaTextFilter_included = ideaTextFilter_included,
									 															ideaTextFilter_excluded = ideaTextFilter_excluded,
									 															includeAdultContent = includeAdultContent,
									 															language = language,
									 															location = location,
									 															network_GoogleSearch = network_GoogleSearch,
									 															network_SearchNetwork = network_SearchNetwork,
									 															relatedToQuery = relatedToQuery,
									 															relatedToUrl = relatedToUrl,
									 															searchVolumeMinimum = searchVolumeMinimum,
									 															searchVolumeMaximum = searchVolumeMaximum,
									 															seedAdGroupId = seedAdGroupId,
									 															requestType = requestType,
									 															attributeTypes = attributeTypes,
									 															pagingStartIndex = pagingStartIndex, 
									 															pagingNumberResults = pagingNumberResults,
									 															apiVersion = apiVersion),
									 '</get>'
									 )
	xmlGet
}

