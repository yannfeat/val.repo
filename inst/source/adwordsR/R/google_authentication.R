#' @title Google API Authentication Details
#' @description Enter the currently available Google Login Details
#' @return lists all credentials needed for loading Adwords data
#' @export
apiDetails <- function() {
	credentials <- vector("list")
	credentials$clientId <- readline(as.character(cat("Your Client ID:")))
	credentials$clientSecret <- readline(as.character(cat("Your Client Secret:")))
	credentials$adwordsDeveloperToken <- readline(as.character(cat("Your Adwords Developer Token:")))
	if (credentials$clientId == "" | credentials$clientSecret == "" | credentials$adwordsDeveloperToken == "") {
		stop(paste0("Please provide all credentials."))
	} else {
		credentials
	}
}

#' @title Initial Adwords Token
#' @description Generates the initial Adwords Token
#' @param saveNewToken Option to save the credentials to the file. (Optional)
#' @param addGitignore Option to add the credentials to the .gitignore file (Optional)
#' @return credentials entered and generates the initial token
#' @export
generateAdwordsToken <- function(saveNewToken = NULL, addGitignore = NULL) {
	credentials <- apiDetails()
	googleUrl <- paste0(
		"https://accounts.google.com/o/oauth2/auth?",
		"client_id=", credentials$clientId, "&response_type=code&",
		"scope=https%3A%2F%2Fadwords.google.com%2Fapi%2Fadwords%2F&",
		"redirect_uri=urn:ietf:wg:oauth:2.0:oob&",
		"access_type=offline&",
		"approval_prompt=force"
	)
	sslCertificate <- system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")
	utils::browseURL(googleUrl)
	credentials$clientToken <- readline(as.character(cat("Your Client Token:")))
	if (credentials$clientToken == "") {
	  stop(paste0("Please provide the Client Token."))
	}

	opts <- list(verbose = T, ssl.verifypeer = FALSE)
	credentials$accessToken <- rjson::fromJSON(
		RCurl::postForm(
			"https://accounts.google.com/o/oauth2/token",
			.opts = opts,
			code = credentials$clientToken,
			client_id = credentials$clientId,
			client_secret = credentials$clientSecret,
			redirect_uri = "urn:ietf:wg:oauth:2.0:oob",
			grant_type = "authorization_code",
			style = "POST"
		)
	)
	if (is.null(saveNewToken)) {
		message("Credentials have not been saved.")
	} else if (identical(saveNewToken, TRUE)) {
		save(credentials, file = ".googleAdwordsAuth.RData")
	} else if (identical(saveNewToken, FALSE)) {
		message("Credentials have not been saved.")
	}
	addToGitignore(addGitignore)
	credentials
}


#' @title Load Adwords Token
#' @description Loads a pre-existing token if it exists, or generates a token if it does not exist.
#' @inheritParams generateAdwordsToken
#' @return loads credentials from existing token or new token
#' @export
loadAdwordsToken <- function(saveNewToken = NULL, addGitignore = NULL) {
	if (file.exists(".googleAdwordsAuth.RData")) {
		load(".googleAdwordsAuth.RData")
	} else {
		credentials <- generateAdwordsToken(saveNewToken, addGitignore)
	}
	credentials
}

#' @title Check Adwords Token
#' @description Checks the existing Adwords token to determine whether it needs updating, and updates if necessary
#' @param credentials The current available token credentials which includes the access token
#' @inheritParams generateAdwordsToken
#' @return The credentials that will work in a request
#' @export
checkAdwordsToken <- function(credentials, saveNewToken = NULL, addGitignore = NULL) {
	currentAccessToken <- credentials$accessToken$access_token
	tokenResponse <- rjson::fromJSON(
		RCurl::getURL(
			paste0("https://www.googleapis.com/oauth2/v1/tokeninfo?access_token=", currentAccessToken)
		)
	)
	if (length(tokenResponse) == 5) {
		if (as.numeric(tokenResponse$expires_in) < 100) {
			cat(paste("Token only valid for less than 100 seconds. Token refreshing."))
			credentials <- refreshAdwordsToken(credentials, saveNewToken, addGitignore)
		} else {
			cat(paste("Token does not need refreshing."))
			credentials$accessToken$expires_in <- tokenResponse$expires_in
		}
	} else {
		cat(paste(tokenResponse$error, ":", tokenResponse$error_description, "- Token refreshing."))
		credentials <- refreshAdwordsToken(credentials, saveNewToken, addGitignore)
	}
	credentials
}

#' @title Refresh Adwords Token
#' @description Refresh the Adwords token to access the API.
#' @inheritParams checkAdwordsToken
#' @inheritParams generateAdwordsToken
#' @return The credentials containing the access token that needs updating using the refresh token
#' @export
refreshAdwordsToken <- function(credentials, saveNewToken = NULL, addGitignore = NULL) {
	refreshResponse <- rjson::fromJSON(
		RCurl::postForm(
			"https://accounts.google.com/o/oauth2/token",
			refresh_token = credentials$accessToken$refresh_token,
			client_id = credentials$clientId,
			client_secret = credentials$clientSecret,
			grant_type = "refresh_token",
			style = "POST",
			.opts = list(ssl.verifypeer = FALSE)
		)
	)
	credentials$accessToken$access_token <- refreshResponse$access_token
	credentials$accessToken$expires_in <- refreshResponse$expires_in
	credentials$accessToken$token_type <- refreshResponse$token_type
	if (identical(saveNewToken, TRUE)) {
		save(credentials, file = ".googleAdwordsAuth.RData")
		addToGitignore(addGitignore)
		message("New token saved by request.")
	} else if (identical(saveNewToken, FALSE)) {
		message("User requests that new token will not be saved.")
	} else if (is.null(saveNewToken)) {
		message("By default, new token will not be saved.")
	} else {
		message("By default, new token will not be saved.")
	}
	credentials
}

#' @title Add to .gitignore
#' @description Adds saved credentials to .gitignore so they are not saved.
#' @inheritParams generateAdwordsToken
#' @return Adds credentials to the .gitignore file.
#' @details By default, this does not add to or create a .gitignore file.
#' @export
addToGitignore <- function(addGitignore = NULL) {
	if (addGitignore == TRUE) {
		if (!file.exists(".gitignore")) {
			cat(".googleAdwordsAuth.RData", file = ".gitignore", sep = "\n")
			message(".googleAdwordsAuth.RData has been added to .gitignore")
		} else {
			cat(".googleAdwordsAuth.RData", file = ".gitignore", append = TRUE)
			message(".googleAdwordsAuth.RData has been added to .gitignore")
		}
	}	else {
		message(".googleAdwordsAuth.RData has not been added to .gitignore")
		message("This is not a recommended action.")
	}
}