# adwordsR

adwordsR: R package for accessing the Google Adwords API.

This package provides an authetication process for the Google API via OAUTH2. It also accesses the API using SOAP requests and building XML files. 
Currently, there is only the Managed Customer Service service is supported through the package, however further packages are planned to built into the package. 

## Guide

You will need an MCC account in Adwords, which can be simply created. Through your MCC, you can apply for an Adwords Developer Token, to gain access to the API. 
You will also need a Client ID and Client Secret. To get these, you will need to set up a Google API project, which will provide you with a Client ID and Secret.

### Authentication

To load or generate your Adwords Authentication token:

`credentials <- loadAdwordsToken()`

If you already have a token in your working directory, then this will be loaded using this command. If not, a token will be generated provided you enter the correct authentication details.

To check your Authentication token:

`credentials <- checkAdwordsToken()`

This will refresh the token if it has expired.


### Example: Build your requests

To access the API, you need to build your XML request. This is not necessary for the reporting side of the API. 

#### For Reporting:

Please note that this is a basic example and does not represent the full functionality. Please reference the `help` documentation for each R function.  

    credentials <- loadAdwordsToken()

    credentials <- checkAdwordsToken()

    data <- getReportData("KEYWORDS_PERFORMANCE_REPORT", 
	                      "2018-01-01", 
						  "2018-02-01", 
						  "123-123-4567", 
						  credentials, 
						  attributes = c("Criteria", 
						                 "Date", 
										 "Impressions"))
					   
#### For Other Services

Please note that this is a basic example and does not represent the full functionality. More complex XML queries can be built. Please reference the `help` documentation for each R function.  

    credentials <- loadAdwordsToken()

    xml <- buildXmlEnvelope("123-123-1234",  
	                        "myUserAgent", 
							credentials$adwordsDeveloperToken, 
							"ManagedCustomerService", 
							c("CustomerId", 
							  "Name"))					   

    credentials <- checkAdwordsToken()

    data <- getXmlRequest("ManagedCustomerService", 
	                      xml,
						  credentials)