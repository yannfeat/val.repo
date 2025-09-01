\name{aiff2wav}
\alias{aiff2wav}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to Convert an AIFF - Class Object to a Wave - or WaveMC - Class Object
}
\description{
This function converts an object produced with \code{readAiff} to a Wave-class object such as is generated with \code{\link[tuneR]{readWave} } . 
}
\usage{
aiff2wav(aifsrc,  makeMC = FALSE, ...)
}
\arguments{
  \item{aifsrc}{
The source object. Must be of class "Aiff" as created with \code{readAiff}
}
  \item{makeMC}{
If TRUE, force the output to be of class \code{WaveMC} regardless of the number of input channels.  However, if the input audio array has more than two channels, the value is always set to TRUE.
}

  \item{...}{
Reserved for future use   
}
 
}
\details{
Given that AIFF files are PCM encoded, the default value of the slot \code{@pcm} is TRUE and remains untouched. The slots \code{@bits, @samp.rate} are read from the source object. Note, however, that should the source bit-depth be a value unsupported by wave files, the function will exit with an error message. 
 For class "Wave" , the slot \code{@stereo} is set to FALSE if the input is a vector or single column, and to TRUE if there are two columns.  

}
\value{
A Wave or WaveMC object as defined in the \code{tuneR} package.  

}

\author{
Author and Maintainer:Carl Witthoft \email{carl@witthoft.com}

}

\seealso{
\code{readAIFF} to load an AIFF file. 

}

\examples{

samp3 <- readAiff(system.file("extdata", "sample3.aif", package = "AIFFtools") )
samp3wav <- aiff2wav(samp3)


}