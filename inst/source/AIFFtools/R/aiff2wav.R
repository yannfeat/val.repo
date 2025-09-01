# take the list output of readAiff and create a Wave or WaveMC object
# or maybe more professionally  loadNamespace(tuneR)


aiff2wav <- function(aifsrc,  makeMC = FALSE, ...) {
if (!('Aiff' %in% class(aifsrc))) stop('Input must be of class "Aiff')
# override makeMC == FALSE if audio is > 2 columns
audio <- aifsrc$SSND$audio
sampleRate <- aifsrc$COMM$sampleRate
numChan <- aifsrc$COMM$numChan
bits <- aifsrc$COMM$sampleSize
if( !is.vector(audio) && ncol(audio) > 2 && !makeMC) {
	makeMC = TRUE
	warning('More than 2 channels; defaulting to WaveMC')
	}
if(!(bits %in% c(8, 16, 24, 32, 64))) {   	
        stop("Only 8-, 16-, 24-, 32- or 64-bit Wave formats supported")
   }
if(makeMC) {
	result <- methods::new('WaveMC')
	if(is.vector(audio) || ( ncol(audio) ==1   || nrow(audio) ==1)  ) {
		 result@.Data <- matrix(audio,ncol=1,nrow=length(audio) )
		} else {
			result@.Data <- audio  # if there were colnames, they'll be carried along	
			}	
		 } else {	
			result <- methods::new('Wave')
			if(is.vector(audio) || (ncol(audio) ==1 || nrow(audio) ==1) ) {
				 result@left <- audio
				 result@stereo = FALSE
				 } else {
					result@left <- audio[,1]
					result@right <- audio[,2]
					result@stereo = TRUE
					}
	} # end of if else wave vs MC
result@samp.rate <- sampleRate
result@bit <-bits
# leave pcm at default
	 
return(invisible(result))
}