# This is a shameless steal from tuneR "readWave" 
# TODO: (also see downpage) 
# 1) remove 'units' unless I find a use
# 2) use 'from,' 'to', to limit which frames get returned (?)
# 3) double check reading "char" format variables, i.e. single-bytes
# 4) figure out what to do if more than one of a given type exists.
# 6) make ID3 a "stdName" now that I can sorta parse it

readAiff <- function(filename, from = 1, to = Inf, ... ) {

readBigEndianInt <- function(){
        as.vector(readBin(con, "int", n = 4, size = 1, signed = FALSE) ) %*% 2^c(24, 16, 8, 0)
    }
    
    
    if(!is.character(filename))    stop("'filename' must be of type character.")
    if(length(filename) != 1) stop("Please specify exactly one 'filename'.")
      if(!file.exists(filename))     stop("File '", filename, "' does not exist.")
    if(file.access(filename, 4))  stop("No read permission for file ", filename)
  ## Open connection
      con <- file(filename, "rb")
        on.exit(close(con)) # be careful ...
    filesize <-file.info(filename)$size
    #   add names , e.g, "ID3" if found
	chunkdat <- list( COMM = NULL, SSND = NULL, MARK = NULL, INST = NULL, COMT = NULL, NAME = NULL,  AUTH = NULL, `(c) ` = NULL, ANNO  = NULL, AESD = NULL, MIDI = NULL, APPL = NULL, `ID3 ` = NULL) 
	stdNames <- names(chunkdat)  # for use later
	allNames <- stdNames	
# start with main FORM
	FORM <- readChar(con,4,useBytes=TRUE)
 #nothing <- suppressWarnings(readChar(con,4,useBytes=TRUE) ) 
 # probably just use for debugging; should be 8 smaller than 'filesize' above. 
  	contentSize <- readBin(con,'integer',1,endian='big')
 AIFF <- readChar(con,4,useBytes=TRUE)
 if(AIFF != 'AIFF') stop(c('Does not seem to be aiff file. Header was ' ,AIFF)) 
 # chunks allowed in any order, so have to read or seek everything. 
  # read in "raw" format, using each ckSize to do so.
# browser()
ckSize = list()
 while( seek(con,where=NA) < filesize  ) {
 	ckNam <- readChar(con,4,useBytes=TRUE)
 	if (!(ckNam %in% allNames)) allNames <- c(allNames, ckNam)
 	ckSize[[ckNam]] <- readBigEndianInt()  # size of data
 # now the data in the chunk 
 	chunkdat[[ckNam]] <- readBin(con,'raw',n=ckSize[[ckNam]], size = 1, endian='big')
} #end of loading chunks
# get audio parameters
# I read ckSize before reading the chunk into $COMM, so don't read it here!
# Note: if COMM ever isn't 18 this may have issues
numChan <- readBin(chunkdat$COMM,'integer',1,2,endian='big')
numFrame <- readBin(chunkdat$COMM[3:6], 'integer',1, 4 ,endian='big')  
sampSize <- readBin(chunkdat$COMM[7:8], 'integer',1, 2,endian='big') 
# sample rate is 10 bytes float -- ugly
rawsamp <- readBin(chunkdat$COMM[9:18], 'raw',10,1,endian='big')
themant <- readBin(rawsamp[3:10],'int',n=8,endian='big',signed = FALSE,size = 2)
binmant <- R.utils::intToBin(themant)
 binmant <- unlist(strsplit(binmant,'') )
 fristdig <- as.numeric(binmant[1])
# convert the fractional part of binmant to decimal
# Note that this will be essentially correct for anything less than ffffffffffffff... 
 decmant <- Reduce(function(x,y) x + y/2, as.numeric(binmant[-1]), right = TRUE) /2
theexp <- readBin(rawsamp[1:2],'integer',size=2,endian='big')
expterm <- 2^(theexp - 16383)   # 2^14-1
sampleRate <- expterm * (fristdig + decmant)
# stack all those into chunkdat$COMM
chunkdat$COMM <- list(numChan = numChan, numFrame = numFrame, sampleSize = sampSize, sampleRate = sampleRate)

#next: process the SSND 
if(!length(chunkdat$SSND)) stop('No audio data found (SSND) ')
soff <- readBin(chunkdat$SSND[1:4],'integer',1,4, endian='big')   #0 almost always
sblk <- readBin(chunkdat$SSND[5:8],'integer',1,4, endian='big')	#0 almost always
# sampsize is bits, so divide by 8, and skip the offset and blocksize words
wavdat <- readBin(chunkdat$SSND[9:(length(chunkdat$SSND) ) ], 'integer', (ckSize$SSND-8)/(sampSize/8), sampSize/8, endian='big'  )  
#  delete offset; rearrange wavdat into array w/ one channel per column; 
# I don't see a reason to use sblk since I'm only deconstructing.
if(soff >0) wavdat <- wavdat[-(1:soff)]
wavdat <- matrix(wavdat, ncol=numChan, byrow=TRUE)
# now cut out bits if from, to are not 1, Inf
if ( from > 1 || is.finite(to)) {
	wavdat <- wavdat[max(1,floor(from)) : min(floor(to), nrow(wavdat)), ]
}
chunkdat$SSND <- list(offset = soff, blocksize = sblk, audio = wavdat)

# make a vector of chunk names which have data
haveData <- vector(mode = 'character')
for (jn in 1:length(allNames) ) {
	if(length(chunkdat[[allNames[jn]]])) haveData <- c(haveData, allNames[jn])
}
chunkdat$haveData <- haveData
# then for each of the 'known' names(chunkdat) , if(length) process

# This repurposes allNames as list of other chunks to be processed if/when possible.
# Not implemented at this time as we wouldn't know how to process the data anyway
others <- setdiff(allNames, stdNames)
# for (jo in 1:length(others)) {
# # process what is possible; and when source size is null, remove from chunknames
	# if(!length(chunkdat[[others[jo]]]))	{
		# allNames <- setdiff(allNames,others[jo])
	# } 
#}
# run thru known names
if(length(chunkdat$MARK)) {
	numMark <- readBin(chunkdat$MARK[1:2],'integer',1,2)
	MarkerId=rep(0,times=numMark)
	position=rep(0,times=numMark)
	pstring=rep('0',times=numMark)
	midx = 2 # past numMark
	for (jm in 1:numMark) {
		MarkerId[jm] <- readBin(chunkdat$MARK[(1:2)+midx],'integer',1,2) # check size
		position[jm] <- readBin(chunkdat$MARK[(3:6)+midx], 'integer',1,4)
		stringlen <- readBin(chunkdat$MARK[(7)+midx], 'integer',1,1) #single byte
		endidx <- (8+midx+stringlen)		 
		pstring[jm]<- readBin(chunkdat$MARK[(8+midx):endidx],'character',stringlen/4  ,1)
		midx <- endidx
	}
	chunkdat$MARK <- list(MarkerId=MarkerId, Mposition=position, Mname=pstring)
 }
if(length(chunkdat$INST)) {
	sixchar <- readBin(chunkdat$INST[1:6],'integer',6,1) # C lang "char"
	gain <- readBin(chunkdat$INST[7:8],'integer',1,2)
	# get Loop struct
	playMode <- readBin(chunkdat$INST[9:10],'integer',1,2)
	beginLoop <- readBin(chunkdat$INST[11:12],'integer',1,2) #MarkerId
	endLoop <- readBin(chunkdat$INST[13:14],'integer',1,2) #MarkerId
	sustainLoop <- c(playMode, beginLoop, endLoop)
	#repeat for release
	playMode <- readBin(chunkdat$INST[15:16],'integer',1,2)
	beginLoop <- readBin(chunkdat$INST[17:18],'integer',1,2) #MarkerId
	endLoop <- readBin(chunkdat$INST[19:20],'integer',1,2) #MarkerId
	releaseLoop <- c(playMode, beginLoop, endLoop)
	chunkdat$INST <- list( baseNote = sixchar[1], detune = sixchar[2], lowNote = sixchar[3], highNote = sixchar[4], lowVelocity = sixchar[5], highVelocity = sixchar[6], gain = gain, sustainLoop = sustainLoop, releaseLoop=releaseLoop)
}
if(length(chunkdat$COMT)) {
	numCm <- readBin(chunkdat$COMT[1:4],'numeric',1,4) 
	cmts <- readBin(chunkdat$COMT[5:ckSize$COMT])
	chunkdat$COMT <- list('nbr' = numCm, 'comments' = cmts)
}
if(length(chunkdat$NAME)) {
	chunkdat$NAME <- readBin(chunkdat$NAME,'character',1, ckSize$NAME)
}
if(length(chunkdat$AUTH)) {
	chunkdat$AUTH <- readBin(chunkdat$AUTH,'character', 1, ckSize$AUTH)
}
if(length(chunkdat$`(c) `)) {
		chunkdat$`(c) ` <- readBin(chunkdat$`(c) `,'character', 1, ckSize$`(c) `)

}
if(length(chunkdat$ANNO)) {
		chunkdat$ANNO <- readBin(chunkdat$ANNO,'character', 1, ckSize$ANNO, 1)

}
if(length(chunkdat$AESD)) {
	chunkdat$AESD <- readBin(chunkdat$AESD,'integer',ckSize$AESD, 1)  # one-byte vals
}

if(length(chunkdat$MIDI)) {
		chunkdat$MIDI <- readBin(chunkdat$MIDI,'integer',ckSize$MIDI, 1) # one-byte vals

}
if(length(chunkdat$APPL)) {
	OStype <- readBin(chunkdat$APPL[1:4],'character',1,4)
	apdat <- readBin(chunkdat$APPL[5:ckSize$APPL], 'raw',ckSize$APPL,1)
}
if (length(chunkdat$`ID3 `)){
	# we know it's ID3 so skip first 12 bytes
	IDver <- readBin(chunkdat$`ID3 `[4:5],'integer', 2, 1)
	IDflag <- readBin(chunkdat$`ID3 `[6],'integer', 1, 1)
# see ID3Vxx documentation: MSbits are ignored, hence the strange multipliers
	IDlen <- sum(readBin(chunkdat$`ID3 `[7:10],'integer',4, 1) *c(2^21,2^14,2^7,1)  )
	tmp3 <- vector('character')
	# IDlen ignores main header bytes 
	idx3 <- 11
	while (idx3 < (IDlen + 10) ) {
		tmpnam <- readChar(chunkdat$`ID3 `[idx3:(idx3+3)],4)
# subtract 1 since we ignore some pad
		tmplen <- readBin(chunkdat$`ID3 `[idx3 + (4:7)],'integer',1,4,endian='big') -1
		tmpenc <- readBin(chunkdat$`ID3 `[idx3 + 8] , 'integer',1,1)
		tmpmsg <- readChar(chunkdat$`ID3 `[idx3+ 10 + (1:tmplen)],tmplen)		
		tmp3 <- c(tmp3, tmpnam, tmpenc, tmpmsg)
		idx3 <- idx3+ 10 + (tmplen) + 1		
	}
tmp3 <- matrix(tmp3,ncol=3, byrow= TRUE)
colnames(tmp3) <- c('tag', 'encode','message')	
chunkdat$`ID3 ` <- list(IDver = IDver, IDflag = IDflag, data = tmp3)
}
# put in the unknowns set
chunkdat$unknown <- others
class(chunkdat) <- c(class(chunkdat), 'Aiff')
return(invisible(chunkdat))
}
