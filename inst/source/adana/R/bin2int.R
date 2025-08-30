# Convert from binary to decimal integer
bin2int = function(bin) 
  sum(2^(which(rev(unlist(
    strsplit(as.character(bin), "")) == 1))-1))