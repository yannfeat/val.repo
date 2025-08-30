# Lei & Tingzhi adaptation function
leitingzhi = function(fitvals, cxpc, cxpc2, 
                      mutpm, mutpm2, adapa, adapb, ...){
  if(missing(adapa)) adapa=0.7
  if(missing(adapb)) adapb=0.5
  if(missing(cxpc)) cxpc=0.9
  if(missing(cxpc2)) cxpc2=0.5
  if(missing(mutpm)) mutpm=0.05
  if(missing(mutpm2)) mutpm2=0.2
  fmmr = min(fitvals)/max(fitvals)
  famr = mean(fitvals)/max(fitvals)
  pc = ifelse(famr>adapa & fmmr>adapb, cxpc2*(1-fmmr^-1), cxpc)
  pm = ifelse(famr>adapa & fmmr>adapb, mutpm2*(1-fmmr^-1), mutpm)
  return(list(pc=pc, pm=pm))
}