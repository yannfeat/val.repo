#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat mycbind(arma::colvec& X,
                  const int& l){
  int i;
  const int nrow=X.n_rows;
  arma::mat out(nrow,l);
  for(i=0; i < l; ++i){
    out.col(i) = X;
  }
  return(out);		
}

// [[Rcpp::export]]
arma::mat myrbind(arma::rowvec& X,
                  const int& l){
  int i;
  const int ncol=X.n_cols;
  arma::mat out(l,ncol);
  for(i=0; i < l; ++i){
    out.row(i) = X;
  }
  return(out);		
}

// [[Rcpp::export]]
arma::rowvec myColsums(arma::mat& X){
  //const int nrow=X.n_rows;
  const int ncol=X.n_cols;
  arma::rowvec out(ncol);
  for(int j = 0; j < ncol; ++j){
    arma::colvec cc = X.col(j);
    out(j) = arma::accu(cc);
  }
  return(out);
}

// [[Rcpp::export]]
arma::colvec myRowsums(const arma::mat & X){
  int nRows = X.n_rows;
  arma::colvec out(nRows);
  for(int i = 0; i < nRows; i++){
    out(i) = sum(X.row(i));
  }
  return(out);
}


// [[Rcpp::export]]
arma::cube hhat( 
    const arma::rowvec& g,
    const arma::mat& S,
    const arma::mat& zc,
    const arma::rowvec& dl){
  
  const int n = zc.n_rows;
  const int p = zc.n_cols;
  arma::cube ht(p, n, n);
  ht.zeros();
  
  for(int i = 0;i < n; ++i){
    //arma::mat h = exp(arma::as_scalar(g*zc.row(i).t())) * (zc.row(i).t() - S.col(i))*dl(i);
    arma::mat h(p,1);
    h.zeros();
    for(int t=i; t < n;++t){
      h += exp(arma::as_scalar(g*zc.row(i).t())) * (zc.row(i).t() - S.col(t))*dl(t);
      ht.slice(t).col(i) = h;
    }
  }
  
  return(ht);
}

// [[Rcpp::export]]

Rcpp::List mybetaestCENSORING( 
    const arma::rowvec& times, 
    const arma::rowvec& deltas,
    const arma::mat& covariates,
    const arma::rowvec& betas,
    const arma::rowvec& rhos){
  
  int i,j;
  const int nobs=covariates.n_rows;
  const int ncovs=covariates.n_cols;
  
  double loglikli=0;
  arma::colvec score(ncovs);
  score.fill(0.0);
  arma::mat infor(ncovs,ncovs);
  infor.fill(0.0);
  
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.fill(0.0);
    arma::mat s2(ncovs,ncovs);
    s2.fill(0.0);
    double ye=0;
    
    if(deltas(i)==1){
      
      arma::rowvec zi = covariates.row(i);
      loglikli +=  rhos(i) * arma::as_scalar(betas*zi.t());
      score +=  rhos(i) * zi.t();
      
      for(j=0; j<nobs; ++j){
        if(times(j) >= times(i)){ 
          arma::rowvec zj = covariates.row(j);
          ye = exp(arma::as_scalar(betas*zj.t()));
          s0 += rhos(j) * ye;
          s1 += rhos(j) * ye * zj.t();
          s2 += rhos(j) * ye * zj.t() * zj;
        }
      }
      loglikli -= rhos(i) * log(s0);
      score -= rhos(i) * s1/s0;
      infor += rhos(i) * ( s2/s0 - s1*s1.t()/(s0*s0) ) ;
    }
    
    
  }
  
  Rcpp::List res(4);
  res[0]=betas;
  res[1]=loglikli;
  res[2]=score;
  res[3]=infor;
  
  return(res);
  
}


// [[Rcpp::export]]

Rcpp::List betaestCOX( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const int& nobs, 
    const int& ncovs,
    const arma::mat& cweight, //It's a matrix when COX model is used for censoring
    const arma::rowvec& betas){
  
  int i,j;
  
  double loglikli=0;
  arma::colvec score(ncovs);
  score.fill(0.0);
  arma::mat infor(ncovs,ncovs);
  infor.fill(0.0);
  for(i=0; i<nobs; ++i){
    double s0=0;
    
    double wye=0;
    
    arma::colvec s1(ncovs);
    s1.fill(0.0);
    arma::mat s2(ncovs,ncovs);
    s2.fill(0.0);
    if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
    
    arma::rowvec zi = covariates.rows(i,i);
    loglikli += arma::as_scalar(betas*zi.t());
    score += zi.t();
    
    for(j=0; j<nobs; ++j){
      if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 where  w*Y !=0.
      arma::rowvec zj = covariates.rows(j,j);
      if(times(i) <= times(j)){
        wye = exp(arma::as_scalar(betas*zj.t()));
      }
      else if(times(i) > times(j) && causes(j)>1){ //Other causes come into play here.
        wye = exp(arma::as_scalar(betas*zj.t())) * cweight(j,i)/cweight(j,j);
      }
      s0 += wye;
      s1 += wye * zj.t();
      s2 += wye * zj.t() * zj;
    }
    loglikli -= log(s0);
    score -= s1/s0;
    infor += s2/s0 - s1*s1.t()/(s0*s0);	
    
  }
  
  Rcpp::List res(4);
  res[0]=betas;
  res[1]=loglikli;
  res[2]=score;
  res[3]=infor;
  
  return(res);
  
}



// [[Rcpp::export]]

Rcpp::List betaestKM( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const int& nobs, 
    const int& ncovs,
    const arma::rowvec& cweight,//just a vector when KM estimate is used for censoring
    const arma::rowvec& betas){
  
  int i,j;
  
  double loglikli=0;
  arma::colvec score(ncovs);
  score.fill(0.0);
  arma::mat infor(ncovs,ncovs);
  infor.fill(0.0);
  
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.fill(0.0);
    arma::mat s2(ncovs,ncovs);
    s2.fill(0.0);
    double wye=0;
    
    if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
    
    arma::rowvec zi = covariates.row(i);
    loglikli += arma::as_scalar(betas*zi.t());
    score += zi.t();
    
    for(j=0; j<nobs; ++j){
      if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 where  w*Y !=0.
      arma::rowvec zj = covariates.row(j);
      if(times(i) <= times(j)){
        wye = exp(arma::as_scalar(betas*zj.t()));
      }
      else if (times(i) > times(j) && causes(j)>1){
        wye = exp(arma::as_scalar(betas*zj.t())) * cweight(i)/cweight(j);
      }
      //else continue;
      s0 += wye;
      s1 += wye * zj.t();
      s2 += wye * zj.t() * zj;
    }
    loglikli -= log(s0);
    score -= s1/s0;
    infor += s2/s0 - s1*s1.t()/(s0*s0);
    
    
  }
  
  Rcpp::List res(4);
  res[0]=betas;
  res[1]=loglikli;
  res[2]=score;
  res[3]=infor;
  
  return(res);
  
}


// [[Rcpp::export]]

Rcpp::List mylambdaestCENSORING( 
    const arma::rowvec& times, 
    const arma::rowvec& deltas,
    const arma::mat& covariates,
    const arma::rowvec& betas,
    const arma::rowvec& rhos){
  
  int i,j;
  const int nobs=covariates.n_rows;
  const int ncovs=covariates.n_cols;
  
  //	double loglikli=0;
  //	arma::colvec score(ncovs);
  //	score.fill(0.0);
  //	arma::mat infor(ncovs,ncovs);
  //	infor.fill(0.0);
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.fill(0.0);
    arma::mat s2(ncovs,ncovs);
    s2.fill(0.0);
    double ye=0;
    
    
    arma::rowvec zi = covariates.row(i);
    
    for(j=0; j<nobs; ++j){
      if(times(j) >= times(i)){ 
        arma::rowvec zj = covariates.row(j);
        ye = exp(arma::as_scalar(betas*zj.t()));
        s0 += rhos(j) * ye;
        s1 += rhos(j) * ye * zj.t();
        s2 += rhos(j) * ye * zj.t() * zj;
      }
    }
    
    S0hat(i) = s0/nobs;
    S1hat.col(i) = s1/nobs;
    S1byS0hat.col(i) = s1/s0;
  }
  
  //dlambda_l_0(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(deltas(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += rhos(i)/(S0hat(j)*nobs);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  Rcpp::List res(1);
  res[0]=dlambdat;
  //res[1]=loglikli;
  //res[2]=score;
  //res[3]=infor;
  
  return(res);
  
}

// [[Rcpp::export]]
Rcpp::List se_beta_lambda_strata_COX_diff( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const arma::mat& cencovariates, //censoring dependent covariates
    const int& nobs, 
    const int& ntotal,
    const int& ncovs,
    const arma::mat& cweight,//matrix when cox model is used for censoring
    const arma::rowvec& betas,//Estimates for cause 1
    const arma::rowvec& gammas,//Estimates for censoring
    const arma::rowvec& cluster,
    const arma::rowvec& allcluster,
    const int& nc,
    const int& totalnc,
    arma::colvec& expg,
    const arma::mat& iIc,
    const arma::mat& iI,
    const arma::rowvec& idxtlambda,
    const arma::mat& etapsi,
    const int& ntotalobs,
    const arma::mat allcovariates,
    const arma::rowvec Zpred ){ // Added on 11/03/2021
  
  int i,j,k,t;
  
  //double loglikli=0;
  //arma::colvec score(ncovs);
  //score.fill(0.0);
  //arma::mat infor(ncovs,ncovs);
  //infor.fill(0.0);
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  //arma::cube S2byS0hat(ncovs,ncovs,nobs);
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.zeros();
    arma::mat s2(ncovs,ncovs);
    s2.zeros();
    double wye=0;
    
    //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
    
    arma::rowvec zi = covariates.row(i);
    //loglikli += arma::as_scalar(betas*zi.t());
    //score += zi.t();
    
    for(j=0; j<nobs; ++j){
      if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
      arma::rowvec zj = covariates.row(j);
      if(times(i) <= times(j)){
        wye = exp(arma::as_scalar(betas*zj.t()));
      }
      else if (times(i) > times(j) && causes(j)>1){
        wye = exp(arma::as_scalar(betas*zj.t())) * cweight(j,i)/cweight(j,j);
      }
      else continue;
      s0 += wye;
      s1 += wye * zj.t();
      s2 += wye * zj.t() * zj;
    }	
    S0hat(i) = s0/nc;
    S1hat.col(i) = s1/nc;
    S1byS0hat.col(i) = s1/s0;
    //S2byS0hat.slice(i) = s2/s0;
    
  }
  
  //dlambda10(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(causes(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += 1/(S0hat(j)*nc);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  
  
  //For Censoring(S0C(t),S1C(t),S1C(t)/S0C(t))
  const int ncovsC=cencovariates.n_cols;
  arma::rowvec S0hatC(nobs);S0hatC.zeros();
  arma::mat S1hatC(ncovsC,nobs);S1hatC.zeros();
  arma::mat S1byS0hatC(ncovsC,nobs);S1byS0hatC.zeros();
  //arma::cube S2byS0hatC(ncovsC,ncovsC,nobs);
  
  for(i=0; i<nobs; ++i){ 
    double s0C=0;
    arma::colvec s1C(ncovsC);
    s1C.zeros();
    arma::mat s2C(ncovsC,ncovsC);
    s2C.zeros();
    //double wye=0;
    
    
    
    for(j=0; j<nobs; ++j){
      if(times(j) >= times(i)){
        arma::rowvec zjC = cencovariates.row(j);
        s0C += exp(arma::as_scalar(gammas*zjC.t()));
        s1C += zjC.t() * exp(arma::as_scalar(gammas*zjC.t()));
        s2C += zjC.t() * zjC * exp(arma::as_scalar(gammas*zjC.t()));				
      }
    }	
    S0hatC(i) = s0C/nc;
    S1hatC.col(i) = s1C/nc;
    S1byS0hatC.col(i) = s1C/s0C;
    //S2byS0hatC.slice(i) = s2C/s0C;
    
  }
  
  //For Censoring(dlambdac(t))
  arma::rowvec dlambdac(nobs);dlambdac.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatempc=0;
    if(causes(j)!=0) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatempc += 1/(S0hatC(j)*nc);
      }
    }
    
    dlambdac(j)=dlambdatempc;
  }
  
  arma::cube h=hhat(gammas,S1byS0hatC,cencovariates,dlambdac);
  
  //q_i(u) and psi_i
  //Calculating wdM and dMc as a matrix.(It is efficient to do it this way for calculating q_i(u)).
  arma::mat wdM(nobs,nobs);wdM.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==1 && times(i)==times(j)){
        first = 1;
      }
      if(times(j) <= times(i)){
        second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
      else if(times(j) > times(i) && causes(i) > 1){
        second = exp(arma::as_scalar(betas*zi.t())) *cweight(i,j)/cweight(i,i)*dlambdat(j);
      }
      wdM(i,j) = first - second;
    }
  }
  
  //dMc
  arma::mat dMc(nobs,nobs);dMc.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zci = cencovariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==0 && times(i)==times(j)){
        first = 1;
      }
      if(times(i) >= times(j) && causes(j)==0){
        second = exp(arma::as_scalar(gammas*zci.t())) * dlambdac(j);
      }
      dMc(i,j) = first - second;
    }
  }
  
  //zc_min_S1byS0hatC
  //arma::colvec expg=exp(cencovariates * gammas.t());
  arma::cube zc_min_S1bS0hatc(nobs,nobs,ncovsC);
  for(j=0; j < ncovsC;++j){
    arma::colvec zc=cencovariates.col(j);
    arma::rowvec ratio = S1byS0hatC.row(j);
    zc_min_S1bS0hatc.slice(j) = mycbind(zc,nobs) - myrbind(ratio,nobs);
  }
  
  arma::mat qi21temp(nobs,nobs); qi21temp.zeros();
  arma::rowvec qi21(nobs); qi21.zeros();
  //Main loop to calculate psi
  arma::mat z_min_S1bS0hat_wdM(nobs,nobs);
  arma::rowvec qi11(ncovsC); qi11.zeros();
  arma::mat test(nobs,nobs); test.zeros();
  arma::mat precomp(1,ncovsC); precomp.zeros();
  double qi2;
  arma::vec denom;
  //arma::vec locu;
  //int locu;
  arma::colvec s0C;
  arma::mat tempzc;
  arma::mat q1_all_i(1,ncovsC);
  double qi2final;
  arma::mat qi1(1,nobs); qi1.zeros();
  arma::mat qi(nobs,nobs);
  
  
  
  //Calculating lambda(t) and standard error
  int tl=idxtlambda.n_cols;
  arma::colvec tt;
  //double tfinal;
  arma::mat oneoverS0hat_wdM(nobs,nobs); oneoverS0hat_wdM.zeros();
  arma::mat first_term_W(nobs,nobs); first_term_W.zeros();
  arma::mat second_term_W(nobs,nobs); second_term_W.zeros();
  //arma::vec W1;arma::vec W2; arma::rowvec W3; arma::vec Wall;
  arma::mat W1(nobs,tl); W1.zeros();arma::mat W2(nobs,tl); W2.zeros();arma::mat W3(tl,ntotal); W3.zeros();
  arma::mat W1Fstar(nobs,tl); W1Fstar.zeros(); arma::mat W2finalFstar(nobs,tl); W2finalFstar.zeros();arma::mat W3Fstar(tl,ntotal); W3Fstar.zeros();
  arma::vec hLambda; arma::vec hFstar; 
  arma::rowvec dlt;
  arma::uvec clusteridx;arma::uvec allclusteridx;
  arma::rowvec vLambda(tl); vLambda.zeros();
  arma::rowvec Lambda(tl); Lambda.zeros();
  arma::rowvec Fstart(tl); Fstart.zeros();
  // Added on 11/03/2021
  arma::vec hLambdapred;
  arma::mat W3pred(tl,ntotal); W3pred.zeros();
  //arma::vec test1;
  
  
  //		oneoverS0hat_wdM = 1/myrbind(S0hat,nobs) % wdM;
  
  //		for(k=0; k < ncovsC; ++k){
  //			test = h(span(k),span::all,span::all);
  //			qi11(k) = accu(test % oneoverS0hat_wdM);//Takes care of summation and integral	
  //		}
  
  //		qi21temp = mycbind(expg,nobs) % oneoverS0hat_wdM;
  //		qi21 = myColsums(qi21temp);
  //		precomp = qi11*(iIc*nc);//Omega_hatC = iIc*nc
  //		//arma::mat qi(nobs,nobs);
  //		for(i=0; i < nobs; ++i){//This loop is for u in the formula.
  //			double uu = times(i);
  //			arma::uvec idx = find(uu <= times);
  //			qi2 = accu(qi21.elem(idx));
  //			arma::uvec locu = find(uu==times);
  //			denom = S0hatC.elem(locu);
  //			double finaldenom = denom(0);//This will take care if two time points are the same.
  //			qi2final = qi2/finaldenom;
  //			//tempzc = zc_min_S1bS0hatc(span::all,span(i),span::all);
  //			//qi1 = precomp*tempzc.t();
  //			qi.col(i) = -1*(qi1.t() + qi2final)/nc;
  //		}
  
  arma::rowvec nuj(tl); 
  
  arma::rowvec CumLambda0(tl); CumLambda0.zeros();
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);
    CumLambda0(t) = arma::accu(dlambdat.cols(0,uuu-1));
  }
  
  //CumLambda0 = arma::cumsum(dlambdat);
  
  arma::mat zetaj(tl,ncovs); zetaj.zeros();
  
  //Estimate of adjusted CIF
  arma::rowvec Fstar(tl); Fstar.zeros();
  
  for(i=0; i < tl; ++i){
    double Fstartemp = 0;
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = allcovariates.row(j);
      Fstartemp += ( 1 - exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) ) );
    }
    Fstar(i) =  Fstartemp/ntotalobs;
  }
  
  //arma::rowvec nuj(nobs); nuj.zeros();
  for(i=0; i < tl; ++i){
    double nujtemp = 0;
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = allcovariates.row(j);
      nujtemp += exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) + arma::as_scalar(betas*zj.t()) ) ;
    }
    nuj(i) = nujtemp/ntotalobs;
  }
  
  
  for(i=0; i < tl; ++i){
    arma::rowvec zetajtemp(ncovs); zetajtemp.zeros();
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = allcovariates.row(j);
      zetajtemp += zj * exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) + arma::as_scalar(betas*zj.t()) ) ;
    }
    zetaj.row(i) = zetajtemp/ntotalobs;
  }
  
  //Estimate of F(t|Zpred). Added on 11/03/2021
  arma::rowvec Fpred(tl); Fpred.zeros();
  arma::rowvec Fpredfirst(tl); Fpredfirst.zeros();
  for(i=0; i < tl; ++i){
    Fpred(i) = 1 - exp(-CumLambda0(i)*exp(arma::as_scalar(betas*Zpred.t())));
    Fpredfirst(i) = exp(arma::as_scalar(betas*Zpred.t()) - CumLambda0(i)*exp(arma::as_scalar(betas*Zpred.t())));
  }
  
  
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);
    
    //Added 02/26/2021 (q^(2) is a function of t as well)
    arma::rowvec mytemp1 = S0hat.cols(0,uuu-1);
    oneoverS0hat_wdM = 1/myrbind(mytemp1,nobs) % wdM.cols(0,uuu-1);
    for(k=0; k < ncovsC; ++k){
      test = h(arma::span(k),arma::span::all,arma::span::all);
      qi11(k) = accu(test.cols(0,uuu-1) % oneoverS0hat_wdM);//Takes care of summation and integral	
    }
    qi21temp = mycbind(expg,uuu) % oneoverS0hat_wdM;
    qi21 = myColsums(qi21temp);
    precomp = qi11*(iIc*totalnc);//Omega_hatC = iIc*nc
    
    for(i=0; i < nobs; ++i){//This loop is for u in the formula.
      double uu = times(i);
      arma::uvec idx = find(uu <= times.cols(0,uuu-1));
      qi2 = accu(qi21.elem(idx));
      arma::uvec locu = find(uu==times);
      denom = S0hatC.elem(locu);
      double finaldenom = denom(0);//This will take care if two time points are the same.
      qi2final = qi2/finaldenom;
      //tempzc = zc_min_S1bS0hatc(span::all,span(i),span::all);
      //qi1 = precomp*tempzc.t();
      qi.col(i) = -1*(qi1.t() + qi2final)/nc;
      //qi.col(i) = -1*(qi1.t())/totalnc;
    }
    //Added 02/26/2021 end
    
    first_term_W = wdM/myrbind(S0hat,nobs);
    W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
    second_term_W = qi % dMc;
    //W2 = myRowsums(second_term_W.cols(0,uuu-1));
    W2.col(t) = myRowsums(second_term_W);
    dlt = dlambdat.cols(0,uuu-1);
    hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
    W3.row(t) = hLambda.t() * (iI*totalnc) * (etapsi);
    //Wall = W1 + W2 + W3.t();
    //			Wall = (W1 + W2);
    
    //			double test1=0;
    //			double test2=0;
    //			double test3=0;
    //			for(j=1; j <= totalnc; ++j){
    //				clusteridx = find(cluster==j);
    //				allclusteridx = find(allcluster==j);
    //				//W3 = hLambda.t() * (iI *totalnc) * etapsi.cols(allclusteridx);
    //				W3 = hLambda.t() * (iI *nc) * etapsi.cols(clusteridx);
    //				test1 = arma::accu(Wall.rows(clusteridx));
    //				test2 = arma::accu(W3);
    //				test3 += (test1 + nc/totalnc*test2) * (test1 + nc/totalnc*test2);
    //			}
    //			
    //			vLambda(t) = test3/(nc*nc);
    Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
    //Fstart(t) = Fstar(uuu-1);
    
    //Variance of adjusted CIF
    arma::rowvec zj = zetaj.row(t);
    arma::colvec zjc = zj.t();
    hFstar = myRowsums( (mycbind(zjc,uuu) -  nuj(t) * S1byS0hat.cols(0,uuu-1)) % myrbind(dlt,ncovs));
    W3Fstar.row(t) = hFstar.t() * (iI*totalnc) * (etapsi);
    
    W2finalFstar.col(t) = nuj(t)*W2.col(t);
    W1Fstar.col(t) = nuj(t)*W1.col(t);
    
    
    // Added on 11/03/2021
    //Variance of F(t|Z0)
    arma::colvec Zpredc = Zpred.t();
    hLambdapred = myRowsums( (mycbind(Zpredc,uuu) - S1byS0hat.cols(0,uuu-1) ) % myrbind(dlt,ncovs));
    W3pred.row(t) = hLambdapred.t() * (iI*totalnc) * (etapsi);
    
    
  }	
  
  
  Rcpp::List res(16);
  res[0]=W1;
  res[1]=W2;
  res[2]=W3;
  res[3]=zc_min_S1bS0hatc;
  res[4]=dMc;
  res[5]=Lambda;
  res[6]=CumLambda0;
  res[7]=Fstar;
  res[8]=W1Fstar;
  res[9]=W2finalFstar;
  res[10]=W3Fstar;
  res[11]=Fstar;
  res[12]=nuj;
  res[13]=Fpred;
  res[14]=W3pred;
  res[15]=Fpredfirst;
  
  return(res);
  
  
}


// Rcpp::List se_beta_lambda_strata_COX_diff( 
//     const arma::rowvec& times,
//     const arma::rowvec& causes,
//     const arma::mat& covariates,
//     const arma::mat& cencovariates, //censoring dependent covariates
//     const int& nobs, 
//     const int& ntotal,
//     const int& ncovs,
//     const arma::mat& cweight,//matrix when cox model is used for censoring
//     const arma::rowvec& betas,//Estimates for cause 1
//     const arma::rowvec& gammas,//Estimates for censoring
//     const arma::rowvec& cluster,
//     const arma::rowvec& allcluster,
//     const int& nc,
//     const int& totalnc,
//     arma::colvec& expg,
//     const arma::mat& iIc,
//     const arma::mat& iI,
//     const arma::rowvec& idxtlambda,
//     const arma::mat& etapsi,
//     const int& ntotalobs,
//     const arma::mat allcovariates){
//   
//   int i,j,k,t;
//   
//   //double loglikli=0;
//   //arma::colvec score(ncovs);
//   //score.fill(0.0);
//   //arma::mat infor(ncovs,ncovs);
//   //infor.fill(0.0);
//   arma::rowvec S0hat(nobs);S0hat.zeros();
//   arma::mat S1hat(ncovs,nobs);S1hat.zeros();
//   arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
//   //arma::cube S2byS0hat(ncovs,ncovs,nobs);
//   
//   for(i=0; i<nobs; ++i){
//     double s0=0;
//     arma::colvec s1(ncovs);
//     s1.zeros();
//     arma::mat s2(ncovs,ncovs);
//     s2.zeros();
//     double wye=0;
//     
//     //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
//     
//     arma::rowvec zi = covariates.row(i);
//     //loglikli += arma::as_scalar(betas*zi.t());
//     //score += zi.t();
//     
//     for(j=0; j<nobs; ++j){
//       if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
//       arma::rowvec zj = covariates.row(j);
//       if(times(i) <= times(j)){
//         wye = exp(arma::as_scalar(betas*zj.t()));
//       }
//       else if (times(i) > times(j) && causes(j)>1){
//         wye = exp(arma::as_scalar(betas*zj.t())) * cweight(j,i)/cweight(j,j);
//       }
//       else continue;
//       s0 += wye;
//       s1 += wye * zj.t();
//       s2 += wye * zj.t() * zj;
//     }	
//     S0hat(i) = s0/nc;
//     S1hat.col(i) = s1/nc;
//     S1byS0hat.col(i) = s1/s0;
//     //S2byS0hat.slice(i) = s2/s0;
//     
//   }
//   
//   //dlambda10(t)
//   arma::rowvec dlambdat(nobs);dlambdat.zeros();
//   for(j=0; j < nobs; ++j){
//     double dlambdatemp=0;
//     if(causes(j)!=1) continue;
//     for(i=0; i < nobs; ++i){
//       if(times(j) == times(i)){
//         dlambdatemp += 1/(S0hat(j)*nc);
//       }
//     }
//     dlambdat(j)=dlambdatemp;
//   }
//   
//   arma::rowvec CumLambda0allt(nobs);CumLambda0allt.zeros();
//   CumLambda0allt = arma::cumsum(dlambdat);
//   
//   //For Censoring(S0C(t),S1C(t),S1C(t)/S0C(t))
//   const int ncovsC=cencovariates.n_cols;
//   arma::rowvec S0hatC(nobs);S0hatC.zeros();
//   arma::mat S1hatC(ncovsC,nobs);S1hatC.zeros();
//   arma::mat S1byS0hatC(ncovsC,nobs);S1byS0hatC.zeros();
//   //arma::cube S2byS0hatC(ncovsC,ncovsC,nobs);
//   
//   for(i=0; i<nobs; ++i){ 
//     double s0C=0;
//     arma::colvec s1C(ncovsC);
//     s1C.zeros();
//     arma::mat s2C(ncovsC,ncovsC);
//     s2C.zeros();
//     //double wye=0;
//     
//     
//     
//     for(j=0; j<nobs; ++j){
//       if(times(j) >= times(i)){
//         arma::rowvec zjC = cencovariates.row(j);
//         s0C += exp(arma::as_scalar(gammas*zjC.t()));
//         s1C += zjC.t() * exp(arma::as_scalar(gammas*zjC.t()));
//         s2C += zjC.t() * zjC * exp(arma::as_scalar(gammas*zjC.t()));				
//       }
//     }	
//     S0hatC(i) = s0C/nc;
//     S1hatC.col(i) = s1C/nc;
//     S1byS0hatC.col(i) = s1C/s0C;
//     //S2byS0hatC.slice(i) = s2C/s0C;
//     
//   }
//   
//   //For Censoring(dlambdac(t))
//   arma::rowvec dlambdac(nobs);dlambdac.zeros();
//   for(j=0; j < nobs; ++j){
//     double dlambdatempc=0;
//     if(causes(j)!=0) continue;
//     for(i=0; i < nobs; ++i){
//       if(times(j) == times(i)){
//         dlambdatempc += 1/(S0hatC(j)*nc);
//       }
//     }
//     
//     dlambdac(j)=dlambdatempc;
//   }
//   
//   arma::cube h=hhat(gammas,S1byS0hatC,cencovariates,dlambdac);
//   
//   //q_i(u) and psi_i
//   //Calculating wdM and dMc as a matrix.(It is efficient to do it this way for calculating q_i(u)).
//   arma::mat wdM(nobs,nobs);wdM.zeros();
//   for(i=0; i < nobs; ++i){
//     arma::rowvec zi = covariates.row(i);
//     for(j=0; j < nobs; ++j){
//       double first=0;
//       double second=0;
//       if(causes(i)==1 && times(i)==times(j)){
//         first = 1;
//       }
//       if(times(j) <= times(i)){
//         second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
//       }
//       else if(times(j) > times(i) && causes(i) > 1){
//         second = exp(arma::as_scalar(betas*zi.t())) *cweight(i,j)/cweight(i,i)*dlambdat(j);
//       }
//       wdM(i,j) = first - second;
//     }
//   }
//   
//   //dMc
//   arma::mat dMc(nobs,nobs);dMc.zeros();
//   for(i=0; i < nobs; ++i){
//     arma::rowvec zci = cencovariates.row(i);
//     for(j=0; j < nobs; ++j){
//       double first=0;
//       double second=0;
//       if(causes(i)==0 && times(i)==times(j)){
//         first = 1;
//       }
//       if(times(i) >= times(j) && causes(j)==0){
//         second = exp(arma::as_scalar(gammas*zci.t())) * dlambdac(j);
//       }
//       dMc(i,j) = first - second;
//     }
//   }
//   
//   //zc_min_S1byS0hatC
//   //arma::colvec expg=exp(cencovariates * gammas.t());
//   arma::cube zc_min_S1bS0hatc(nobs,nobs,ncovsC);
//   for(j=0; j < ncovsC;++j){
//     arma::colvec zc=cencovariates.col(j);
//     arma::rowvec ratio = S1byS0hatC.row(j);
//     zc_min_S1bS0hatc.slice(j) = mycbind(zc,nobs) - myrbind(ratio,nobs);
//   }
//   
//   arma::mat qi21temp(nobs,nobs); qi21temp.zeros();
//   arma::rowvec qi21(nobs); qi21.zeros();
//   //Main loop to calculate psi
//   arma::mat z_min_S1bS0hat_wdM(nobs,nobs);
//   arma::rowvec qi11(ncovsC); qi11.zeros();
//   arma::mat test(nobs,nobs); test.zeros();
//   arma::mat precomp(1,ncovsC); precomp.zeros();
//   double qi2;
//   arma::vec denom;
//   //arma::vec locu;
//   //int locu;
//   arma::colvec s0C;
//   arma::mat tempzc;
//   arma::mat q1_all_i(1,ncovsC);
//   double qi2final;
//   arma::mat qi1(1,nobs); qi1.zeros();
//   arma::mat qi(nobs,nobs);
//   
//   
//   
//   //Calculating lambda(t) and standard error
//   int tl=idxtlambda.n_cols;
//   arma::colvec tt;
//   //double tfinal;
//   arma::mat oneoverS0hat_wdM(nobs,nobs); oneoverS0hat_wdM.zeros();
//   arma::mat first_term_W(nobs,nobs); first_term_W.zeros();
//   arma::mat second_term_W(nobs,nobs); second_term_W.zeros();
//   //arma::vec W1;arma::vec W2; arma::rowvec W3; arma::vec Wall;
//   arma::mat W1(nobs,tl); W1.zeros();arma::mat W2(nobs,tl); W2.zeros();arma::mat W3(tl,ntotal); W3.zeros();
//   arma::mat W1Fstar(nobs,tl); W1Fstar.zeros(); arma::mat W2finalFstar(nobs,tl); W2finalFstar.zeros();arma::mat W3Fstar(tl,ntotal); W3Fstar.zeros();
//   arma::vec hLambda; arma::vec hFstar;
//   arma::rowvec dlt;
//   arma::uvec clusteridx;arma::uvec allclusteridx;
//   arma::rowvec vLambda(tl); vLambda.zeros();
//   arma::rowvec Lambda(tl); Lambda.zeros();
//   arma::rowvec Fstart(tl); Fstart.zeros();
//   //arma::vec test1;
//   
//   
//   //		oneoverS0hat_wdM = 1/myrbind(S0hat,nobs) % wdM;
//   
//   //		for(k=0; k < ncovsC; ++k){
//   //			test = h(span(k),span::all,span::all);
//   //			qi11(k) = accu(test % oneoverS0hat_wdM);//Takes care of summation and integral	
//   //		}
//   
//   //		qi21temp = mycbind(expg,nobs) % oneoverS0hat_wdM;
//   //		qi21 = myColsums(qi21temp);
//   //		precomp = qi11*(iIc*nc);//Omega_hatC = iIc*nc
//   //		//arma::mat qi(nobs,nobs);
//   //		for(i=0; i < nobs; ++i){//This loop is for u in the formula.
//   //			double uu = times(i);
//   //			arma::uvec idx = find(uu <= times);
//   //			qi2 = accu(qi21.elem(idx));
//   //			arma::uvec locu = find(uu==times);
//   //			denom = S0hatC.elem(locu);
//   //			double finaldenom = denom(0);//This will take care if two time points are the same.
//   //			qi2final = qi2/finaldenom;
//   //			//tempzc = zc_min_S1bS0hatc(span::all,span(i),span::all);
//   //			//qi1 = precomp*tempzc.t();
//   //			qi.col(i) = -1*(qi1.t() + qi2final)/nc;
//   //		}
//   
//   arma::rowvec nuj(tl); 
//   
//   arma::rowvec CumLambda0(tl); CumLambda0.zeros();
//   for(t=0; t < tl; ++t){
//     int uuu = idxtlambda(t);
//     CumLambda0(t) = arma::accu(dlambdat.cols(0,uuu-1));
//   }
//   
//   //CumLambda0 = arma::cumsum(dlambdat);
//   
//   arma::mat zetaj(tl,ncovs); zetaj.zeros();
//   
//   //Estimate of adjusted CIF
//   arma::rowvec Fstar(tl); Fstar.zeros();
//   
//   for(i=0; i < tl; ++i){
//     double Fstartemp = 0;
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       Fstartemp += ( 1 - exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) ) );
//     }
//     Fstar(i) =  Fstartemp/ntotalobs;
//   }
//   
//   arma::rowvec Fstarallt(nobs); Fstarallt.zeros();
//   
//   for(i=0; i < nobs; ++i){
//     double Fstartempallt = 0;
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       Fstartempallt += ( 1 - exp(-CumLambda0allt(i) * exp(arma::as_scalar(betas*zj.t())) ) );
//     }
//     Fstarallt(i) =  Fstartempallt/ntotalobs;
//   }
//   
//   //arma::rowvec nuj(nobs); nuj.zeros();
//   for(i=0; i < tl; ++i){
//     double nujtemp = 0;
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       nujtemp += exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) + arma::as_scalar(betas*zj.t()) ) ;
//     }
//     nuj(i) = nujtemp/ntotalobs;
//   }
//   
//   
//   for(i=0; i < tl; ++i){
//     arma::rowvec zetajtemp(ncovs); zetajtemp.zeros();
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       zetajtemp += zj * exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) + arma::as_scalar(betas*zj.t()) ) ;
//     }
//     zetaj.row(i) = zetajtemp/ntotalobs;
//   }
//   
//   
//   for(t=0; t < tl; ++t){
//     int uuu = idxtlambda(t);
//     
//     //Added 02/26/2021 (q^(2) is a function of t as well)
//     arma::rowvec mytemp1 = S0hat.cols(0,uuu-1);
//     oneoverS0hat_wdM = 1/myrbind(mytemp1,nobs) % wdM.cols(0,uuu-1);
//     for(k=0; k < ncovsC; ++k){
//       test = h(arma::span(k),arma::span::all,arma::span::all);
//       qi11(k) = accu(test.cols(0,uuu-1) % oneoverS0hat_wdM);//Takes care of summation and integral	
//     }
//     qi21temp = mycbind(expg,uuu) % oneoverS0hat_wdM;
//     qi21 = myColsums(qi21temp);
//     precomp = qi11*(iIc*totalnc);//Omega_hatC = iIc*nc
//     
//     for(i=0; i < nobs; ++i){//This loop is for u in the formula.
//       double uu = times(i);
//       arma::uvec idx = find(uu <= times.cols(0,uuu-1));
//       qi2 = accu(qi21.elem(idx));
//       arma::uvec locu = find(uu==times);
//       denom = S0hatC.elem(locu);
//       double finaldenom = denom(0);//This will take care if two time points are the same.
//       qi2final = qi2/finaldenom;
//       //tempzc = zc_min_S1bS0hatc(span::all,span(i),span::all);
//       //qi1 = precomp*tempzc.t();
//       qi.col(i) = -1*(qi1.t() + qi2final)/nc;
//       //qi.col(i) = -1*(qi1.t())/totalnc;
//     }
//     //Added 02/26/2021 end
//     
//     first_term_W = wdM/myrbind(S0hat,nobs);
//     W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
//     second_term_W = qi % dMc;
//     //W2 = myRowsums(second_term_W.cols(0,uuu-1));
//     W2.col(t) = myRowsums(second_term_W);
//     dlt = dlambdat.cols(0,uuu-1);
//     hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
//     W3.row(t) = hLambda.t() * (iI*totalnc) * (etapsi);
//     //Wall = W1 + W2 + W3.t();
//     //			Wall = (W1 + W2);
//     
//     //			double test1=0;
//     //			double test2=0;
//     //			double test3=0;
//     //			for(j=1; j <= totalnc; ++j){
//     //				clusteridx = find(cluster==j);
//     //				allclusteridx = find(allcluster==j);
//     //				//W3 = hLambda.t() * (iI *totalnc) * etapsi.cols(allclusteridx);
//     //				W3 = hLambda.t() * (iI *nc) * etapsi.cols(clusteridx);
//     //				test1 = arma::accu(Wall.rows(clusteridx));
//     //				test2 = arma::accu(W3);
//     //				test3 += (test1 + nc/totalnc*test2) * (test1 + nc/totalnc*test2);
//     //			}
//     //			
//     //			vLambda(t) = test3/(nc*nc);
//     Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
//     //Fstart(t) = Fstar(uuu-1);
//     
//     //Variance of adjusted CIF
//     arma::rowvec zj = zetaj.row(t);
//     arma::colvec zjc = zj.t();
//     hFstar = myRowsums( (mycbind(zjc,uuu) -  nuj(t) * S1byS0hat.cols(0,uuu-1)) % myrbind(dlt,ncovs));
//     W3Fstar.row(t) = hFstar.t() * (iI*totalnc) * (etapsi);
//     
//     W2finalFstar.col(t) = nuj(t)*W2.col(t);
//     W1Fstar.col(t) = nuj(t)*W1.col(t);
//     
//   }	
//   
//   
//   Rcpp::List res(13);
//   res[0]=W1;
//   res[1]=W2;
//   res[2]=W3;
//   res[3]=zc_min_S1bS0hatc;
//   res[4]=dMc;
//   res[5]=Lambda;
//   res[6]=CumLambda0allt;
//   res[7]=Fstarallt;
//   res[8]=W1Fstar;
//   res[9]=W2finalFstar;
//   res[10]=W3Fstar;
//   res[11]=Fstar;
//   res[12]=nuj;
//   
//   return(res);
//   
//   
// }

// [[Rcpp::export]]

Rcpp::List se_beta_lambda_strata_COX_diff1( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const arma::mat& cencovariates, //censoring dependent covariates
    const int& nobs, 
    const int& nobs1,
    const int& ncovs,
    const arma::mat& cweight,//matrix when cox model is used for censoring
    const arma::rowvec& betas,//Estimates for cause 1
    const arma::rowvec& gammas,//Estimates for censoring
    const arma::rowvec& cluster,
    const arma::rowvec& allcluster,
    const int& nc,
    const int& totalnc,
    arma::colvec& expg,
    const arma::mat& iIc,
    const arma::mat& iI,
    const arma::rowvec& idxtlambda,
    const arma::mat& etapsi,
    const arma::cube& zcminusecold,
    const arma::mat& dMc1,
    const int& ntotalobs,
    const arma::mat& allcovariates){
  
  int i,j,k,t;
  
  //double loglikli=0;
  //arma::colvec score(ncovs);
  //score.fill(0.0);
  //arma::mat infor(ncovs,ncovs);
  //infor.fill(0.0);
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  //arma::cube S2byS0hat(ncovs,ncovs,nobs);
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.zeros();
    arma::mat s2(ncovs,ncovs);
    s2.zeros();
    double wye=0;
    
    //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
    
    arma::rowvec zi = covariates.row(i);
    //loglikli += arma::as_scalar(betas*zi.t());
    //score += zi.t();
    
    for(j=0; j<nobs; ++j){
      if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
      arma::rowvec zj = covariates.row(j);
      if(times(i) <= times(j)){
        wye = exp(arma::as_scalar(betas*zj.t()));
      }
      else if (times(i) > times(j) && causes(j)>1){
        wye = exp(arma::as_scalar(betas*zj.t())) * cweight(j,i)/cweight(j,j);
      }
      else continue;
      s0 += wye;
      s1 += wye * zj.t();
      s2 += wye * zj.t() * zj;
    }	
    S0hat(i) = s0/nc;
    S1hat.col(i) = s1/nc;
    S1byS0hat.col(i) = s1/s0;
    //S2byS0hat.slice(i) = s2/s0;
    
  }
  
  //dlambda10(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(causes(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += 1/(S0hat(j)*nc);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  
  
  //For Censoring(S0C(t),S1C(t),S1C(t)/S0C(t))
  const int ncovsC=cencovariates.n_cols;
  arma::rowvec S0hatC(nobs);S0hatC.zeros();
  arma::mat S1hatC(ncovsC,nobs);S1hatC.zeros();
  arma::mat S1byS0hatC(ncovsC,nobs);S1byS0hatC.zeros();
  //arma::cube S2byS0hatC(ncovsC,ncovsC,nobs);
  
  for(i=0; i<nobs; ++i){ 
    double s0C=0;
    arma::colvec s1C(ncovsC);
    s1C.zeros();
    arma::mat s2C(ncovsC,ncovsC);
    s2C.zeros();
    //double wye=0;
    
    
    
    for(j=0; j<nobs; ++j){
      if(times(j) >= times(i)){
        arma::rowvec zjC = cencovariates.row(j);
        s0C += exp(arma::as_scalar(gammas*zjC.t()));
        s1C += zjC.t() * exp(arma::as_scalar(gammas*zjC.t()));
        s2C += zjC.t() * zjC * exp(arma::as_scalar(gammas*zjC.t()));				
      }
    }	
    S0hatC(i) = s0C/nc;
    S1hatC.col(i) = s1C/nc;
    S1byS0hatC.col(i) = s1C/s0C;
    //S2byS0hatC.slice(i) = s2C/s0C;
    
  }
  
  //For Censoring(dlambdac(t))
  arma::rowvec dlambdac(nobs);dlambdac.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatempc=0;
    if(causes(j)!=0) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatempc += 1/(S0hatC(j)*nc);
      }
    }
    
    dlambdac(j)=dlambdatempc;
  }
  
  arma::cube h=hhat(gammas,S1byS0hatC,cencovariates,dlambdac);
  
  //q_i(u) and psi_i
  //Calculating wdM and dMc as a matrix.(It is efficient to do it this way for calculating q_i(u)).
  arma::mat wdM(nobs,nobs);wdM.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==1 && times(i)==times(j)){
        first = 1;
      }
      if(times(j) <= times(i)){
        second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
      else if(times(j) > times(i) && causes(i) > 1){
        second = exp(arma::as_scalar(betas*zi.t())) *cweight(i,j)/cweight(i,i)*dlambdat(j);
      }
      wdM(i,j) = first - second;
    }
  }
  
  //dMc
  arma::mat dMc(nobs,nobs);dMc.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zci = cencovariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==0 && times(i)==times(j)){
        first = 1;
      }
      if(times(i) >= times(j) && causes(j)==0){
        second = exp(arma::as_scalar(gammas*zci.t())) * dlambdac(j);
      }
      dMc(i,j) = first - second;
    }
  }
  
  //zc_min_S1byS0hatC
  //arma::colvec expg=exp(cencovariates * gammas.t());
  arma::cube zc_min_S1bS0hatc(nobs,nobs,ncovsC);
  for(j=0; j < ncovsC;++j){
    arma::colvec zc=cencovariates.col(j);
    arma::rowvec ratio = S1byS0hatC.row(j);
    zc_min_S1bS0hatc.slice(j) = mycbind(zc,nobs) - myrbind(ratio,nobs);
  }
  
  arma::mat qi21temp(nobs,nobs); qi21temp.zeros();
  arma::rowvec qi21(nobs); qi21.zeros();
  //Main loop to calculate psi
  arma::mat z_min_S1bS0hat_wdM(nobs,nobs);
  arma::rowvec qi11(ncovsC); qi11.zeros();
  arma::mat test(nobs,nobs); test.zeros();
  arma::mat precomp(1,ncovsC); precomp.zeros();
  //double qi2;
  arma::vec denom;
  //arma::vec locu;
  //int locu;
  arma::colvec s0C;
  arma::mat tempzc;
  arma::mat q1_all_i(1,ncovsC);
  //double qi2final;
  arma::mat qi1(1,nobs1); 
  arma::mat qi(nobs1,nobs1);
  
  
  
  //Calculating lambda(t) and standard error
  int tl=idxtlambda.n_cols;
  arma::colvec tt;
  //double tfinal;
  arma::mat oneoverS0hat_wdM(nobs,nobs); oneoverS0hat_wdM.zeros();
  arma::mat first_term_W(nobs,nobs); first_term_W.zeros();
  arma::mat second_term_W(nobs1,nobs1); second_term_W.zeros();
  arma::vec W1;arma::mat W2(nobs1,tl); W2.zeros(); arma::rowvec W3; arma::vec Wall;
  arma::mat W2finalFstar(nobs1,tl); W2finalFstar.zeros();
  arma::vec hLambda;
  arma::rowvec dlt;
  arma::uvec clusteridx;arma::uvec allclusteridx;
  arma::rowvec vLambda(tl); vLambda.zeros();
  arma::rowvec Lambda(tl); Lambda.zeros();
  
  arma::rowvec nuj(tl);
  
  arma::rowvec CumLambda0(tl); CumLambda0.zeros();
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);
    CumLambda0(t) = arma::accu(dlambdat.cols(0,uuu-1));
  }
  
  //CumLambda0 = arma::cumsum(dlambdat);
  
  //Estimate of adjusted CIF
  arma::rowvec Fstar(tl); Fstar.zeros();
  
  for(i=0; i < tl; ++i){
    double Fstartemp = 0;
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = allcovariates.row(j);
      Fstartemp += ( 1 - exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) ) );
    }
    Fstar(i) =  Fstartemp/ntotalobs;
  }
  
  //arma::rowvec nuj(nobs); nuj.zeros();
  for(i=0; i < tl; ++i){
    double nujtemp = 0;
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = allcovariates.row(j);
      nujtemp += exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) + arma::as_scalar(betas*zj.t()) ) ;
    }
    nuj(i) = nujtemp/ntotalobs;
  }
  
  
  
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);
    
    //Added 02/26/2021 (q^(2) is a function of t as well)
    arma::rowvec mytemp1 = S0hat.cols(0,uuu-1);
    oneoverS0hat_wdM = 1/myrbind(mytemp1,nobs) % wdM.cols(0,uuu-1);
    for(k=0; k < ncovsC; ++k){
      test = h(arma::span(k),arma::span::all,arma::span::all);
      qi11(k) = accu(test.cols(0,uuu-1) % oneoverS0hat_wdM);//Takes care of summation and integral
    }
    precomp = qi11*(iIc*totalnc);//Omega_hatC = iIc*totalnc
    
    for(i=0; i < nobs1; ++i){//This loop is for u in the formula.
      tempzc = zcminusecold(arma::span::all,arma::span(i),arma::span::all);
      qi1 = precomp*tempzc.t();
      qi.col(i) = -1*(qi1.t())/totalnc;
    }
    //Added 02/26/2021 end
    
    second_term_W = qi % dMc1;
    W2.col(t) = myRowsums(second_term_W);
    W2finalFstar.col(t) = nuj(t) * myRowsums(second_term_W);
    
  }
  
  
  
  
  
  Rcpp::List res(2);
  res[0]=W2;
  res[1]=W2finalFstar;
  
  //res[0]=0;
  //res[1]=0;
  
  
  return(res);
  
  
}

// [[Rcpp::export]]
Rcpp::List se_beta_lambda_strata_COX_diff_unstratified( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const arma::mat& cencovariates, //censoring dependent covariates
    const int& nobs, 
    const int& ntotal,
    const int& ncovs,
    const arma::mat& cweight,//matrix when cox model is used for censoring
    const arma::rowvec& betas,//Estimates for cause 1
    const arma::rowvec& gammas,//Estimates for censoring
    const arma::rowvec& cluster,
    const arma::rowvec& allcluster,
    const int& nc,
    const int& totalnc,
    arma::colvec& expg,
    const arma::mat& iIc,
    const arma::mat& iI,
    const arma::rowvec& idxtlambda,
    const arma::mat& etapsi,
    const int& ntotalobs,
    const arma::mat allcovariates,
    const arma::rowvec& adjustedbetas,
    const arma::rowvec Zpred){
  
  int i,j,k,t;
  
  //double loglikli=0;
  //arma::colvec score(ncovs);
  //score.fill(0.0);
  //arma::mat infor(ncovs,ncovs);
  //infor.fill(0.0);
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  //arma::cube S2byS0hat(ncovs,ncovs,nobs);
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.zeros();
    arma::mat s2(ncovs,ncovs);
    s2.zeros();
    double wye=0;
    
    //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
    
    arma::rowvec zi = covariates.row(i);
    //loglikli += arma::as_scalar(betas*zi.t());
    //score += zi.t();
    
    for(j=0; j<nobs; ++j){
      if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
      arma::rowvec zj = covariates.row(j);
      if(times(i) <= times(j)){
        wye = exp(arma::as_scalar(betas*zj.t()));
      }
      else if (times(i) > times(j) && causes(j)>1){
        wye = exp(arma::as_scalar(betas*zj.t())) * cweight(j,i)/cweight(j,j);
      }
      else continue;
      s0 += wye;
      s1 += wye * zj.t();
      s2 += wye * zj.t() * zj;
    }	
    S0hat(i) = s0/nc;
    S1hat.col(i) = s1/nc;
    S1byS0hat.col(i) = s1/s0;
    //S2byS0hat.slice(i) = s2/s0;
    
  }
  
  //dlambda10(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(causes(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += 1/(S0hat(j)*nc);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  
  
  //For Censoring(S0C(t),S1C(t),S1C(t)/S0C(t))
  const int ncovsC=cencovariates.n_cols;
  arma::rowvec S0hatC(nobs);S0hatC.zeros();
  arma::mat S1hatC(ncovsC,nobs);S1hatC.zeros();
  arma::mat S1byS0hatC(ncovsC,nobs);S1byS0hatC.zeros();
  //arma::cube S2byS0hatC(ncovsC,ncovsC,nobs);
  
  for(i=0; i<nobs; ++i){ 
    double s0C=0;
    arma::colvec s1C(ncovsC);
    s1C.zeros();
    arma::mat s2C(ncovsC,ncovsC);
    s2C.zeros();
    //double wye=0;
    
    
    
    for(j=0; j<nobs; ++j){
      if(times(j) >= times(i)){
        arma::rowvec zjC = cencovariates.row(j);
        s0C += exp(arma::as_scalar(gammas*zjC.t()));
        s1C += zjC.t() * exp(arma::as_scalar(gammas*zjC.t()));
        s2C += zjC.t() * zjC * exp(arma::as_scalar(gammas*zjC.t()));				
      }
    }	
    S0hatC(i) = s0C/nc;
    S1hatC.col(i) = s1C/nc;
    S1byS0hatC.col(i) = s1C/s0C;
    //S2byS0hatC.slice(i) = s2C/s0C;
    
  }
  
  //For Censoring(dlambdac(t))
  arma::rowvec dlambdac(nobs);dlambdac.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatempc=0;
    if(causes(j)!=0) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatempc += 1/(S0hatC(j)*nc);
      }
    }
    
    dlambdac(j)=dlambdatempc;
  }
  
  arma::cube h=hhat(gammas,S1byS0hatC,cencovariates,dlambdac);
  
  //q_i(u) and psi_i
  //Calculating wdM and dMc as a matrix.(It is efficient to do it this way for calculating q_i(u)).
  arma::mat wdM(nobs,nobs);wdM.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==1 && times(i)==times(j)){
        first = 1;
      }
      if(times(j) <= times(i)){
        second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
      else if(times(j) > times(i) && causes(i) > 1){
        second = exp(arma::as_scalar(betas*zi.t())) *cweight(i,j)/cweight(i,i)*dlambdat(j);
      }
      wdM(i,j) = first - second;
    }
  }
  
  //dMc
  arma::mat dMc(nobs,nobs);dMc.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zci = cencovariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==0 && times(i)==times(j)){
        first = 1;
      }
      if(times(i) >= times(j) && causes(j)==0){
        second = exp(arma::as_scalar(gammas*zci.t())) * dlambdac(j);
      }
      dMc(i,j) = first - second;
    }
  }
  
  //zc_min_S1byS0hatC
  //arma::colvec expg=exp(cencovariates * gammas.t());
  arma::cube zc_min_S1bS0hatc(nobs,nobs,ncovsC);
  for(j=0; j < ncovsC;++j){
    arma::colvec zc=cencovariates.col(j);
    arma::rowvec ratio = S1byS0hatC.row(j);
    zc_min_S1bS0hatc.slice(j) = mycbind(zc,nobs) - myrbind(ratio,nobs);
  }
  
  arma::mat qi21temp(nobs,nobs); qi21temp.zeros();
  arma::rowvec qi21(nobs); qi21.zeros();
  //Main loop to calculate psi
  arma::mat z_min_S1bS0hat_wdM(nobs,nobs);
  arma::rowvec qi11(ncovsC); qi11.zeros();
  arma::mat test(nobs,nobs); test.zeros();
  arma::mat precomp(1,ncovsC); precomp.zeros();
  double qi2;
  arma::vec denom;
  //arma::vec locu;
  //int locu;
  arma::colvec s0C;
  arma::mat tempzc;
  arma::mat q1_all_i(1,ncovsC);
  double qi2final;
  arma::mat qi1(1,nobs); qi1.zeros();
  arma::mat qi(nobs,nobs);
  
  
  
  //Calculating lambda(t) and standard error
  int tl=idxtlambda.n_cols;
  arma::colvec tt;
  //double tfinal;
  arma::mat oneoverS0hat_wdM(nobs,nobs); oneoverS0hat_wdM.zeros();
  arma::mat first_term_W(nobs,nobs); first_term_W.zeros();
  arma::mat second_term_W(nobs,nobs); second_term_W.zeros();
  //arma::vec W1;arma::vec W2; arma::rowvec W3; arma::vec Wall;
  arma::mat W1(nobs,tl); W1.zeros();arma::mat W2(nobs,tl); W2.zeros();arma::mat W3(tl,ntotal); W3.zeros();
  arma::mat W1Fstar(nobs,tl); W1Fstar.zeros(); arma::mat W2finalFstar(nobs,tl); W2finalFstar.zeros();arma::mat W3Fstar(tl,ntotal); W3Fstar.zeros();
  arma::vec hLambda; arma::vec hFstar;
  arma::rowvec dlt;
  arma::uvec clusteridx;arma::uvec allclusteridx;
  arma::rowvec vLambda(tl); vLambda.zeros();
  arma::rowvec Lambda(tl); Lambda.zeros();
  arma::rowvec Fstart(tl); Fstart.zeros();
  
  // Added on 11/03/2021
  arma::vec hLambdapred;
  arma::mat W3pred(tl,ntotal); W3pred.zeros();
  //arma::vec test1;
  
  
  //		oneoverS0hat_wdM = 1/myrbind(S0hat,nobs) % wdM;
  
  //		for(k=0; k < ncovsC; ++k){
  //			test = h(span(k),span::all,span::all);
  //			qi11(k) = accu(test % oneoverS0hat_wdM);//Takes care of summation and integral	
  //		}
  
  //		qi21temp = mycbind(expg,nobs) % oneoverS0hat_wdM;
  //		qi21 = myColsums(qi21temp);
  //		precomp = qi11*(iIc*nc);//Omega_hatC = iIc*nc
  //		//arma::mat qi(nobs,nobs);
  //		for(i=0; i < nobs; ++i){//This loop is for u in the formula.
  //			double uu = times(i);
  //			arma::uvec idx = find(uu <= times);
  //			qi2 = accu(qi21.elem(idx));
  //			arma::uvec locu = find(uu==times);
  //			denom = S0hatC.elem(locu);
  //			double finaldenom = denom(0);//This will take care if two time points are the same.
  //			qi2final = qi2/finaldenom;
  //			//tempzc = zc_min_S1bS0hatc(span::all,span(i),span::all);
  //			//qi1 = precomp*tempzc.t();
  //			qi.col(i) = -1*(qi1.t() + qi2final)/nc;
  //		}
  
  arma::rowvec nuj(tl); 
  
  arma::rowvec CumLambda0(tl); CumLambda0.zeros();
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);
    CumLambda0(t) = arma::accu(dlambdat.cols(0,uuu-1));
  }
  
  //CumLambda0 = arma::cumsum(dlambdat);
  
  arma::mat zetaj(tl,ncovs); zetaj.zeros();
  
  //Estimate of adjusted CIF
  arma::rowvec Fstar(tl); Fstar.zeros();
  
  for(i=0; i < tl; ++i){
    double Fstartemp = 0;
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = allcovariates.row(j);
      Fstartemp += ( 1 - exp(-CumLambda0(i) * exp(arma::as_scalar(adjustedbetas*zj.t())) ) );
    }
    Fstar(i) =  Fstartemp/ntotalobs;
  }
  
  //arma::rowvec nuj(nobs); nuj.zeros();
  for(i=0; i < tl; ++i){
    double nujtemp = 0;
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = allcovariates.row(j);
      nujtemp += exp(-CumLambda0(i) * exp(arma::as_scalar(adjustedbetas*zj.t())) + arma::as_scalar(adjustedbetas*zj.t()) ) ;
    }
    nuj(i) = nujtemp/ntotalobs;
  }
  
  
  for(i=0; i < tl; ++i){
    arma::rowvec zetajtemp(ncovs); zetajtemp.zeros();
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = allcovariates.row(j);
      zetajtemp += zj * exp(-CumLambda0(i) * exp(arma::as_scalar(adjustedbetas*zj.t())) + arma::as_scalar(adjustedbetas*zj.t()) ) ;
    }
    zetaj.row(i) = zetajtemp/ntotalobs;
  }
  
  //Estimate of F(t|Zpred). Added on 11/03/2021
  arma::rowvec Fpred(tl); Fpred.zeros();
  arma::rowvec Fpredfirst(tl); Fpredfirst.zeros();
  for(i=0; i < tl; ++i){
    Fpred(i) = 1 - exp(-CumLambda0(i)*exp(arma::as_scalar(betas*Zpred.t())));
    Fpredfirst(i) = exp(arma::as_scalar(betas*Zpred.t()) - CumLambda0(i)*exp(arma::as_scalar(betas*Zpred.t())));
  }
  
  
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);
    
    //Added 02/26/2021 (q^(2) is a function of t as well)
    arma::rowvec mytemp1 = S0hat.cols(0,uuu-1);
    oneoverS0hat_wdM = 1/myrbind(mytemp1,nobs) % wdM.cols(0,uuu-1);
    for(k=0; k < ncovsC; ++k){
      test = h(arma::span(k),arma::span::all,arma::span::all);
      qi11(k) = accu(test.cols(0,uuu-1) % oneoverS0hat_wdM);//Takes care of summation and integral	
    }
    qi21temp = mycbind(expg,uuu) % oneoverS0hat_wdM;
    qi21 = myColsums(qi21temp);
    precomp = qi11*(iIc*totalnc);//Omega_hatC = iIc*nc
    
    for(i=0; i < nobs; ++i){//This loop is for u in the formula.
      double uu = times(i);
      arma::uvec idx = find(uu <= times.cols(0,uuu-1));
      qi2 = accu(qi21.elem(idx));
      arma::uvec locu = find(uu==times);
      denom = S0hatC.elem(locu);
      double finaldenom = denom(0);//This will take care if two time points are the same.
      qi2final = qi2/finaldenom;
      //tempzc = zc_min_S1bS0hatc(span::all,span(i),span::all);
      //qi1 = precomp*tempzc.t();
      qi.col(i) = -1*(qi1.t() + qi2final)/nc;
      //qi.col(i) = -1*(qi1.t())/totalnc;
    }
    //Added 02/26/2021 end
    
    first_term_W = wdM/myrbind(S0hat,nobs);
    W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
    second_term_W = qi % dMc;
    //W2 = myRowsums(second_term_W.cols(0,uuu-1));
    W2.col(t) = myRowsums(second_term_W);
    dlt = dlambdat.cols(0,uuu-1);
    hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
    W3.row(t) = hLambda.t() * (iI*totalnc) * (etapsi);
    //Wall = W1 + W2 + W3.t();
    //			Wall = (W1 + W2);
    
    //			double test1=0;
    //			double test2=0;
    //			double test3=0;
    //			for(j=1; j <= totalnc; ++j){
    //				clusteridx = find(cluster==j);
    //				allclusteridx = find(allcluster==j);
    //				//W3 = hLambda.t() * (iI *totalnc) * etapsi.cols(allclusteridx);
    //				W3 = hLambda.t() * (iI *nc) * etapsi.cols(clusteridx);
    //				test1 = arma::accu(Wall.rows(clusteridx));
    //				test2 = arma::accu(W3);
    //				test3 += (test1 + nc/totalnc*test2) * (test1 + nc/totalnc*test2);
    //			}
    //			
    //			vLambda(t) = test3/(nc*nc);
    Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
    //Fstart(t) = Fstar(uuu-1);
    
    //Variance of adjusted CIF
    arma::rowvec zj = zetaj.row(t);
    arma::colvec zjc = zj.t();
    hFstar = myRowsums( (mycbind(zjc,uuu) -  nuj(t) * S1byS0hat.cols(0,uuu-1)) % myrbind(dlt,ncovs));
    W3Fstar.row(t) = hFstar.t() * (iI*totalnc) * (etapsi);
    
    W2finalFstar.col(t) = nuj(t)*W2.col(t);
    W1Fstar.col(t) = nuj(t)*W1.col(t);
    
    // Added on 11/03/2021
    //Variance of F(t|Z0)
    arma::colvec Zpredc = Zpred.t();
    hLambdapred = myRowsums( (mycbind(Zpredc,uuu) - S1byS0hat.cols(0,uuu-1) ) % myrbind(dlt,ncovs));
    W3pred.row(t) = hLambdapred.t() * (iI*totalnc) * (etapsi);
    
  }	
  
  
  Rcpp::List res(16);
  res[0]=W1;
  res[1]=W2;
  res[2]=W3;
  res[3]=zc_min_S1bS0hatc;
  res[4]=dMc;
  res[5]=Lambda;
  res[6]=CumLambda0;
  res[7]=Fstar;
  res[8]=W1Fstar;
  res[9]=W2finalFstar;
  res[10]=W3Fstar;
  res[11]=Fstar;
  res[12]=nuj;
  res[13]=Fpred;
  res[14]=W3pred;
  res[15]=Fpredfirst;
  
  return(res);
  
  
}


// Rcpp::List se_beta_lambda_strata_COX_diff_unstratified( 
//     const arma::rowvec& times,
//     const arma::rowvec& causes,
//     const arma::mat& covariates,
//     const arma::mat& cencovariates, //censoring dependent covariates
//     const int& nobs, 
//     const int& ntotal,
//     const int& ncovs,
//     const arma::mat& cweight,//matrix when cox model is used for censoring
//     const arma::rowvec& betas,//Estimates for cause 1
//     const arma::rowvec& gammas,//Estimates for censoring
//     const arma::rowvec& cluster,
//     const arma::rowvec& allcluster,
//     const int& nc,
//     const int& totalnc,
//     arma::colvec& expg,
//     const arma::mat& iIc,
//     const arma::mat& iI,
//     const arma::rowvec& idxtlambda,
//     const arma::mat& etapsi,
//     const int& ntotalobs,
//     const arma::mat allcovariates,
//     const arma::rowvec& adjustedbetas){
//   
//   int i,j,k,t;
//   
//   //double loglikli=0;
//   //arma::colvec score(ncovs);
//   //score.fill(0.0);
//   //arma::mat infor(ncovs,ncovs);
//   //infor.fill(0.0);
//   arma::rowvec S0hat(nobs);S0hat.zeros();
//   arma::mat S1hat(ncovs,nobs);S1hat.zeros();
//   arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
//   //arma::cube S2byS0hat(ncovs,ncovs,nobs);
//   
//   for(i=0; i<nobs; ++i){
//     double s0=0;
//     arma::colvec s1(ncovs);
//     s1.zeros();
//     arma::mat s2(ncovs,ncovs);
//     s2.zeros();
//     double wye=0;
//     
//     //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
//     
//     arma::rowvec zi = covariates.row(i);
//     //loglikli += arma::as_scalar(betas*zi.t());
//     //score += zi.t();
//     
//     for(j=0; j<nobs; ++j){
//       if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
//       arma::rowvec zj = covariates.row(j);
//       if(times(i) <= times(j)){
//         wye = exp(arma::as_scalar(betas*zj.t()));
//       }
//       else if (times(i) > times(j) && causes(j)>1){
//         wye = exp(arma::as_scalar(betas*zj.t())) * cweight(j,i)/cweight(j,j);
//       }
//       else continue;
//       s0 += wye;
//       s1 += wye * zj.t();
//       s2 += wye * zj.t() * zj;
//     }	
//     S0hat(i) = s0/nc;
//     S1hat.col(i) = s1/nc;
//     S1byS0hat.col(i) = s1/s0;
//     //S2byS0hat.slice(i) = s2/s0;
//     
//   }
//   
//   //dlambda10(t)
//   arma::rowvec dlambdat(nobs);dlambdat.zeros();
//   for(j=0; j < nobs; ++j){
//     double dlambdatemp=0;
//     if(causes(j)!=1) continue;
//     for(i=0; i < nobs; ++i){
//       if(times(j) == times(i)){
//         dlambdatemp += 1/(S0hat(j)*nc);
//       }
//     }
//     dlambdat(j)=dlambdatemp;
//   }
//   
//   
//   
//   //For Censoring(S0C(t),S1C(t),S1C(t)/S0C(t))
//   const int ncovsC=cencovariates.n_cols;
//   arma::rowvec S0hatC(nobs);S0hatC.zeros();
//   arma::mat S1hatC(ncovsC,nobs);S1hatC.zeros();
//   arma::mat S1byS0hatC(ncovsC,nobs);S1byS0hatC.zeros();
//   //arma::cube S2byS0hatC(ncovsC,ncovsC,nobs);
//   
//   for(i=0; i<nobs; ++i){ 
//     double s0C=0;
//     arma::colvec s1C(ncovsC);
//     s1C.zeros();
//     arma::mat s2C(ncovsC,ncovsC);
//     s2C.zeros();
//     //double wye=0;
//     
//     
//     
//     for(j=0; j<nobs; ++j){
//       if(times(j) >= times(i)){
//         arma::rowvec zjC = cencovariates.row(j);
//         s0C += exp(arma::as_scalar(gammas*zjC.t()));
//         s1C += zjC.t() * exp(arma::as_scalar(gammas*zjC.t()));
//         s2C += zjC.t() * zjC * exp(arma::as_scalar(gammas*zjC.t()));				
//       }
//     }	
//     S0hatC(i) = s0C/nc;
//     S1hatC.col(i) = s1C/nc;
//     S1byS0hatC.col(i) = s1C/s0C;
//     //S2byS0hatC.slice(i) = s2C/s0C;
//     
//   }
//   
//   //For Censoring(dlambdac(t))
//   arma::rowvec dlambdac(nobs);dlambdac.zeros();
//   for(j=0; j < nobs; ++j){
//     double dlambdatempc=0;
//     if(causes(j)!=0) continue;
//     for(i=0; i < nobs; ++i){
//       if(times(j) == times(i)){
//         dlambdatempc += 1/(S0hatC(j)*nc);
//       }
//     }
//     
//     dlambdac(j)=dlambdatempc;
//   }
//   
//   arma::cube h=hhat(gammas,S1byS0hatC,cencovariates,dlambdac);
//   
//   //q_i(u) and psi_i
//   //Calculating wdM and dMc as a matrix.(It is efficient to do it this way for calculating q_i(u)).
//   arma::mat wdM(nobs,nobs);wdM.zeros();
//   for(i=0; i < nobs; ++i){
//     arma::rowvec zi = covariates.row(i);
//     for(j=0; j < nobs; ++j){
//       double first=0;
//       double second=0;
//       if(causes(i)==1 && times(i)==times(j)){
//         first = 1;
//       }
//       if(times(j) <= times(i)){
//         second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
//       }
//       else if(times(j) > times(i) && causes(i) > 1){
//         second = exp(arma::as_scalar(betas*zi.t())) *cweight(i,j)/cweight(i,i)*dlambdat(j);
//       }
//       wdM(i,j) = first - second;
//     }
//   }
//   
//   //dMc
//   arma::mat dMc(nobs,nobs);dMc.zeros();
//   for(i=0; i < nobs; ++i){
//     arma::rowvec zci = cencovariates.row(i);
//     for(j=0; j < nobs; ++j){
//       double first=0;
//       double second=0;
//       if(causes(i)==0 && times(i)==times(j)){
//         first = 1;
//       }
//       if(times(i) >= times(j) && causes(j)==0){
//         second = exp(arma::as_scalar(gammas*zci.t())) * dlambdac(j);
//       }
//       dMc(i,j) = first - second;
//     }
//   }
//   
//   //zc_min_S1byS0hatC
//   //arma::colvec expg=exp(cencovariates * gammas.t());
//   arma::cube zc_min_S1bS0hatc(nobs,nobs,ncovsC);
//   for(j=0; j < ncovsC;++j){
//     arma::colvec zc=cencovariates.col(j);
//     arma::rowvec ratio = S1byS0hatC.row(j);
//     zc_min_S1bS0hatc.slice(j) = mycbind(zc,nobs) - myrbind(ratio,nobs);
//   }
//   
//   arma::mat qi21temp(nobs,nobs); qi21temp.zeros();
//   arma::rowvec qi21(nobs); qi21.zeros();
//   //Main loop to calculate psi
//   arma::mat z_min_S1bS0hat_wdM(nobs,nobs);
//   arma::rowvec qi11(ncovsC); qi11.zeros();
//   arma::mat test(nobs,nobs); test.zeros();
//   arma::mat precomp(1,ncovsC); precomp.zeros();
//   double qi2;
//   arma::vec denom;
//   //arma::vec locu;
//   //int locu;
//   arma::colvec s0C;
//   arma::mat tempzc;
//   arma::mat q1_all_i(1,ncovsC);
//   double qi2final;
//   arma::mat qi1(1,nobs); qi1.zeros();
//   arma::mat qi(nobs,nobs);
//   
//   
//   
//   //Calculating lambda(t) and standard error
//   int tl=idxtlambda.n_cols;
//   arma::colvec tt;
//   //double tfinal;
//   arma::mat oneoverS0hat_wdM(nobs,nobs); oneoverS0hat_wdM.zeros();
//   arma::mat first_term_W(nobs,nobs); first_term_W.zeros();
//   arma::mat second_term_W(nobs,nobs); second_term_W.zeros();
//   //arma::vec W1;arma::vec W2; arma::rowvec W3; arma::vec Wall;
//   arma::mat W1(nobs,tl); W1.zeros();arma::mat W2(nobs,tl); W2.zeros();arma::mat W3(tl,ntotal); W3.zeros();
//   arma::mat W1Fstar(nobs,tl); W1Fstar.zeros(); arma::mat W2finalFstar(nobs,tl); W2finalFstar.zeros();arma::mat W3Fstar(tl,ntotal); W3Fstar.zeros();
//   arma::vec hLambda; arma::vec hFstar;
//   arma::rowvec dlt;
//   arma::uvec clusteridx;arma::uvec allclusteridx;
//   arma::rowvec vLambda(tl); vLambda.zeros();
//   arma::rowvec Lambda(tl); Lambda.zeros();
//   arma::rowvec Fstart(tl); Fstart.zeros();
//   //arma::vec test1;
//   
//   
//   //		oneoverS0hat_wdM = 1/myrbind(S0hat,nobs) % wdM;
//   
//   //		for(k=0; k < ncovsC; ++k){
//   //			test = h(span(k),span::all,span::all);
//   //			qi11(k) = accu(test % oneoverS0hat_wdM);//Takes care of summation and integral	
//   //		}
//   
//   //		qi21temp = mycbind(expg,nobs) % oneoverS0hat_wdM;
//   //		qi21 = myColsums(qi21temp);
//   //		precomp = qi11*(iIc*nc);//Omega_hatC = iIc*nc
//   //		//arma::mat qi(nobs,nobs);
//   //		for(i=0; i < nobs; ++i){//This loop is for u in the formula.
//   //			double uu = times(i);
//   //			arma::uvec idx = find(uu <= times);
//   //			qi2 = accu(qi21.elem(idx));
//   //			arma::uvec locu = find(uu==times);
//   //			denom = S0hatC.elem(locu);
//   //			double finaldenom = denom(0);//This will take care if two time points are the same.
//   //			qi2final = qi2/finaldenom;
//   //			//tempzc = zc_min_S1bS0hatc(span::all,span(i),span::all);
//   //			//qi1 = precomp*tempzc.t();
//   //			qi.col(i) = -1*(qi1.t() + qi2final)/nc;
//   //		}
//   
//   arma::rowvec nuj(tl); 
//   
//   arma::rowvec CumLambda0(tl); CumLambda0.zeros();
//   for(t=0; t < tl; ++t){
//     int uuu = idxtlambda(t);
//     CumLambda0(t) = arma::accu(dlambdat.cols(0,uuu-1));
//   }
//   
//   //CumLambda0 = arma::cumsum(dlambdat);
//   
//   arma::mat zetaj(tl,ncovs); zetaj.zeros();
//   
//   //Estimate of adjusted CIF
//   arma::rowvec Fstar(tl); Fstar.zeros();
//   
//   for(i=0; i < tl; ++i){
//     double Fstartemp = 0;
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       Fstartemp += ( 1 - exp(-CumLambda0(i) * exp(arma::as_scalar(adjustedbetas*zj.t())) ) );
//     }
//     Fstar(i) =  Fstartemp/ntotalobs;
//   }
//   
//   //arma::rowvec nuj(nobs); nuj.zeros();
//   for(i=0; i < tl; ++i){
//     double nujtemp = 0;
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       nujtemp += exp(-CumLambda0(i) * exp(arma::as_scalar(adjustedbetas*zj.t())) + arma::as_scalar(adjustedbetas*zj.t()) ) ;
//     }
//     nuj(i) = nujtemp/ntotalobs;
//   }
//   
//   
//   for(i=0; i < tl; ++i){
//     arma::rowvec zetajtemp(ncovs); zetajtemp.zeros();
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       zetajtemp += zj * exp(-CumLambda0(i) * exp(arma::as_scalar(adjustedbetas*zj.t())) + arma::as_scalar(adjustedbetas*zj.t()) ) ;
//     }
//     zetaj.row(i) = zetajtemp/ntotalobs;
//   }
//   
//   
//   for(t=0; t < tl; ++t){
//     int uuu = idxtlambda(t);
//     
//     //Added 02/26/2021 (q^(2) is a function of t as well)
//     arma::rowvec mytemp1 = S0hat.cols(0,uuu-1);
//     oneoverS0hat_wdM = 1/myrbind(mytemp1,nobs) % wdM.cols(0,uuu-1);
//     for(k=0; k < ncovsC; ++k){
//       test = h(arma::span(k),arma::span::all,arma::span::all);
//       qi11(k) = accu(test.cols(0,uuu-1) % oneoverS0hat_wdM);//Takes care of summation and integral	
//     }
//     qi21temp = mycbind(expg,uuu) % oneoverS0hat_wdM;
//     qi21 = myColsums(qi21temp);
//     precomp = qi11*(iIc*totalnc);//Omega_hatC = iIc*nc
//     
//     for(i=0; i < nobs; ++i){//This loop is for u in the formula.
//       double uu = times(i);
//       arma::uvec idx = find(uu <= times.cols(0,uuu-1));
//       qi2 = accu(qi21.elem(idx));
//       arma::uvec locu = find(uu==times);
//       denom = S0hatC.elem(locu);
//       double finaldenom = denom(0);//This will take care if two time points are the same.
//       qi2final = qi2/finaldenom;
//       //tempzc = zc_min_S1bS0hatc(span::all,span(i),span::all);
//       //qi1 = precomp*tempzc.t();
//       qi.col(i) = -1*(qi1.t() + qi2final)/nc;
//       //qi.col(i) = -1*(qi1.t())/totalnc;
//     }
//     //Added 02/26/2021 end
//     
//     first_term_W = wdM/myrbind(S0hat,nobs);
//     W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
//     second_term_W = qi % dMc;
//     //W2 = myRowsums(second_term_W.cols(0,uuu-1));
//     W2.col(t) = myRowsums(second_term_W);
//     dlt = dlambdat.cols(0,uuu-1);
//     hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
//     W3.row(t) = hLambda.t() * (iI*totalnc) * (etapsi);
//     //Wall = W1 + W2 + W3.t();
//     //			Wall = (W1 + W2);
//     
//     //			double test1=0;
//     //			double test2=0;
//     //			double test3=0;
//     //			for(j=1; j <= totalnc; ++j){
//     //				clusteridx = find(cluster==j);
//     //				allclusteridx = find(allcluster==j);
//     //				//W3 = hLambda.t() * (iI *totalnc) * etapsi.cols(allclusteridx);
//     //				W3 = hLambda.t() * (iI *nc) * etapsi.cols(clusteridx);
//     //				test1 = arma::accu(Wall.rows(clusteridx));
//     //				test2 = arma::accu(W3);
//     //				test3 += (test1 + nc/totalnc*test2) * (test1 + nc/totalnc*test2);
//     //			}
//     //			
//     //			vLambda(t) = test3/(nc*nc);
//     Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
//     //Fstart(t) = Fstar(uuu-1);
//     
//     //Variance of adjusted CIF
//     arma::rowvec zj = zetaj.row(t);
//     arma::colvec zjc = zj.t();
//     hFstar = myRowsums( (mycbind(zjc,uuu) -  nuj(t) * S1byS0hat.cols(0,uuu-1)) % myrbind(dlt,ncovs));
//     W3Fstar.row(t) = hFstar.t() * (iI*totalnc) * (etapsi);
//     
//     W2finalFstar.col(t) = nuj(t)*W2.col(t);
//     W1Fstar.col(t) = nuj(t)*W1.col(t);
//     
//   }	
//   
//   
//   Rcpp::List res(13);
//   res[0]=W1;
//   res[1]=W2;
//   res[2]=W3;
//   res[3]=zc_min_S1bS0hatc;
//   res[4]=dMc;
//   res[5]=Lambda;
//   res[6]=CumLambda0;
//   res[7]=Fstar;
//   res[8]=W1Fstar;
//   res[9]=W2finalFstar;
//   res[10]=W3Fstar;
//   res[11]=Fstar;
//   res[12]=nuj;
//   
//   return(res);
//   
//   
// }

// [[Rcpp::export]]
Rcpp::List se_beta_lambda_strata_KM_diff( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const int& nobs, 
    const int& ntotal,
    const int& ncovs,
    const arma::rowvec& cweight,//just a vector when KM estimate is used for censoring
    const arma::rowvec& betas,
    const arma::rowvec& cluster,
    const arma::rowvec& allcluster,
    const int& nc,
    const int& totalnc,
    const arma::mat& iI,
    const arma::rowvec& idxtlambda,
    const arma::mat etapsi,
    const int& ntotalobs,
    const arma::mat& allcovariates,
    const arma::rowvec Zpred){
  
  int i,j,t,u,ttt;
  
  //double loglikli=0;
  //arma::colvec score(ncovs);
  //score.fill(0.0);
  //arma::mat infor(ncovs,ncovs);
  //infor.fill(0.0);
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  //arma::cube S2byS0hat(ncovs,ncovs,nobs);
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.zeros();
    arma::mat s2(ncovs,ncovs);
    s2.zeros();
    double wye=0;
    
    //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
    
    arma::rowvec zi = covariates.row(i);
    //loglikli += arma::as_scalar(betas*zi.t());
    //score += zi.t();
    
    for(j=0; j<nobs; ++j){
      if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
      arma::rowvec zj = covariates.row(j);
      if(times(i) <= times(j)){
        wye = exp(arma::as_scalar(betas*zj.t()));
      }
      else if (times(i) > times(j) && causes(j)>1){
        wye = exp(arma::as_scalar(betas*zj.t())) * cweight(i)/cweight(j);
      }
      else continue;
      s0 += wye;
      s1 += wye * zj.t();
      s2 += wye * zj.t() * zj;
    }	
    S0hat(i) = s0/nc;
    S1hat.col(i) = s1/nc;
    S1byS0hat.col(i) = s1/s0;
    //S2byS0hat.slice(i) = s2/s0;
    
  }
  
  //dlambda10(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(causes(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += 1/(S0hat(j)*nc);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  
  
  //eta_i: 
  //	arma::mat etai(ncovs,nobs);
  //	etai.zeros();
  
  //	for(i=0; i < nobs; ++i){
  //		arma::rowvec zi = covariates.row(i);
  //		arma::colvec etaifirst(ncovs); etaifirst.zeros();
  //		for(j=0; j < nobs; ++j){
  //			if(causes(i)==1 && times(j)==times(i)){
  //				etaifirst += (zi.t() - S1byS0hat.col(j));
  //			}
  //			if(causes(i)>1 && times(j) > times(i)){
  //				etaifirst -= (zi.t() - S1byS0hat.col(j)) * cweight(j)/cweight(i) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
  
  //			}
  //			else if(times(j) <= times(i)){
  //				etaifirst -= (zi.t() - S1byS0hat.col(j)) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
  //			}
  //		}
  
  //		etai.col(i) = etaifirst;
  //	}
  
  //q1(u): 
  arma::mat q(ncovs,nobs); q.zeros();
  for(u=0; u < nobs; ++u){
    if(causes(u)!=0) continue; //We need this when we calculate psi and also makes the algorithm efficient
    arma::colvec qtemp(ncovs); qtemp.zeros();
    for(i=0; i < nobs; ++i){
      if(causes(i) > 1 && times(i) < times(u)){
        arma::rowvec zi = covariates.row(i);
        for(t=0; t < nobs; ++t){
          if(times(u) <= times(t) && causes(t)==1){
            qtemp += (zi.t() - S1byS0hat.col(t)) * exp(arma::as_scalar(betas*zi.t()))*cweight(t)/cweight(i)*dlambdat(t);
          }
        }
      }
    }
    q.col(u) = qtemp/nc;
    
  }
  
  //S0c(t): 
  arma::rowvec S0hatc(nobs);S0hatc.zeros();
  for(j=0; j < nobs; ++j){
    double S0c=0;
    for(i=0; i < nobs; ++i){
      if(times(i) >= times(j)){
        S0c+=1;
      }
    }
    S0hatc(j)=S0c/nc;
  }	
  
  //dlambdac(t)
  arma::rowvec dlambdac(nobs);dlambdac.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatempc=0;
    if(causes(j)==0){
      for(i=0; i < nobs; ++i){
        if(times(j) == times(i)){
          dlambdatempc += 1/(S0hatc(j)*nc);
        }
      }
    }
      dlambdac(j)=dlambdatempc;
  }
  
  //psi
  //	arma::mat psi(ncovs,nobs); psi.zeros();
  //	for(i=0; i < nobs; ++i){
  //		arma:: colvec psitemp(ncovs); psitemp.zeros();		
  //		for(j=0; j < nobs; ++j){
  //			if(times(i)==times(j) && causes(i)==0){
  //				psitemp += q.col(i)/S0hatc(i);
  //			}
  //			if(causes(j)==0 && times(i) >= times(j)){
  //				psitemp -= q.col(j) * dlambdac(j)/S0hatc(j);
  //			}
  //		}
  //		psi.col(i) = psitemp;
  //	}
  //Gathering cluster information to calculate the variance of beta
  //	arma::uvec clusteridxbeta;
  //	arma::vec vmiddle; vmiddle.zeros();
  //	arma::mat vtotal(ncovs,ncovs); vtotal.zeros();
  //	arma::mat vbeta(ncovs,ncovs); vbeta.zeros();
  //	for(j=1; j <= nc; ++j){
  //		clusteridxbeta=find(cluster==j);
  //		vmiddle = myRowsums(etai.cols(clusteridxbeta) + psi.cols(clusteridxbeta));
  //		vtotal += vmiddle * vmiddle.t();
  //	}
  //	vbeta = iI * vtotal * iI;
  
  //Calculating lambda(t) and standard error
  
  //wdM
  arma::mat wdM(nobs,nobs);wdM.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==1 && times(i)==times(j)){
        first = 1;
      }
      if(times(j) <= times(i)){
        second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
      else if(times(j) > times(i) && causes(i) > 1){
        second = exp(arma::as_scalar(betas*zi.t())) *cweight(j)/cweight(i)*dlambdat(j);
      }
      wdM(i,j) = first - second;
    }
  }
  
  //dMc
  arma::mat dMc(nobs,nobs);dMc.zeros();
  for(i=0; i < nobs; ++i){
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==0 && times(i)==times(j)){
        first = 1;
      }
      if(times(i) >= times(j) && causes(j)==0){
        second = dlambdac(j);
      }
      dMc(i,j) = first - second;
    }
  }	
  
  
  int tl=idxtlambda.n_cols;
  arma::mat first_term_W = wdM/myrbind(S0hat,nobs);
  arma::vec hLambda; arma::vec hFstar;
  arma::rowvec dlt;
  arma::uvec clusteridx;arma::uvec allclusteridx;
  arma::rowvec vLambda(tl); vLambda.zeros();
  arma::rowvec Lambda(tl); Lambda.zeros();
  arma::rowvec Fstart(tl); Fstart.zeros();
  //arma::vec W1;arma::vec W2(nobs); arma::rowvec W3; arma::vec Wall; arma::vec W3new;
  arma::vec W2(nobs);
  arma::mat W1(nobs,tl); W1.zeros();arma::mat W2final(nobs,tl); W2final.zeros();arma::mat W3(tl,ntotal); W3.zeros();
  arma::mat W1Fstar(nobs,tl); W1Fstar.zeros(); arma::mat W2finalFstar(nobs,tl); W2finalFstar.zeros();arma::mat W3Fstar(tl,ntotal); W3Fstar.zeros();
  arma::rowvec q2(nobs);
  //double test4=0;
  arma::rowvec nuj(tl); 
  
  // Added on 11/03/2021
  arma::vec hLambdapred;
  arma::mat W3pred(tl,ntotal); W3pred.zeros();
  
  arma::rowvec CumLambda0(tl); CumLambda0.zeros();
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);
    CumLambda0(t) = arma::accu(dlambdat.cols(0,uuu-1));
  }
  //CumLambda0 = arma::cumsum(dlambdat);
  
  arma::mat zetaj(tl,ncovs); zetaj.zeros();
  
  //Estimate of adjusted CIF
  arma::rowvec Fstar(tl); Fstar.zeros();
  
  for(i=0; i < tl; ++i){
    double Fstartemp = 0;
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = allcovariates.row(j);
      Fstartemp += ( 1 - exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) ) );
    }
    Fstar(i) =  Fstartemp/ntotalobs;
  }
  
  //arma::rowvec nuj(nobs); nuj.zeros();
  for(i=0; i < tl; ++i){
    double nujtemp = 0;
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = allcovariates.row(j);
      nujtemp += exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) + arma::as_scalar(betas*zj.t()) ) ;
    }
    nuj(i) = nujtemp/ntotalobs;
  }
  
  
  for(i=0; i < tl; ++i){
    arma::rowvec zetajtemp(ncovs); zetajtemp.zeros();
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = allcovariates.row(j);
      zetajtemp += zj * exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) + arma::as_scalar(betas*zj.t()) ) ;
    }
    zetaj.row(i) = zetajtemp/ntotalobs;
  }
  
  //Estimate of F(t|Zpred). Added on 11/03/2021
  arma::rowvec Fpred(tl); Fpred.zeros();
  arma::rowvec Fpredfirst(tl); Fpredfirst.zeros();
  for(i=0; i < tl; ++i){
    Fpred(i) = 1 - exp(-CumLambda0(i)*exp(arma::as_scalar(betas*Zpred.t())));
    Fpredfirst(i) = exp(arma::as_scalar(betas*Zpred.t()) - CumLambda0(i)*exp(arma::as_scalar(betas*Zpred.t())));
  }
  
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);		
    W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
    //FOR THE SECOND TERM, DO SIMILAR AS q1(u)
    //second_term_W = qi % dMc;
    //W2 = myRowsums(second_term_W.cols(0,uuu-1));
    
    //q2(u,t): 
    q2.zeros();
    for(u=0; u < nobs; ++u){
      if(causes(u)!=0) continue; //We need this when we calculate psi and also makes the algorithm efficient
      double qtemp1=0;
      for(i=0; i < nobs; ++i){
        if(causes(i) > 1 && times(i) < times(u)){
          arma::rowvec zi = covariates.row(i);
          for(ttt=0; ttt < uuu; ++ttt){
            if(times(u) <= times(ttt) && causes(ttt)==1){
              qtemp1 += 1/S0hat(ttt) * exp(arma::as_scalar(betas*zi.t()))* cweight(ttt)/cweight(i) * dlambdat(ttt);
            }
          }
        }
      }
      q2(u) = qtemp1/nc;		
    }
    
    //Intergral of q2(u,t) dMc(u)
    W2.zeros();
    for(i=0; i < nobs; ++i){
      double psitemp1=0;		
      for(j=0; j < nobs; ++j){
        if(times(i)==times(j) && causes(i)==0){
          psitemp1 += q2(i)/S0hatc(i);
        }
        if(causes(j)==0 && times(i) >= times(j)){
          psitemp1 -= q2(j) * dlambdac(j)/S0hatc(j);
        }
      }
      W2(i) = psitemp1;
    }
    W2final.col(t) = W2;
    
    
    
    dlt = dlambdat.cols(0,uuu-1);
    hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
    
    W3.row(t) = hLambda.t() * (iI*totalnc) * (etapsi);
    //		//W3new = W3.t();
    
    //		Wall = W1  + W2;
    
    //		double test1=0;
    //		double test2=0;
    //		double test3=0;
    //		double ql=nc/totalnc;
    
    //		for(j=1; j <= totalnc; ++j){
    //			clusteridx = find(cluster==j);
    //			allclusteridx = find(allcluster==j);
    //			W3 = hLambda.t() * (iI*totalnc) * (etapsi.cols(allclusteridx));
    //			W3new = W3.t();
    //			test1 += ( arma::accu(Wall.rows(clusteridx)) + accu(W3new) )  * ( arma::accu(Wall.rows(clusteridx)) + accu(W3new) );
    //		}
    //					
    //		vLambda(t) = test1/(nc*nc);
    Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
    //Fstart(t) = Fstar(uuu-1);
    //			//test4 = accu(Wall.rows(clusteridx));
    
    
    //Variance of adjusted CIF
    arma::rowvec zj = zetaj.row(t);
    arma::colvec zjc = zj.t();
    hFstar = myRowsums( (mycbind(zjc,uuu) -  nuj(t) * S1byS0hat.cols(0,uuu-1)) % myrbind(dlt,ncovs));
    W3Fstar.row(t) = hFstar.t() * (iI*totalnc) * (etapsi);
    
    W2finalFstar.col(t) = nuj(t)*W2final.col(t);
    W1Fstar.col(t) = nuj(t)*W1.col(t);
    
    // Added on 11/03/2021
    //Variance of F(t|Z0)
    arma::colvec Zpredc = Zpred.t();
    hLambdapred = myRowsums( (mycbind(Zpredc,uuu) - S1byS0hat.cols(0,uuu-1) ) % myrbind(dlt,ncovs));
    W3pred.row(t) = hLambdapred.t() * (iI*totalnc) * (etapsi);
    
  }
  
  
  
  Rcpp::List res(13);
  //res[0]=dlambdat;
  res[0]=W1;
  res[1]=W2final;
  res[2]=W3;
  res[3]=Lambda;
  res[4]=CumLambda0;
  res[5]=Fstar;
  res[6]=W1Fstar;
  res[7]=W2finalFstar;
  res[8]=W3Fstar;
  res[9]=Fstar;
  res[10]=Fpred;
  res[11]=W3pred;
  res[12]=Fpredfirst;
  return(res);
  
  
}


// Rcpp::List se_beta_lambda_strata_KM_diff( 
//     const arma::rowvec& times,
//     const arma::rowvec& causes,
//     const arma::mat& covariates,
//     const int& nobs, 
//     const int& ntotal,
//     const int& ncovs,
//     const arma::rowvec& cweight,//just a vector when KM estimate is used for censoring
//     const arma::rowvec& betas,
//     const arma::rowvec& cluster,
//     const arma::rowvec& allcluster,
//     const int& nc,
//     const int& totalnc,
//     const arma::mat& iI,
//     const arma::rowvec& idxtlambda,
//     const arma::mat etapsi,
//     const int& ntotalobs,
//     const arma::mat& allcovariates){
//   
//   int i,j,t,u,ttt;
//   
//   //double loglikli=0;
//   //arma::colvec score(ncovs);
//   //score.fill(0.0);
//   //arma::mat infor(ncovs,ncovs);
//   //infor.fill(0.0);
//   arma::rowvec S0hat(nobs);S0hat.zeros();
//   arma::mat S1hat(ncovs,nobs);S1hat.zeros();
//   arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
//   //arma::cube S2byS0hat(ncovs,ncovs,nobs);
//   
//   for(i=0; i<nobs; ++i){
//     double s0=0;
//     arma::colvec s1(ncovs);
//     s1.zeros();
//     arma::mat s2(ncovs,ncovs);
//     s2.zeros();
//     double wye=0;
//     
//     //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
//     
//     arma::rowvec zi = covariates.row(i);
//     //loglikli += arma::as_scalar(betas*zi.t());
//     //score += zi.t();
//     
//     for(j=0; j<nobs; ++j){
//       if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
//       arma::rowvec zj = covariates.row(j);
//       if(times(i) <= times(j)){
//         wye = exp(arma::as_scalar(betas*zj.t()));
//       }
//       else if (times(i) > times(j) && causes(j)>1){
//         wye = exp(arma::as_scalar(betas*zj.t())) * cweight(i)/cweight(j);
//       }
//       else continue;
//       s0 += wye;
//       s1 += wye * zj.t();
//       s2 += wye * zj.t() * zj;
//     }	
//     S0hat(i) = s0/nc;
//     S1hat.col(i) = s1/nc;
//     S1byS0hat.col(i) = s1/s0;
//     //S2byS0hat.slice(i) = s2/s0;
//     
//   }
//   
//   //dlambda10(t)
//   arma::rowvec dlambdat(nobs);dlambdat.zeros();
//   for(j=0; j < nobs; ++j){
//     double dlambdatemp=0;
//     if(causes(j)!=1) continue;
//     for(i=0; i < nobs; ++i){
//       if(times(j) == times(i)){
//         dlambdatemp += 1/(S0hat(j)*nc);
//       }
//     }
//     dlambdat(j)=dlambdatemp;
//   }
//   
//   
//   
//   //eta_i: 
//   //	arma::mat etai(ncovs,nobs);
//   //	etai.zeros();
//   
//   //	for(i=0; i < nobs; ++i){
//   //		arma::rowvec zi = covariates.row(i);
//   //		arma::colvec etaifirst(ncovs); etaifirst.zeros();
//   //		for(j=0; j < nobs; ++j){
//   //			if(causes(i)==1 && times(j)==times(i)){
//   //				etaifirst += (zi.t() - S1byS0hat.col(j));
//   //			}
//   //			if(causes(i)>1 && times(j) > times(i)){
//   //				etaifirst -= (zi.t() - S1byS0hat.col(j)) * cweight(j)/cweight(i) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
//   
//   //			}
//   //			else if(times(j) <= times(i)){
//   //				etaifirst -= (zi.t() - S1byS0hat.col(j)) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
//   //			}
//   //		}
//   
//   //		etai.col(i) = etaifirst;
//   //	}
//   
//   //q1(u): 
//   arma::mat q(ncovs,nobs); q.zeros();
//   for(u=0; u < nobs; ++u){
//     if(causes(u)!=0) continue; //We need this when we calculate psi and also makes the algorithm efficient
//     arma::colvec qtemp(ncovs); qtemp.zeros();
//     for(i=0; i < nobs; ++i){
//       if(causes(i) > 1 && times(i) < times(u)){
//         arma::rowvec zi = covariates.row(i);
//         for(t=0; t < nobs; ++t){
//           if(times(u) <= times(t) && causes(t)==1){
//             qtemp += (zi.t() - S1byS0hat.col(t)) * exp(arma::as_scalar(betas*zi.t()))*cweight(t)/cweight(i)*dlambdat(t);
//           }
//         }
//       }
//     }
//     q.col(u) = qtemp/nc;
//     
//   }
//   
//   //S0c(t): 
//   arma::rowvec S0hatc(nobs);S0hatc.zeros();
//   for(j=0; j < nobs; ++j){
//     double S0c=0;
//     for(i=0; i < nobs; ++i){
//       if(times(i) >= times(j)){
//         S0c+=1;
//       }
//     }
//     S0hatc(j)=S0c/nc;
//   }	
//   
//   //dlambdac(t)
//   arma::rowvec dlambdac(nobs);dlambdac.zeros();
//   for(j=0; j < nobs; ++j){
//     double dlambdatempc=0;
//     if(causes(j)==0){
//       for(i=0; i < nobs; ++i){
//         if(times(j) == times(i)){
//           dlambdatempc += 1/(S0hatc(j)*nc);
//         }
//       }
//     }
// 
//       dlambdac(j)=dlambdatempc;
//   }
//   
//   //psi
//   //	arma::mat psi(ncovs,nobs); psi.zeros();
//   //	for(i=0; i < nobs; ++i){
//   //		arma:: colvec psitemp(ncovs); psitemp.zeros();		
//   //		for(j=0; j < nobs; ++j){
//   //			if(times(i)==times(j) && causes(i)==0){
//   //				psitemp += q.col(i)/S0hatc(i);
//   //			}
//   //			if(causes(j)==0 && times(i) >= times(j)){
//   //				psitemp -= q.col(j) * dlambdac(j)/S0hatc(j);
//   //			}
//   //		}
//   //		psi.col(i) = psitemp;
//   //	}
//   //Gathering cluster information to calculate the variance of beta
//   //	arma::uvec clusteridxbeta;
//   //	arma::vec vmiddle; vmiddle.zeros();
//   //	arma::mat vtotal(ncovs,ncovs); vtotal.zeros();
//   //	arma::mat vbeta(ncovs,ncovs); vbeta.zeros();
//   //	for(j=1; j <= nc; ++j){
//   //		clusteridxbeta=find(cluster==j);
//   //		vmiddle = myRowsums(etai.cols(clusteridxbeta) + psi.cols(clusteridxbeta));
//   //		vtotal += vmiddle * vmiddle.t();
//   //	}
//   //	vbeta = iI * vtotal * iI;
//   
//   //Calculating lambda(t) and standard error
//   
//   //wdM
//   arma::mat wdM(nobs,nobs);wdM.zeros();
//   for(i=0; i < nobs; ++i){
//     arma::rowvec zi = covariates.row(i);
//     for(j=0; j < nobs; ++j){
//       double first=0;
//       double second=0;
//       if(causes(i)==1 && times(i)==times(j)){
//         first = 1;
//       }
//       if(times(j) <= times(i)){
//         second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
//       }
//       else if(times(j) > times(i) && causes(i) > 1){
//         second = exp(arma::as_scalar(betas*zi.t())) *cweight(j)/cweight(i)*dlambdat(j);
//       }
//       wdM(i,j) = first - second;
//     }
//   }
//   
//   //dMc
//   arma::mat dMc(nobs,nobs);dMc.zeros();
//   for(i=0; i < nobs; ++i){
//     for(j=0; j < nobs; ++j){
//       double first=0;
//       double second=0;
//       if(causes(i)==0 && times(i)==times(j)){
//         first = 1;
//       }
//       if(times(i) >= times(j) && causes(j)==0){
//         second = dlambdac(j);
//       }
//       dMc(i,j) = first - second;
//     }
//   }	
//   
//   
//   int tl=idxtlambda.n_cols;
//   arma::mat first_term_W = wdM/myrbind(S0hat,nobs);
//   arma::vec hLambda; arma::vec hFstar;
//   arma::rowvec dlt;
//   arma::uvec clusteridx;arma::uvec allclusteridx;
//   arma::rowvec vLambda(tl); vLambda.zeros();
//   arma::rowvec Lambda(tl); Lambda.zeros();
//   arma::rowvec Fstart(tl); Fstart.zeros();
//   //arma::vec W1;arma::vec W2(nobs); arma::rowvec W3; arma::vec Wall; arma::vec W3new;
//   arma::vec W2(nobs);
//   arma::mat W1(nobs,tl); W1.zeros();arma::mat W2final(nobs,tl); W2final.zeros();arma::mat W3(tl,ntotal); W3.zeros();
//   arma::mat W1Fstar(nobs,tl); W1Fstar.zeros(); arma::mat W2finalFstar(nobs,tl); W2finalFstar.zeros();arma::mat W3Fstar(tl,ntotal); W3Fstar.zeros();
//   arma::rowvec q2(nobs);
//   //double test4=0;
//   arma::rowvec nuj(tl); 
//   
//   arma::rowvec CumLambda0(tl); CumLambda0.zeros();
//   for(t=0; t < tl; ++t){
//     int uuu = idxtlambda(t);
//     CumLambda0(t) = arma::accu(dlambdat.cols(0,uuu-1));
//   }
//   //CumLambda0 = arma::cumsum(dlambdat);
//   
//   arma::mat zetaj(tl,ncovs); zetaj.zeros();
//   
//   //Estimate of adjusted CIF
//   arma::rowvec Fstar(tl); Fstar.zeros();
//   
//   for(i=0; i < tl; ++i){
//     double Fstartemp = 0;
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       Fstartemp += ( 1 - exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) ) );
//     }
//     Fstar(i) =  Fstartemp/ntotalobs;
//   }
//   
//   //arma::rowvec nuj(nobs); nuj.zeros();
//   for(i=0; i < tl; ++i){
//     double nujtemp = 0;
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       nujtemp += exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) + arma::as_scalar(betas*zj.t()) ) ;
//     }
//     nuj(i) = nujtemp/ntotalobs;
//   }
//   
//   
//   for(i=0; i < tl; ++i){
//     arma::rowvec zetajtemp(ncovs); zetajtemp.zeros();
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       zetajtemp += zj * exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) + arma::as_scalar(betas*zj.t()) ) ;
//     }
//     zetaj.row(i) = zetajtemp/ntotalobs;
//   }
//   
//   
//   for(t=0; t < tl; ++t){
//     int uuu = idxtlambda(t);		
//     W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
//     //FOR THE SECOND TERM, DO SIMILAR AS q1(u)
//     //second_term_W = qi % dMc;
//     //W2 = myRowsums(second_term_W.cols(0,uuu-1));
//     
//     //q2(u,t): 
//     q2.zeros();
//     for(u=0; u < nobs; ++u){
//       if(causes(u)!=0) continue; //We need this when we calculate psi and also makes the algorithm efficient
//       double qtemp1=0;
//       for(i=0; i < nobs; ++i){
//         if(causes(i) > 1 && times(i) < times(u)){
//           arma::rowvec zi = covariates.row(i);
//           for(ttt=0; ttt < uuu; ++ttt){
//             if(times(u) <= times(ttt) && causes(ttt)==1){
//               qtemp1 += 1/S0hat(ttt) * exp(arma::as_scalar(betas*zi.t()))* cweight(ttt)/cweight(i) * dlambdat(ttt);
//             }
//           }
//         }
//       }
//       q2(u) = qtemp1/nc;		
//     }
//     
//     //Intergral of q2(u,t) dMc(u)
//     W2.zeros();
//     for(i=0; i < nobs; ++i){
//       double psitemp1=0;		
//       for(j=0; j < nobs; ++j){
//         if(times(i)==times(j) && causes(i)==0){
//           psitemp1 += q2(i)/S0hatc(i);
//         }
//         if(causes(j)==0 && times(i) >= times(j)){
//           psitemp1 -= q2(j) * dlambdac(j)/S0hatc(j);
//         }
//       }
//       W2(i) = psitemp1;
//     }
//     W2final.col(t) = W2;
//     
//     
//     
//     dlt = dlambdat.cols(0,uuu-1);
//     hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
//     
//     W3.row(t) = hLambda.t() * (iI*totalnc) * (etapsi);
//     //		//W3new = W3.t();
//     
//     //		Wall = W1  + W2;
//     
//     //		double test1=0;
//     //		double test2=0;
//     //		double test3=0;
//     //		double ql=nc/totalnc;
//     
//     //		for(j=1; j <= totalnc; ++j){
//     //			clusteridx = find(cluster==j);
//     //			allclusteridx = find(allcluster==j);
//     //			W3 = hLambda.t() * (iI*totalnc) * (etapsi.cols(allclusteridx));
//     //			W3new = W3.t();
//     //			test1 += ( arma::accu(Wall.rows(clusteridx)) + accu(W3new) )  * ( arma::accu(Wall.rows(clusteridx)) + accu(W3new) );
//     //		}
//     //					
//     //		vLambda(t) = test1/(nc*nc);
//     Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
//     //Fstart(t) = Fstar(uuu-1);
//     //			//test4 = accu(Wall.rows(clusteridx));
//     
//     
//     //Variance of adjusted CIF
//     arma::rowvec zj = zetaj.row(t);
//     arma::colvec zjc = zj.t();
//     hFstar = myRowsums( (mycbind(zjc,uuu) -  nuj(t) * S1byS0hat.cols(0,uuu-1)) % myrbind(dlt,ncovs));
//     W3Fstar.row(t) = hFstar.t() * (iI*totalnc) * (etapsi);
//     
//     W2finalFstar.col(t) = nuj(t)*W2final.col(t);
//     W1Fstar.col(t) = nuj(t)*W1.col(t);
//     
//   }
//   
//   
//   
//   Rcpp::List res(10);
//   //res[0]=dlambdat;
//   res[0]=W1;
//   res[1]=W2final;
//   res[2]=W3;
//   res[3]=Lambda;
//   res[4]=CumLambda0;
//   res[5]=Fstar;
//   res[6]=W1Fstar;
//   res[7]=W2finalFstar;
//   res[8]=W3Fstar;
//   res[9]=Fstar;
//   return(res);
//   
//   
// }

// [[Rcpp::export]]
Rcpp::List se_beta_lambda_strata_KM_diff_unstratified( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const int& nobs, 
    const int& ntotal,
    const int& ncovs,
    const arma::rowvec& cweight,//just a vector when KM estimate is used for censoring
    const arma::rowvec& betas,
    const arma::rowvec& cluster,
    const arma::rowvec& allcluster,
    const int& nc,
    const int& totalnc,
    const arma::mat& iI,
    const arma::rowvec& idxtlambda,
    const arma::mat etapsi,
    const int& ntotalobs,
    const arma::mat& adjustedcovariates,
    const arma::rowvec& adjustedbetas,
    const arma::rowvec Zpred){
  
  int i,j,t,u,ttt;
  
  //double loglikli=0;
  //arma::colvec score(ncovs);
  //score.fill(0.0);
  //arma::mat infor(ncovs,ncovs);
  //infor.fill(0.0);
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  //arma::cube S2byS0hat(ncovs,ncovs,nobs);
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.zeros();
    arma::mat s2(ncovs,ncovs);
    s2.zeros();
    double wye=0;
    
    //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
    
    arma::rowvec zi = covariates.row(i);
    //loglikli += arma::as_scalar(betas*zi.t());
    //score += zi.t();
    
    for(j=0; j<nobs; ++j){
      if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
      arma::rowvec zj = covariates.row(j);
      if(times(i) <= times(j)){
        wye = exp(arma::as_scalar(betas*zj.t()));
      }
      else if (times(i) > times(j) && causes(j)>1){
        wye = exp(arma::as_scalar(betas*zj.t())) * cweight(i)/cweight(j);
      }
      else continue;
      s0 += wye;
      s1 += wye * zj.t();
      s2 += wye * zj.t() * zj;
    }	
    S0hat(i) = s0/nc;
    S1hat.col(i) = s1/nc;
    S1byS0hat.col(i) = s1/s0;
    //S2byS0hat.slice(i) = s2/s0;
    
  }
  
  //dlambda10(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(causes(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += 1/(S0hat(j)*nc);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  
  
  //eta_i: 
  //	arma::mat etai(ncovs,nobs);
  //	etai.zeros();
  
  //	for(i=0; i < nobs; ++i){
  //		arma::rowvec zi = covariates.row(i);
  //		arma::colvec etaifirst(ncovs); etaifirst.zeros();
  //		for(j=0; j < nobs; ++j){
  //			if(causes(i)==1 && times(j)==times(i)){
  //				etaifirst += (zi.t() - S1byS0hat.col(j));
  //			}
  //			if(causes(i)>1 && times(j) > times(i)){
  //				etaifirst -= (zi.t() - S1byS0hat.col(j)) * cweight(j)/cweight(i) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
  
  //			}
  //			else if(times(j) <= times(i)){
  //				etaifirst -= (zi.t() - S1byS0hat.col(j)) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
  //			}
  //		}
  
  //		etai.col(i) = etaifirst;
  //	}
  
  //q1(u): 
  arma::mat q(ncovs,nobs); q.zeros();
  for(u=0; u < nobs; ++u){
    if(causes(u)!=0) continue; //We need this when we calculate psi and also makes the algorithm efficient
    arma::colvec qtemp(ncovs); qtemp.zeros();
    for(i=0; i < nobs; ++i){
      if(causes(i) > 1 && times(i) < times(u)){
        arma::rowvec zi = covariates.row(i);
        for(t=0; t < nobs; ++t){
          if(times(u) <= times(t) && causes(t)==1){
            qtemp += (zi.t() - S1byS0hat.col(t)) * exp(arma::as_scalar(betas*zi.t()))*cweight(t)/cweight(i)*dlambdat(t);
          }
        }
      }
    }
    q.col(u) = qtemp/nc;
    
  }
  
  //S0c(t): 
  arma::rowvec S0hatc(nobs);S0hatc.zeros();
  for(j=0; j < nobs; ++j){
    double S0c=0;
    for(i=0; i < nobs; ++i){
      if(times(i) >= times(j)){
        S0c+=1;
      }
    }
    S0hatc(j)=S0c/nc;
  }	
  
  //dlambdac(t)
  arma::rowvec dlambdac(nobs);dlambdac.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatempc=0;
    if(causes(j)==0){
      for(i=0; i < nobs; ++i){
        if(times(j) == times(i)){
          dlambdatempc += 1/(S0hatc(j)*nc);
        }
      }
    }
    
    dlambdac(j)=dlambdatempc;
  }
  
  //psi
  //	arma::mat psi(ncovs,nobs); psi.zeros();
  //	for(i=0; i < nobs; ++i){
  //		arma:: colvec psitemp(ncovs); psitemp.zeros();		
  //		for(j=0; j < nobs; ++j){
  //			if(times(i)==times(j) && causes(i)==0){
  //				psitemp += q.col(i)/S0hatc(i);
  //			}
  //			if(causes(j)==0 && times(i) >= times(j)){
  //				psitemp -= q.col(j) * dlambdac(j)/S0hatc(j);
  //			}
  //		}
  //		psi.col(i) = psitemp;
  //	}
  //Gathering cluster information to calculate the variance of beta
  //	arma::uvec clusteridxbeta;
  //	arma::vec vmiddle; vmiddle.zeros();
  //	arma::mat vtotal(ncovs,ncovs); vtotal.zeros();
  //	arma::mat vbeta(ncovs,ncovs); vbeta.zeros();
  //	for(j=1; j <= nc; ++j){
  //		clusteridxbeta=find(cluster==j);
  //		vmiddle = myRowsums(etai.cols(clusteridxbeta) + psi.cols(clusteridxbeta));
  //		vtotal += vmiddle * vmiddle.t();
  //	}
  //	vbeta = iI * vtotal * iI;
  
  //Calculating lambda(t) and standard error
  
  //wdM
  arma::mat wdM(nobs,nobs);wdM.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==1 && times(i)==times(j)){
        first = 1;
      }
      if(times(j) <= times(i)){
        second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
      else if(times(j) > times(i) && causes(i) > 1){
        second = exp(arma::as_scalar(betas*zi.t())) *cweight(j)/cweight(i)*dlambdat(j);
      }
      wdM(i,j) = first - second;
    }
  }
  
  //dMc
  arma::mat dMc(nobs,nobs);dMc.zeros();
  for(i=0; i < nobs; ++i){
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==0 && times(i)==times(j)){
        first = 1;
      }
      if(times(i) >= times(j) && causes(j)==0){
        second = dlambdac(j);
      }
      dMc(i,j) = first - second;
    }
  }	
  
  
  int tl=idxtlambda.n_cols;
  arma::mat first_term_W = wdM/myrbind(S0hat,nobs);
  arma::vec hLambda; arma::vec hFstar;
  arma::rowvec dlt;
  arma::uvec clusteridx;arma::uvec allclusteridx;
  arma::rowvec vLambda(tl); vLambda.zeros();
  arma::rowvec Lambda(tl); Lambda.zeros();
  arma::rowvec Fstart(tl); Fstart.zeros();
  //arma::vec W1;arma::vec W2(nobs); arma::rowvec W3; arma::vec Wall; arma::vec W3new;
  arma::vec W2(nobs);
  arma::mat W1(nobs,tl); W1.zeros();arma::mat W2final(nobs,tl); W2final.zeros();arma::mat W3(tl,ntotal); W3.zeros();
  arma::mat W1Fstar(nobs,tl); W1Fstar.zeros(); arma::mat W2finalFstar(nobs,tl); W2finalFstar.zeros();arma::mat W3Fstar(tl,ntotal); W3Fstar.zeros();
  arma::rowvec q2(nobs);
  //double test4=0;
  // Added on 11/03/2021
  arma::vec hLambdapred;
  arma::mat W3pred(tl,ntotal); W3pred.zeros();
  
  arma::rowvec nuj(tl); 
  
  arma::rowvec CumLambda0(tl); CumLambda0.zeros();
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);
    CumLambda0(t) = arma::accu(dlambdat.cols(0,uuu-1));
  }
  //CumLambda0 = arma::cumsum(dlambdat);
  
  arma::mat zetaj(tl,ncovs); zetaj.zeros();
  
  //Estimate of adjusted CIF
  arma::rowvec Fstar(tl); Fstar.zeros();
  
  for(i=0; i < tl; ++i){
    double Fstartemp = 0;
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = adjustedcovariates.row(j);
      Fstartemp += ( 1 - exp(-CumLambda0(i) * exp( arma::as_scalar(adjustedbetas*zj.t())) ) );
    }
    Fstar(i) =  Fstartemp/ntotalobs;
  }
  
  //arma::rowvec nuj(nobs); nuj.zeros();
  for(i=0; i < tl; ++i){
    double nujtemp = 0;
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = adjustedcovariates.row(j);
      nujtemp += exp(-CumLambda0(i) * exp(arma::as_scalar(adjustedbetas*zj.t()))  + arma::as_scalar(adjustedbetas*zj.t()) ) ;
    }
    nuj(i) = nujtemp/ntotalobs;
  }
  
  
  for(i=0; i < tl; ++i){
    arma::rowvec zetajtemp(ncovs); zetajtemp.zeros();
    for(j=0; j < ntotalobs; ++j){
      arma::rowvec zj = adjustedcovariates.row(j);
      zetajtemp += zj * exp(-CumLambda0(i) * exp( arma::as_scalar(adjustedbetas*zj.t())) + arma::as_scalar(adjustedbetas*zj.t()) ) ;
    }
    zetaj.row(i) = zetajtemp/ntotalobs;
  }
  
  //Estimate of F(t|Zpred). Added on 11/03/2021
  arma::rowvec Fpred(tl); Fpred.zeros();
  arma::rowvec Fpredfirst(tl); Fpredfirst.zeros();
  for(i=0; i < tl; ++i){
    Fpred(i) = 1 - exp(-CumLambda0(i)*exp(arma::as_scalar(betas*Zpred.t())));
    Fpredfirst(i) = exp(arma::as_scalar(betas*Zpred.t()) - CumLambda0(i)*exp(arma::as_scalar(betas*Zpred.t())));
  }
  
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);
    W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
    //FOR THE SECOND TERM, DO SIMILAR AS q1(u)
    //second_term_W = qi % dMc;
    //W2 = myRowsums(second_term_W.cols(0,uuu-1));
    
    //q2(u,t):
    q2.zeros();
    for(u=0; u < nobs; ++u){
      if(causes(u)!=0) continue; //We need this when we calculate psi and also makes the algorithm efficient
      double qtemp1=0;
      for(i=0; i < nobs; ++i){
        if(causes(i) > 1 && times(i) < times(u)){
          arma::rowvec zi = covariates.row(i);
          for(ttt=0; ttt < uuu; ++ttt){
            if(times(u) <= times(ttt) && causes(ttt)==1){
              qtemp1 += 1/S0hat(ttt) * exp(arma::as_scalar(betas*zi.t()))* cweight(ttt)/cweight(i) * dlambdat(ttt);
            }
          }
        }
      }
      q2(u) = qtemp1/nc;
    }
    
    //Intergral of q2(u,t) dMc(u)
    W2.zeros();
    for(i=0; i < nobs; ++i){
      double psitemp1=0;
      for(j=0; j < nobs; ++j){
        if(times(i)==times(j) && causes(i)==0){
          psitemp1 += q2(i)/S0hatc(i);
        }
        if(causes(j)==0 && times(i) >= times(j)){
          psitemp1 -= q2(j) * dlambdac(j)/S0hatc(j);
        }
      }
      W2(i) = psitemp1;
    }
    W2final.col(t) = W2;
    
    
    
    dlt = dlambdat.cols(0,uuu-1);
    hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
    
    W3.row(t) = hLambda.t() * (iI*totalnc) * (etapsi);
    //		//W3new = W3.t();
    
    //		Wall = W1  + W2;
    
    //		double test1=0;
    //		double test2=0;
    //		double test3=0;
    //		double ql=nc/totalnc;
    
    //		for(j=1; j <= totalnc; ++j){
    //			clusteridx = find(cluster==j);
    //			allclusteridx = find(allcluster==j);
    //			W3 = hLambda.t() * (iI*totalnc) * (etapsi.cols(allclusteridx));
    //			W3new = W3.t();
    //			test1 += ( arma::accu(Wall.rows(clusteridx)) + accu(W3new) )  * ( arma::accu(Wall.rows(clusteridx)) + accu(W3new) );
    //		}
    //
    //		vLambda(t) = test1/(nc*nc);
    Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
    //Fstart(t) = Fstar(uuu-1);
    //			//test4 = accu(Wall.rows(clusteridx));
    
    
    //Variance of adjusted CIF
    arma::rowvec zj = zetaj.row(t);
    arma::colvec zjc = zj.t();
    hFstar = myRowsums( (mycbind(zjc,uuu) -  nuj(t) * S1byS0hat.cols(0,uuu-1)) % myrbind(dlt,ncovs));
    W3Fstar.row(t) = hFstar.t() * (iI*totalnc) * (etapsi);
    
    W2finalFstar.col(t) = nuj(t)*W2final.col(t);
    W1Fstar.col(t) = nuj(t)*W1.col(t);
    
    // Added on 11/03/2021
    //Variance of F(t|Z0)
    arma::colvec Zpredc = Zpred.t();
    hLambdapred = myRowsums( (mycbind(Zpredc,uuu) - S1byS0hat.cols(0,uuu-1) ) % myrbind(dlt,ncovs));
    W3pred.row(t) = hLambdapred.t() * (iI*totalnc) * (etapsi);
    
  }
  
  
  
  Rcpp::List res(13);
  res[0]=W1;
  res[1]=W2final;
  res[2]=W3;
  res[3]=Lambda;
  res[4]=CumLambda0;
  res[5]=Fstar;
  res[6]=W1Fstar;
  res[7]=W2finalFstar;
  res[8]=W3Fstar;
  res[9]=Fstar;
  //res[0]=0;
  res[10]=Fpred;
  res[11]=W3pred;
  res[12]=Fpredfirst;
  return(res);
  
  
}



// Rcpp::List se_beta_lambda_strata_KM_diff_unstratified( 
//     const arma::rowvec& times,
//     const arma::rowvec& causes,
//     const arma::mat& covariates,
//     const int& nobs, 
//     const int& ntotal,
//     const int& ncovs,
//     const arma::rowvec& cweight,//just a vector when KM estimate is used for censoring
//     const arma::rowvec& betas,
//     const arma::rowvec& cluster,
//     const arma::rowvec& allcluster,
//     const int& nc,
//     const int& totalnc,
//     const arma::mat& iI,
//     const arma::rowvec& idxtlambda,
//     const arma::mat etapsi,
//     const int& ntotalobs,
//     const arma::mat& adjustedcovariates,
//     const arma::rowvec& adjustedbetas){
//   
//   int i,j,t,u,ttt;
//   
//   //double loglikli=0;
//   //arma::colvec score(ncovs);
//   //score.fill(0.0);
//   //arma::mat infor(ncovs,ncovs);
//   //infor.fill(0.0);
//   arma::rowvec S0hat(nobs);S0hat.zeros();
//   arma::mat S1hat(ncovs,nobs);S1hat.zeros();
//   arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
//   //arma::cube S2byS0hat(ncovs,ncovs,nobs);
//   
//   for(i=0; i<nobs; ++i){
//     double s0=0;
//     arma::colvec s1(ncovs);
//     s1.zeros();
//     arma::mat s2(ncovs,ncovs);
//     s2.zeros();
//     double wye=0;
//     
//     //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
//     
//     arma::rowvec zi = covariates.row(i);
//     //loglikli += arma::as_scalar(betas*zi.t());
//     //score += zi.t();
//     
//     for(j=0; j<nobs; ++j){
//       if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
//       arma::rowvec zj = covariates.row(j);
//       if(times(i) <= times(j)){
//         wye = exp(arma::as_scalar(betas*zj.t()));
//       }
//       else if (times(i) > times(j) && causes(j)>1){
//         wye = exp(arma::as_scalar(betas*zj.t())) * cweight(i)/cweight(j);
//       }
//       else continue;
//       s0 += wye;
//       s1 += wye * zj.t();
//       s2 += wye * zj.t() * zj;
//     }	
//     S0hat(i) = s0/nc;
//     S1hat.col(i) = s1/nc;
//     S1byS0hat.col(i) = s1/s0;
//     //S2byS0hat.slice(i) = s2/s0;
//     
//   }
//   
//   //dlambda10(t)
//   arma::rowvec dlambdat(nobs);dlambdat.zeros();
//   for(j=0; j < nobs; ++j){
//     double dlambdatemp=0;
//     if(causes(j)!=1) continue;
//     for(i=0; i < nobs; ++i){
//       if(times(j) == times(i)){
//         dlambdatemp += 1/(S0hat(j)*nc);
//       }
//     }
//     dlambdat(j)=dlambdatemp;
//   }
//   
//   
//   
//   //eta_i: 
//   //	arma::mat etai(ncovs,nobs);
//   //	etai.zeros();
//   
//   //	for(i=0; i < nobs; ++i){
//   //		arma::rowvec zi = covariates.row(i);
//   //		arma::colvec etaifirst(ncovs); etaifirst.zeros();
//   //		for(j=0; j < nobs; ++j){
//   //			if(causes(i)==1 && times(j)==times(i)){
//   //				etaifirst += (zi.t() - S1byS0hat.col(j));
//   //			}
//   //			if(causes(i)>1 && times(j) > times(i)){
//   //				etaifirst -= (zi.t() - S1byS0hat.col(j)) * cweight(j)/cweight(i) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
//   
//   //			}
//   //			else if(times(j) <= times(i)){
//   //				etaifirst -= (zi.t() - S1byS0hat.col(j)) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
//   //			}
//   //		}
//   
//   //		etai.col(i) = etaifirst;
//   //	}
//   
//   //q1(u): 
//   arma::mat q(ncovs,nobs); q.zeros();
//   for(u=0; u < nobs; ++u){
//     if(causes(u)!=0) continue; //We need this when we calculate psi and also makes the algorithm efficient
//     arma::colvec qtemp(ncovs); qtemp.zeros();
//     for(i=0; i < nobs; ++i){
//       if(causes(i) > 1 && times(i) < times(u)){
//         arma::rowvec zi = covariates.row(i);
//         for(t=0; t < nobs; ++t){
//           if(times(u) <= times(t) && causes(t)==1){
//             qtemp += (zi.t() - S1byS0hat.col(t)) * exp(arma::as_scalar(betas*zi.t()))*cweight(t)/cweight(i)*dlambdat(t);
//           }
//         }
//       }
//     }
//     q.col(u) = qtemp/nc;
//     
//   }
//   
//   //S0c(t): 
//   arma::rowvec S0hatc(nobs);S0hatc.zeros();
//   for(j=0; j < nobs; ++j){
//     double S0c=0;
//     for(i=0; i < nobs; ++i){
//       if(times(i) >= times(j)){
//         S0c+=1;
//       }
//     }
//     S0hatc(j)=S0c/nc;
//   }	
//   
//   //dlambdac(t)
//   arma::rowvec dlambdac(nobs);dlambdac.zeros();
//   for(j=0; j < nobs; ++j){
//     double dlambdatempc=0;
//     if(causes(j)==0){
//       for(i=0; i < nobs; ++i){
//         if(times(j) == times(i)){
//           dlambdatempc += 1/(S0hatc(j)*nc);
//         }
//       }
//     }
//     
//     dlambdac(j)=dlambdatempc;
//   }
//   
//   //psi
//   //	arma::mat psi(ncovs,nobs); psi.zeros();
//   //	for(i=0; i < nobs; ++i){
//   //		arma:: colvec psitemp(ncovs); psitemp.zeros();		
//   //		for(j=0; j < nobs; ++j){
//   //			if(times(i)==times(j) && causes(i)==0){
//   //				psitemp += q.col(i)/S0hatc(i);
//   //			}
//   //			if(causes(j)==0 && times(i) >= times(j)){
//   //				psitemp -= q.col(j) * dlambdac(j)/S0hatc(j);
//   //			}
//   //		}
//   //		psi.col(i) = psitemp;
//   //	}
//   //Gathering cluster information to calculate the variance of beta
//   //	arma::uvec clusteridxbeta;
//   //	arma::vec vmiddle; vmiddle.zeros();
//   //	arma::mat vtotal(ncovs,ncovs); vtotal.zeros();
//   //	arma::mat vbeta(ncovs,ncovs); vbeta.zeros();
//   //	for(j=1; j <= nc; ++j){
//   //		clusteridxbeta=find(cluster==j);
//   //		vmiddle = myRowsums(etai.cols(clusteridxbeta) + psi.cols(clusteridxbeta));
//   //		vtotal += vmiddle * vmiddle.t();
//   //	}
//   //	vbeta = iI * vtotal * iI;
//   
//   //Calculating lambda(t) and standard error
//   
//   //wdM
//   arma::mat wdM(nobs,nobs);wdM.zeros();
//   for(i=0; i < nobs; ++i){
//     arma::rowvec zi = covariates.row(i);
//     for(j=0; j < nobs; ++j){
//       double first=0;
//       double second=0;
//       if(causes(i)==1 && times(i)==times(j)){
//         first = 1;
//       }
//       if(times(j) <= times(i)){
//         second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
//       }
//       else if(times(j) > times(i) && causes(i) > 1){
//         second = exp(arma::as_scalar(betas*zi.t())) *cweight(j)/cweight(i)*dlambdat(j);
//       }
//       wdM(i,j) = first - second;
//     }
//   }
//   
//   //dMc
//   arma::mat dMc(nobs,nobs);dMc.zeros();
//   for(i=0; i < nobs; ++i){
//     for(j=0; j < nobs; ++j){
//       double first=0;
//       double second=0;
//       if(causes(i)==0 && times(i)==times(j)){
//         first = 1;
//       }
//       if(times(i) >= times(j) && causes(j)==0){
//         second = dlambdac(j);
//       }
//       dMc(i,j) = first - second;
//     }
//   }	
//   
//   
//   int tl=idxtlambda.n_cols;
//   arma::mat first_term_W = wdM/myrbind(S0hat,nobs);
//   arma::vec hLambda; arma::vec hFstar;
//   arma::rowvec dlt;
//   arma::uvec clusteridx;arma::uvec allclusteridx;
//   arma::rowvec vLambda(tl); vLambda.zeros();
//   arma::rowvec Lambda(tl); Lambda.zeros();
//   arma::rowvec Fstart(tl); Fstart.zeros();
//   //arma::vec W1;arma::vec W2(nobs); arma::rowvec W3; arma::vec Wall; arma::vec W3new;
//   arma::vec W2(nobs);
//   arma::mat W1(nobs,tl); W1.zeros();arma::mat W2final(nobs,tl); W2final.zeros();arma::mat W3(tl,ntotal); W3.zeros();
//   arma::mat W1Fstar(nobs,tl); W1Fstar.zeros(); arma::mat W2finalFstar(nobs,tl); W2finalFstar.zeros();arma::mat W3Fstar(tl,ntotal); W3Fstar.zeros();
//   arma::rowvec q2(nobs);
//   //double test4=0;
//   arma::rowvec nuj(tl); 
//   
//   arma::rowvec CumLambda0(tl); CumLambda0.zeros();
//   for(t=0; t < tl; ++t){
//     int uuu = idxtlambda(t);
//     CumLambda0(t) = arma::accu(dlambdat.cols(0,uuu-1));
//   }
//   //CumLambda0 = arma::cumsum(dlambdat);
//   
//   arma::mat zetaj(tl,ncovs); zetaj.zeros();
//   
//   //Estimate of adjusted CIF
//   arma::rowvec Fstar(tl); Fstar.zeros();
//   
//   for(i=0; i < tl; ++i){
//     double Fstartemp = 0;
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = adjustedcovariates.row(j);
//       Fstartemp += ( 1 - exp(-CumLambda0(i) * exp( arma::as_scalar(adjustedbetas*zj.t())) ) );
//     }
//     Fstar(i) =  Fstartemp/ntotalobs;
//   }
//   
//   //arma::rowvec nuj(nobs); nuj.zeros();
//   for(i=0; i < tl; ++i){
//     double nujtemp = 0;
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = adjustedcovariates.row(j);
//       nujtemp += exp(-CumLambda0(i) * exp(arma::as_scalar(adjustedbetas*zj.t()))  + arma::as_scalar(adjustedbetas*zj.t()) ) ;
//     }
//     nuj(i) = nujtemp/ntotalobs;
//   }
//   
//   
//   for(i=0; i < tl; ++i){
//     arma::rowvec zetajtemp(ncovs); zetajtemp.zeros();
//     for(j=0; j < ntotalobs; ++j){
//       arma::rowvec zj = adjustedcovariates.row(j);
//       zetajtemp += zj * exp(-CumLambda0(i) * exp( arma::as_scalar(adjustedbetas*zj.t())) + arma::as_scalar(adjustedbetas*zj.t()) ) ;
//     }
//     zetaj.row(i) = zetajtemp/ntotalobs;
//   }
//   
//   
//   for(t=0; t < tl; ++t){
//     int uuu = idxtlambda(t);
//     W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
//     //FOR THE SECOND TERM, DO SIMILAR AS q1(u)
//     //second_term_W = qi % dMc;
//     //W2 = myRowsums(second_term_W.cols(0,uuu-1));
//     
//     //q2(u,t):
//     q2.zeros();
//     for(u=0; u < nobs; ++u){
//       if(causes(u)!=0) continue; //We need this when we calculate psi and also makes the algorithm efficient
//       double qtemp1=0;
//       for(i=0; i < nobs; ++i){
//         if(causes(i) > 1 && times(i) < times(u)){
//           arma::rowvec zi = covariates.row(i);
//           for(ttt=0; ttt < uuu; ++ttt){
//             if(times(u) <= times(ttt) && causes(ttt)==1){
//               qtemp1 += 1/S0hat(ttt) * exp(arma::as_scalar(betas*zi.t()))* cweight(ttt)/cweight(i) * dlambdat(ttt);
//             }
//           }
//         }
//       }
//       q2(u) = qtemp1/nc;
//     }
//     
//     //Intergral of q2(u,t) dMc(u)
//     W2.zeros();
//     for(i=0; i < nobs; ++i){
//       double psitemp1=0;
//       for(j=0; j < nobs; ++j){
//         if(times(i)==times(j) && causes(i)==0){
//           psitemp1 += q2(i)/S0hatc(i);
//         }
//         if(causes(j)==0 && times(i) >= times(j)){
//           psitemp1 -= q2(j) * dlambdac(j)/S0hatc(j);
//         }
//       }
//       W2(i) = psitemp1;
//     }
//     W2final.col(t) = W2;
//     
//     
//     
//     dlt = dlambdat.cols(0,uuu-1);
//     hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
//     
//     W3.row(t) = hLambda.t() * (iI*totalnc) * (etapsi);
//     //		//W3new = W3.t();
//     
//     //		Wall = W1  + W2;
//     
//     //		double test1=0;
//     //		double test2=0;
//     //		double test3=0;
//     //		double ql=nc/totalnc;
//     
//     //		for(j=1; j <= totalnc; ++j){
//     //			clusteridx = find(cluster==j);
//     //			allclusteridx = find(allcluster==j);
//     //			W3 = hLambda.t() * (iI*totalnc) * (etapsi.cols(allclusteridx));
//     //			W3new = W3.t();
//     //			test1 += ( arma::accu(Wall.rows(clusteridx)) + accu(W3new) )  * ( arma::accu(Wall.rows(clusteridx)) + accu(W3new) );
//     //		}
//     //
//     //		vLambda(t) = test1/(nc*nc);
//     Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
//     //Fstart(t) = Fstar(uuu-1);
//     //			//test4 = accu(Wall.rows(clusteridx));
//     
//     
//     //Variance of adjusted CIF
//     arma::rowvec zj = zetaj.row(t);
//     arma::colvec zjc = zj.t();
//     hFstar = myRowsums( (mycbind(zjc,uuu) -  nuj(t) * S1byS0hat.cols(0,uuu-1)) % myrbind(dlt,ncovs));
//     W3Fstar.row(t) = hFstar.t() * (iI*totalnc) * (etapsi);
//     
//     W2finalFstar.col(t) = nuj(t)*W2final.col(t);
//     W1Fstar.col(t) = nuj(t)*W1.col(t);
//     
//   }
//   
//   
//   
//   Rcpp::List res(10);
//   res[0]=W1;
//   res[1]=W2final;
//   res[2]=W3;
//   res[3]=Lambda;
//   res[4]=CumLambda0;
//   res[5]=Fstar;
//   res[6]=W1Fstar;
//   res[7]=W2finalFstar;
//   res[8]=W3Fstar;
//   res[9]=Fstar;
//   //res[0]=0;
//   return(res);
//   
//   
// }

// [[Rcpp::export]]

Rcpp::List se_beta_strata_COX( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const arma::mat& cencovariates, //censoring dependent covariates
    const int& nobs, 
    const int& ncovs,
    const arma::mat& cweight,//matrix when cox model is used for censoring
    const arma::rowvec& betas,//Estimates for cause 1
    const arma::rowvec& gammas,//Estimates for censoring
    const int& nc,
    arma::colvec& expg,
    const arma::mat& iIc){
  
  int i,j,k;
  
  //double loglikli=0;
  //arma::colvec score(ncovs);
  //score.fill(0.0);
  //arma::mat infor(ncovs,ncovs);
  //infor.fill(0.0);
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  //arma::cube S2byS0hat(ncovs,ncovs,nobs);
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.zeros();
    arma::mat s2(ncovs,ncovs);
    s2.zeros();
    double wye=0;
    
    //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
    
    arma::rowvec zi = covariates.row(i);
    //loglikli += arma::as_scalar(betas*zi.t());
    //score += zi.t();
    
    for(j=0; j<nobs; ++j){
      if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
      arma::rowvec zj = covariates.row(j);
      if(times(i) <= times(j)){
        wye = exp(arma::as_scalar(betas*zj.t()));
      }
      else if (times(i) > times(j) && causes(j)>1){
        wye = exp(arma::as_scalar(betas*zj.t())) * cweight(j,i)/cweight(j,j);
      }
      else continue;
      s0 += wye;
      s1 += wye * zj.t();
      s2 += wye * zj.t() * zj;
    }	
    S0hat(i) = s0/nc;
    S1hat.col(i) = s1/nc;
    S1byS0hat.col(i) = s1/s0;
    //S2byS0hat.slice(i) = s2/s0;
    
  }
  
  //dlambda10(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(causes(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += 1/(S0hat(j)*nc);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  
  
  //eta_i: 
  arma::mat etai(ncovs,nobs);
  etai.zeros();
  
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    arma::colvec etaifirst(ncovs); etaifirst.zeros();
    for(j=0; j < nobs; ++j){
      if(causes(i)==1 && times(j)==times(i)){
        etaifirst += (zi.t() - S1byS0hat.col(j));
      }
      if(causes(i)>1 && times(j) > times(i)){
        etaifirst -= (zi.t() - S1byS0hat.col(j)) * cweight(i,j)/cweight(i,i) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
        
      }
      else if(times(j) <= times(i)){
        etaifirst -= (zi.t() - S1byS0hat.col(j)) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
    }
    
    etai.col(i) = etaifirst;
  }
  
  //For Censoring(S0C(t),S1C(t),S1C(t)/S0C(t))
  const int ncovsC=cencovariates.n_cols;
  arma::rowvec S0hatC(nobs);S0hatC.zeros();
  arma::mat S1hatC(ncovsC,nobs);S1hatC.zeros();
  arma::mat S1byS0hatC(ncovsC,nobs);S1byS0hatC.zeros();
  //arma::cube S2byS0hatC(ncovsC,ncovsC,nobs);
  
  for(i=0; i<nobs; ++i){ 
    double s0C=0;
    arma::colvec s1C(ncovsC);
    s1C.zeros();
    arma::mat s2C(ncovsC,ncovsC);
    s2C.zeros();
    //double wye=0;
    
    
    
    for(j=0; j<nobs; ++j){
      if(times(j) >= times(i)){
        arma::rowvec zjC = cencovariates.row(j);
        s0C += exp(arma::as_scalar(gammas*zjC.t()));
        s1C += zjC.t() * exp(arma::as_scalar(gammas*zjC.t()));
        s2C += zjC.t() * zjC * exp(arma::as_scalar(gammas*zjC.t()));				
      }
    }	
    S0hatC(i) = s0C/nc;
    S1hatC.col(i) = s1C/nc;
    S1byS0hatC.col(i) = s1C/s0C;
    //S2byS0hatC.slice(i) = s2C/s0C;
    
  }
  //	//Calculating iIc.
  //	arma::mat ii(ncovsC,ncovsC); ii.zeros();
  //	for(i = 0; i < nobs; ++i){
  //		if(causes(i)==0){
  //			for(j = 0; j < nobs; ++j){
  //				if(times(i)==times(j) && causes(j)==0){
  //					arma::colvec r=S1byS0hatC.col(j);
  //					ii += S2byS0hatC.slice(j) - mymult(r);
  //				}
  //			}
  //		}
  //	}
  
  //	arma::mat iIc(ncovsC,ncovsC);
  //	iIc = arma::inv(ii);
  //For Censoring(dlambdac(t))
  arma::rowvec dlambdac(nobs);dlambdac.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatempc=0;
    if(causes(j)!=0) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatempc += 1/(S0hatC(j)*nc);
      }
    }
    
    dlambdac(j)=dlambdatempc;
  }
  
  arma::cube h=hhat(gammas,S1byS0hatC,cencovariates,dlambdac);
  
  //q_i(u) and psi_i
  //Calculating wdM and dMc as a matrix.(It is efficient to do it this way for calculating q_i(u)).
  arma::mat wdM(nobs,nobs);wdM.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==1 && times(i)==times(j)){
        first = 1;
      }
      if(times(j) <= times(i)){
        second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
      else if(times(j) > times(i) && causes(i) > 1){
        second = exp(arma::as_scalar(betas*zi.t())) *cweight(i,j)/cweight(i,i)*dlambdat(j);
      }
      wdM(i,j) = first - second;
    }
  }
  
  //dMc
  arma::mat dMc(nobs,nobs);dMc.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zci = cencovariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==0 && times(i)==times(j)){
        first = 1;
      }
      if(times(i) >= times(j) && causes(j)==0){
        second = exp(arma::as_scalar(gammas*zci.t())) * dlambdac(j);
      }
      dMc(i,j) = first - second;
    }
  }
  
  //zc_min_S1byS0hatC
  //arma::colvec expg=exp(cencovariates * gammas.t());
  arma::cube zc_min_S1bS0hatc(nobs,nobs,ncovsC);
  for(j=0; j < ncovsC;++j){
    arma::colvec zc=cencovariates.col(j);
    arma::rowvec ratio = S1byS0hatC.row(j);
    zc_min_S1bS0hatc.slice(j) = mycbind(zc,nobs) - myrbind(ratio,nobs);
  }
  
  arma::mat qi21temp(nobs,nobs); qi21temp.zeros();
  arma::rowvec qi21(nobs); qi21.zeros();
  //Main loop to calculate psi
  arma::mat z_min_S1bS0hat_wdM(nobs,nobs);
  arma::rowvec qi11(ncovsC); qi11.zeros();
  arma::mat test(nobs,nobs); test.zeros();
  arma::mat precomp(1,ncovsC); precomp.zeros();
  double qi2;
  arma::vec denom;
  //arma::vec locu;
  //int locu;
  arma::colvec s0C;
  arma::mat tempzc;
  arma::mat q1_all_i(1,ncovsC);
  double qi2final;
  arma::mat qi1(1,nobs); qi1.zeros();
  arma::mat qi(nobs,nobs);
  arma::mat psi(ncovs,nobs);
  
  for(j=0; j < ncovs; ++ j){
    arma::colvec z=covariates.col(j);
    arma::rowvec rat = S1byS0hat.row(j);
    z_min_S1bS0hat_wdM = (mycbind(z,nobs) - myrbind(rat,nobs)) % wdM;
    
    for(k=0; k < ncovsC; ++k){
      test = h(arma::span(k),arma::span::all,arma::span::all);
      qi11(k) = accu(test % z_min_S1bS0hat_wdM);//Takes care of summation and integral	
    }
    
    
    qi21temp = mycbind(expg,nobs) % z_min_S1bS0hat_wdM;
    qi21 = myColsums(qi21temp);
    
    //precomp = qi11*iIc;
    for(i=0; i < nobs; ++i){//This loop is for u in the formula.
      double u = times(i);
      arma::uvec idx = find(u <= times);
      qi2 = accu(qi21.elem(idx));
      arma::uvec locu = find(u==times);
      denom = S0hatC.elem(locu);
      double finaldenom = denom(0);//This will take care if two points are the same.
      qi2final = qi2/finaldenom;
      //tempzc = zc_min_S1bS0hatc(span::all,span(i),span::all);//First span:all takes care of i in the formula. span(i) takes care of u.
      //qi1 = precomp*tempzc.t();
      qi.col(i) = -1*(qi1.t() + qi2final)/nc;
    }
    arma::vec rr = myRowsums(qi % dMc);
    psi.row(j) = rr.t();
    
  }
  
  
  
  
  Rcpp::List res(4);
  res[0]=etai;
  res[1]=psi;
  //res[2]=dlambdat;
  res[2]=zc_min_S1bS0hatc;
  res[3]=dMc;
  //res[3]=qi;
  
  return(res);
  
  
}

// [[Rcpp::export]]

Rcpp::List se_beta_strata_COX1( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const arma::mat& cencovariates, //censoring dependent covariates
    const int& nobs, 
    const int& nobs1,
    const int& ncovs,
    const arma::mat& cweight,//matrix when cox model is used for censoring
    const arma::rowvec& betas,//Estimates for cause 1
    const arma::rowvec& gammas,//Estimates for censoring
    const int& nc,
    arma::colvec& expg,
    const arma::mat& iIc,
    const arma::cube& zcminusecold,
    const int& totalnc,
    const arma::mat& dMC1){
  
  int i,j,k;
  
  //double loglikli=0;
  //arma::colvec score(ncovs);
  //score.fill(0.0);
  //arma::mat infor(ncovs,ncovs);
  //infor.fill(0.0);
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  //arma::cube S2byS0hat(ncovs,ncovs,nobs);
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.zeros();
    arma::mat s2(ncovs,ncovs);
    s2.zeros();
    double wye=0;
    
    //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
    
    arma::rowvec zi = covariates.row(i);
    //loglikli += arma::as_scalar(betas*zi.t());
    //score += zi.t();
    
    for(j=0; j<nobs; ++j){
      if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
      arma::rowvec zj = covariates.row(j);
      if(times(i) <= times(j)){
        wye = exp(arma::as_scalar(betas*zj.t()));
      }
      else if (times(i) > times(j) && causes(j)>1){
        wye = exp(arma::as_scalar(betas*zj.t())) * cweight(j,i)/cweight(j,j);
      }
      else continue;
      s0 += wye;
      s1 += wye * zj.t();
      s2 += wye * zj.t() * zj;
    }	
    S0hat(i) = s0/nc;
    S1hat.col(i) = s1/nc;
    S1byS0hat.col(i) = s1/s0;
    //S2byS0hat.slice(i) = s2/s0;
    
  }
  
  //dlambda10(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(causes(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += 1/(S0hat(j)*nc);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  
  
  //eta_i: 
  arma::mat etai(ncovs,nobs);
  etai.zeros();
  
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    arma::colvec etaifirst(ncovs); etaifirst.zeros();
    for(j=0; j < nobs; ++j){
      if(causes(i)==1 && times(j)==times(i)){
        etaifirst += (zi.t() - S1byS0hat.col(j));
      }
      if(causes(i)>1 && times(j) > times(i)){
        etaifirst -= (zi.t() - S1byS0hat.col(j)) * cweight(i,j)/cweight(i,i) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
        
      }
      else if(times(j) <= times(i)){
        etaifirst -= (zi.t() - S1byS0hat.col(j)) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
    }
    
    etai.col(i) = etaifirst;
  }
  
  //For Censoring(S0C(t),S1C(t),S1C(t)/S0C(t))
  const int ncovsC=cencovariates.n_cols;
  arma::rowvec S0hatC(nobs);S0hatC.zeros();
  arma::mat S1hatC(ncovsC,nobs);S1hatC.zeros();
  arma::mat S1byS0hatC(ncovsC,nobs);S1byS0hatC.zeros();
  //arma::cube S2byS0hatC(ncovsC,ncovsC,nobs);
  
  for(i=0; i<nobs; ++i){ 
    double s0C=0;
    arma::colvec s1C(ncovsC);
    s1C.zeros();
    arma::mat s2C(ncovsC,ncovsC);
    s2C.zeros();
    //double wye=0;
    
    
    
    for(j=0; j<nobs; ++j){
      if(times(j) >= times(i)){
        arma::rowvec zjC = cencovariates.row(j);
        s0C += exp(arma::as_scalar(gammas*zjC.t()));
        s1C += zjC.t() * exp(arma::as_scalar(gammas*zjC.t()));
        s2C += zjC.t() * zjC * exp(arma::as_scalar(gammas*zjC.t()));				
      }
    }	
    S0hatC(i) = s0C/nc;
    S1hatC.col(i) = s1C/nc;
    S1byS0hatC.col(i) = s1C/s0C;
    //S2byS0hatC.slice(i) = s2C/s0C;
    
  }
  //	//Calculating iIc.
  //	arma::mat ii(ncovsC,ncovsC); ii.zeros();
  //	for(i = 0; i < nobs; ++i){
  //		if(causes(i)==0){
  //			for(j = 0; j < nobs; ++j){
  //				if(times(i)==times(j) && causes(j)==0){
  //					arma::colvec r=S1byS0hatC.col(j);
  //					ii += S2byS0hatC.slice(j) - mymult(r);
  //				}
  //			}
  //		}
  //	}
  
  //	arma::mat iIc(ncovsC,ncovsC);
  //	iIc = arma::inv(ii);
  //For Censoring(dlambdac(t))
  arma::rowvec dlambdac(nobs);dlambdac.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatempc=0;
    if(causes(j)!=0) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatempc += 1/(S0hatC(j)*nc);
      }
    }
    
    dlambdac(j)=dlambdatempc;
  }
  
  arma::cube h=hhat(gammas,S1byS0hatC,cencovariates,dlambdac);
  
  //q_i(u) and psi_i
  //Calculating wdM and dMc as a matrix.(It is efficient to do it this way for calculating q_i(u)).
  arma::mat wdM(nobs,nobs);wdM.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==1 && times(i)==times(j)){
        first = 1;
      }
      if(times(j) <= times(i)){
        second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
      else if(times(j) > times(i) && causes(i) > 1){
        second = exp(arma::as_scalar(betas*zi.t())) *cweight(i,j)/cweight(i,i)*dlambdat(j);
      }
      wdM(i,j) = first - second;
    }
  }
  
  //dMc
  arma::mat dMc(nobs,nobs);dMc.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zci = cencovariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==0 && times(i)==times(j)){
        first = 1;
      }
      if(times(i) >= times(j) && causes(j)==0){
        second = exp(arma::as_scalar(gammas*zci.t())) * dlambdac(j);
      }
      dMc(i,j) = first - second;
    }
  }
  
  //zc_min_S1byS0hatC
  
  arma::cube zc_min_S1bS0hatc(nobs,nobs,ncovsC);
  for(j=0; j < ncovsC;++j){
    arma::colvec zc=cencovariates.col(j);
    arma::rowvec ratio = S1byS0hatC.row(j);
    zc_min_S1bS0hatc.slice(j) = mycbind(zc,nobs) - myrbind(ratio,nobs);
  }
  
  arma::mat qi21temp(nobs,nobs); qi21temp.zeros();
  arma::rowvec qi21(nobs); qi21.zeros();
  //Main loop to calculate psi
  arma::mat z_min_S1bS0hat_wdM(nobs,nobs);
  arma::rowvec qi11(ncovsC); qi11.zeros();
  arma::mat test(nobs,nobs); test.zeros();
  arma::mat precomp(1,ncovsC); precomp.zeros();
  //double qi2;
  arma::vec denom;
  //arma::vec locu;
  //int locu;
  arma::colvec s0C;
  arma::mat tempzc;
  arma::mat q1_all_i(1,ncovsC);
  //double qi2final;
  arma::mat qi1(1,nobs1);
  arma::mat qi(nobs1,nobs1);
  arma::mat psi(ncovs,nobs1);
  
  
  for(j=0; j < ncovs; ++ j){
    arma::colvec z=covariates.col(j);
    arma::rowvec rat = S1byS0hat.row(j);
    z_min_S1bS0hat_wdM = (mycbind(z,nobs) - myrbind(rat,nobs)) % wdM;
    
    for(k=0; k < ncovsC; ++k){
      test = h(arma::span(k),arma::span::all,arma::span::all);
      qi11(k) = accu(test % z_min_S1bS0hat_wdM);//Takes care of summation and integral	
    }
    
    
    qi21temp = mycbind(expg,nobs) % z_min_S1bS0hat_wdM;
    qi21 = myColsums(qi21temp);
    //arma::rowvec qi211 = myColsums(qi21);
    //arma::mat qi(nobs,nobs); qi.zeros();
    
    precomp = qi11*iIc*totalnc;
    for(i=0; i < nobs1; ++i){//This loop is for u in the formula.
      //double u = times1(i);
      //arma::uvec idx = find(u <= times1);
      //qi2 = accu(qi21.elem(idx));
      //arma::uvec locu = find(u==times1);
      //denom = S0hatC.elem(locu);
      //double finaldenom = denom(0);//This will take care if two points are the same.
      //qi2final = qi2/finaldenom;
      //tempzc = zc_min_S1bS0hatc(span::all,span(i),span::all);
      tempzc = zcminusecold(arma::span::all,arma::span(i),arma::span::all);
      qi1 = precomp*tempzc.t();
      //qi.col(i) = -1*(qi1.t() + qi2final)/nc;
      qi.col(i) = -1*(qi1.t())/totalnc;
    }
    arma::vec rr = myRowsums(qi % dMC1);
    psi.row(j) = rr.t();
    
  }
  
  
  
  
  Rcpp::List res(1);
  res[0]=psi;
  //res[0]=qi;
  
  
  
  return(res);
  
  
}


// [[Rcpp::export]]
Rcpp::List se_beta_strata_KM( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const int& nobs, 
    const int& ncovs,
    const arma::rowvec& cweight,//just a vector when KM estimate is used for censoring
    const arma::rowvec& betas,
    const int& nc){
  
  int i,j,t,u;
  
  //double loglikli=0;
  //arma::colvec score(ncovs);
  //score.fill(0.0);
  //arma::mat infor(ncovs,ncovs);
  //infor.fill(0.0);
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  //arma::cube S2byS0hat(ncovs,ncovs,nobs);
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.zeros();
    arma::mat s2(ncovs,ncovs);
    s2.zeros();
    double wye=0;
    
    //if(causes(i)!=1) continue;//if cause is not 1 then skip to next i
    
    arma::rowvec zi = covariates.row(i);
    //loglikli += arma::as_scalar(betas*zi.t());
    //score += zi.t();
    
    for(j=0; j<nobs; ++j){
      if((times(i) > times(j)) && causes(j) <= 1) continue; //causes other than 1 and 0 implies  w*Y !=0.
      arma::rowvec zj = covariates.row(j);
      if(times(i) <= times(j)){
        wye = exp(arma::as_scalar(betas*zj.t()));
      }
      else if (times(i) > times(j) && causes(j)>1){
        wye = exp(arma::as_scalar(betas*zj.t())) * cweight(i)/cweight(j);
      }
      else continue;
      s0 += wye;
      s1 += wye * zj.t();
      s2 += wye * zj.t() * zj;
    }	
    S0hat(i) = s0/nc;
    S1hat.col(i) = s1/nc;
    S1byS0hat.col(i) = s1/s0;
    //S2byS0hat.slice(i) = s2/s0;
    
  }
  
  //dlambda10(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(causes(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += 1/(S0hat(j)*nc);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  
  
  //eta_i: 
  arma::mat etai(ncovs,nobs);
  etai.zeros();
  
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    arma::colvec etaifirst(ncovs); etaifirst.zeros();
    for(j=0; j < nobs; ++j){
      if(causes(i)==1 && times(j)==times(i)){
        etaifirst += (zi.t() - S1byS0hat.col(j));
      }
      if(causes(i)>1 && times(j) > times(i)){
        etaifirst -= (zi.t() - S1byS0hat.col(j)) * cweight(j)/cweight(i) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
        
      }
      else if(times(j) <= times(i)){
        etaifirst -= (zi.t() - S1byS0hat.col(j)) * exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
    }
    
    etai.col(i) = etaifirst;
  }
  
  //q1(u): 
  arma::mat q(ncovs,nobs); q.zeros();
  for(u=0; u < nobs; ++u){
    if(causes(u)!=0) continue; //We need this when we calculate psi and also makes the algorithm efficient
    arma::colvec qtemp(ncovs); qtemp.zeros();
    for(i=0; i < nobs; ++i){
      if(causes(i) > 1 && times(i) < times(u)){
        arma::rowvec zi = covariates.row(i);
        for(t=0; t < nobs; ++t){
          if(times(u) <= times(t) && causes(t)==1){
            qtemp += (zi.t() - S1byS0hat.col(t)) * exp(arma::as_scalar(betas*zi.t()))*cweight(t)/cweight(i)*dlambdat(t);
          }
        }
      }
    }
    q.col(u) = qtemp/nc;
    
  }
  
  //S0c(t): 
  arma::rowvec S0hatc(nobs);S0hatc.zeros();
  for(j=0; j < nobs; ++j){
    double S0c=0;
    for(i=0; i < nobs; ++i){
      if(times(i) >= times(j)){
        S0c+=1;
      }
    }
    S0hatc(j)=S0c/nc;
  }	
  
  //dlambdac(t)
  arma::rowvec dlambdac(nobs);dlambdac.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatempc=0;
    if(causes(j)==0){
      for(i=0; i < nobs; ++i){
        if(times(j) == times(i)){
          dlambdatempc += 1/(S0hatc(j)*nc);
        }
      }
    }

      dlambdac(j)=dlambdatempc;
  }
  
  //psi
  arma::mat psi(ncovs,nobs); psi.zeros();
  for(i=0; i < nobs; ++i){
    arma:: colvec psitemp(ncovs); psitemp.zeros();		
    for(j=0; j < nobs; ++j){
      if(times(i)==times(j) && causes(i)==0){
        psitemp += q.col(i)/S0hatc(i);
      }
      if(causes(j)==0 && times(i) >= times(j)){
        psitemp -= q.col(j) * dlambdac(j)/S0hatc(j);
      }
    }
    psi.col(i) = psitemp;
  }
  //Gathering cluster information to calculate the variance of beta
  //	arma::uvec clusteridxbeta;
  //	arma::vec vmiddle; vmiddle.zeros();
  //	arma::mat vtotal(ncovs,ncovs); vtotal.zeros();
  //	arma::mat vbeta(ncovs,ncovs); vbeta.zeros();
  //	for(j=1; j <= nc; ++j){
  //		clusteridxbeta=find(cluster==j);
  //		vmiddle = myRowsums(etai.cols(clusteridxbeta) + psi.cols(clusteridxbeta));
  //		vtotal += vmiddle * vmiddle.t();
  //	}
  //	vbeta = iI * vtotal * iI;
  
  
  
  Rcpp::List res(3);
  res[0]=etai;
  res[1]=psi;
  res[2]=dlambdat;
  return(res);
  
  
}

// [[Rcpp::export]]
Rcpp::List se_beta_strata_COX_survival( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const int& nobs, 
    const int& ncovs,
    const arma::rowvec& betas,
    const int& nc,
    const arma::mat& iI){
  
  int i,j;
  
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.zeros();
    arma::mat s2(ncovs,ncovs);
    s2.zeros();
    double wye=0;
    
    arma::rowvec zi = covariates.row(i);
    
    for(j=0; j<nobs; ++j){
      if((times(j) >= times(i))){ //Risk set
        arma::rowvec zj = covariates.row(j);
        wye = exp(arma::as_scalar(betas*zj.t()));
        s0 += wye;
        s1 += wye * zj.t();
        s2 += wye * zj.t() * zj;
      }
    }	
    S0hat(i) = s0/nc;
    S1hat.col(i) = s1/nc;
    S1byS0hat.col(i) = s1/s0;
    
  }
  
  //dlambda10(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(causes(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += 1/(S0hat(j)*nc);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  
  
  //eta_i: 
  arma::mat etai(ncovs,nobs);
  etai.zeros();
  
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    arma::colvec etaifirst(ncovs); etaifirst.zeros();
    for(j=0; j < nobs; ++j){
      if(causes(i)==1 && times(j)==times(i)){
        etaifirst += (zi.t() - S1byS0hat.col(j));
      }
      if(times(i) >= times(j) ){
        etaifirst -= (zi.t() - S1byS0hat.col(j)) *  exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
        
      }
    }
    
    etai.col(i) = etaifirst;
  }
  
  // //dM
  // arma::mat dM(nobs,nobs);dM.zeros();
  // for(i=0; i < nobs; ++i){
  //   arma::rowvec zi = covariates.row(i);
  //   for(j=0; j < nobs; ++j){
  //     double first=0;
  //     double second=0;
  //     if(causes(i)==1 && times(i)==times(j)){
  //       first = 1;
  //     }
  //     if(times(j) <= times(i)){
  //       second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
  //     }
  //     dM(i,j) = first - second;
  //   }
  // }
  // 
  // int tl=idxtlambda.n_cols;
  // arma::mat first_term_W = dM/myrbind(S0hat,nobs);
  // arma::mat W1(nobs,tl); W1.zeros();
  // arma::mat W3(tl,nobs); W3.zeros();
  // arma::vec hLambda;
  // arma::rowvec dlt;
  // arma:rowvec Lambda(tl); Lambda.zeros();
  // 
  // for(t=0; t < tl; ++t){
  //   int uuu = idxtlambda(t);		
  //   W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
  // 
  //   
  //   dlt = dlambdat.cols(0,uuu-1);
  //   hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
  //   
  //   W3.row(t) = hLambda.t() * (iI*nctotal) * (etai);
  // 
  //   Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
  // }
  
  Rcpp::List res(2);
  res[0]=etai;
  res[1]=dlambdat;
  // res[2]=W1;
  // res[3]=W3;
  // res[4]=Lambda;
  return(res);
  
  
}


// [[Rcpp::export]]
Rcpp::List se_lambda_strata_COX( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const int& nobs, 
    const int& ncovs,
    const arma::rowvec& betas,
    const int& nc,
    const arma::mat& iI,
    const arma::rowvec& idxtlambda,
    const int& nctotal,//Total number of observations from all stratum. If unstratified then nobs=nctotal
    const arma::mat& etai,
    const int& totalclusters){
  
  int i,j,t;
  
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.zeros();
    arma::mat s2(ncovs,ncovs);
    s2.zeros();
    double wye=0;
    
    arma::rowvec zi = covariates.row(i);
    
    for(j=0; j<nobs; ++j){
      if((times(j) >= times(i))){ //Risk set
        arma::rowvec zj = covariates.row(j);
        wye = exp(arma::as_scalar(betas*zj.t()));
        s0 += wye;
        s1 += wye * zj.t();
        s2 += wye * zj.t() * zj;
      }
    }	
    S0hat(i) = s0/nc;
    S1hat.col(i) = s1/nc;
    S1byS0hat.col(i) = s1/s0;
    
  }
  
  //dlambda10(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(causes(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += 1/(S0hat(j)*nc);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  
  
  // //eta_i: 
  // arma::mat etai(ncovs,nobs);
  // etai.zeros();
  // 
  // for(i=0; i < nobs; ++i){
  //   arma::rowvec zi = covariates.row(i);
  //   arma::colvec etaifirst(ncovs); etaifirst.zeros();
  //   for(j=0; j < nobs; ++j){
  //     if(causes(i)==1 && times(j)==times(i)){
  //       etaifirst += (zi.t() - S1byS0hat.col(j));
  //     }
  //     if(times(i) >= times(j) ){
  //       etaifirst -= (zi.t() - S1byS0hat.col(j)) *  exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
  //       
  //     }
  //   }
  //   
  //   etai.col(i) = etaifirst;
  // }
  
  //dM
  arma::mat dM(nobs,nobs);dM.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==1 && times(i)==times(j)){
        first = 1;
      }
      if(times(j) <= times(i)){
        second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
      dM(i,j) = first - second;
    }
  }
  
  int tl=idxtlambda.n_cols;
  arma::mat first_term_W = dM/myrbind(S0hat,nobs);
  arma::mat W1(nobs,tl); W1.zeros();
  arma::mat W3(tl,nctotal); W3.zeros();
  //arma::mat W3Sstar(tl,nctotal); W3Sstar.zeros();	arma::mat W1Sstar(nobs,tl); W1Sstar.zeros();
  arma::vec hLambda;
  //arma::vec hSstar;
  arma::rowvec dlt;
  arma::rowvec Lambda(tl); Lambda.zeros();
  //arma::rowvec Sstart(tl); Sstart.zeros();
  //arma::rowvec nuj(nobs); 
  
  //arma::rowvec CumLambda0(nobs); CumLambda0.zeros();
  // CumLambda0 = arma::cumsum(dlambdat);
  //arma::mat zetaj(nobs,ncovs); zetaj.zeros();
  
  //Estimate of adjusted survival
  //arma::rowvec Sstar(nobs); Sstar.zeros();
  
  // for(i=0; i < nobs; ++i){
  //   double Sstartemp = 0;
  //   for(j=0; j < nctotal; ++j){
  //     arma::rowvec zj = covariates.row(j);
  //     Sstartemp +=  exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) ) ;
  //   }
  //   Sstar(i) =  Sstartemp/nctotal;
  // }
  
  //arma::rowvec nuj(nobs); nuj.zeros();
  // for(i=0; i < nobs; ++i){
  //   double nujtemp = 0;
  //   for(j=0; j < nctotal; ++j){
  //     arma::rowvec zj = covariates.row(j);
  //     nujtemp += exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) + arma::as_scalar(betas*zj.t()) ) ;
  //   }
  //   nuj(i) = nujtemp/nctotal;
  // }
  
  
  // for(i=0; i < nobs; ++i){
  //   arma::rowvec zetajtemp(ncovs); zetajtemp.zeros();
  //   for(j=0; j < nctotal; ++j){
  //     arma::rowvec zj = covariates.row(j);
  //     zetajtemp += zj * exp(-CumLambda0(i) * exp(arma::as_scalar(betas*zj.t())) + arma::as_scalar(betas*zj.t()) ) ;
  //   }
  //   zetaj.row(i) = zetajtemp/nctotal;
  // }
  
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);		
    W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
    dlt = dlambdat.cols(0,uuu-1);
    hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
    
    W3.row(t) = hLambda.t() * (iI*totalclusters) * (etai);
    Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
    
    //Sstart(t) = Sstar(uuu-1);
    
    //Variance of adjusted CIF
    // arma::rowvec zj = zetaj.row(uuu-1);
    // arma::colvec zjc = zj.t();
    // hSstar = myRowsums( (mycbind(zjc,uuu) -  nuj(uuu-1) * S1byS0hat.cols(0,uuu-1)) % myrbind(dlt,ncovs));
    // W3Sstar.row(t) = hSstar.t() * (iI*totalclusters) * (etai);
    
    // W1Sstar.col(t) = nuj(uuu-1)*W1.col(t);
  }
  
  Rcpp::List res(5);
  res[0]=etai;
  res[1]=dlambdat;
  res[2]=W1;
  res[3]=W3;
  res[4]=Lambda;
  // res[5]=W1Sstar;
  // res[6]=W3Sstar;
  // res[7]=Sstart;
  // res[8]=Sstar;
  return(res);
  
  
}


// [[Rcpp::export]]
Rcpp::List se_adjusted_strata_COX( 
    const arma::rowvec& times,
    const arma::rowvec& causes,
    const arma::mat& covariates,
    const int& nobs, 
    const int& ncovs,
    const arma::rowvec& betas,
    const int& nc,
    const arma::mat& iI,
    const arma::rowvec& idxtlambda,
    const int& nctotal,//Total number of observations from all stratum. If unstratified then nobs=nctotal
    const arma::mat& etai,
    const int& totalclusters,
    const arma::rowvec& newlambda,
    const arma::mat& allcovariates,
    const arma::rowvec& betasadjusted,
    const arma::rowvec Zpred){
  
  int i,j,t;
  
  int tl=idxtlambda.n_cols;
  arma::rowvec S0hat(nobs);S0hat.zeros();
  arma::mat S1hat(ncovs,nobs);S1hat.zeros();
  arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.zeros();
    arma::mat s2(ncovs,ncovs);
    s2.zeros();
    double wye=0;
    
    arma::rowvec zi = covariates.row(i);
    
    for(j=0; j<nobs; ++j){
      if((times(j) >= times(i))){ //Risk set
        arma::rowvec zj = covariates.row(j);
        wye = exp(arma::as_scalar(betas*zj.t()));
        s0 += wye;
        s1 += wye * zj.t();
        s2 += wye * zj.t() * zj;
      }
    }	
    S0hat(i) = s0/nc;
    S1hat.col(i) = s1/nc;
    S1byS0hat.col(i) = s1/s0;
    
  }
  
  //dlambda10(t)
  arma::rowvec dlambdat(nobs);dlambdat.zeros();
  for(j=0; j < nobs; ++j){
    double dlambdatemp=0;
    if(causes(j)!=1) continue;
    for(i=0; i < nobs; ++i){
      if(times(j) == times(i)){
        dlambdatemp += 1/(S0hat(j)*nc);
      }
    }
    dlambdat(j)=dlambdatemp;
  }
  
  
  //dM
  arma::mat dM(nobs,nobs);dM.zeros();
  for(i=0; i < nobs; ++i){
    arma::rowvec zi = covariates.row(i);
    for(j=0; j < nobs; ++j){
      double first=0;
      double second=0;
      if(causes(i)==1 && times(i)==times(j)){
        first = 1;
      }
      if(times(j) <= times(i)){
        second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
      }
      dM(i,j) = first - second;
    }
  }
  
  
  arma::mat first_term_W = dM/myrbind(S0hat,nobs);
  arma::mat W1(nobs,tl); W1.zeros();
  arma::mat W3(tl,nctotal); W3.zeros();
  arma::mat W3Sstar(tl,nctotal); W3Sstar.zeros();	arma::mat W1Sstar(nobs,tl); W1Sstar.zeros();
  arma::vec hLambda;
  arma::vec hSstar;
  arma::rowvec dlt;
  arma::rowvec Lambda(tl); Lambda.zeros();
  arma::rowvec Sstart(tl); Sstart.zeros();
  arma::rowvec nuj(tl);
  
  // Added on 11/03/2021
  arma::vec hLambdapred;
  arma::mat W3pred(tl,nctotal); W3pred.zeros();
  
  arma::rowvec CumLambda0(nobs); CumLambda0.zeros();
  CumLambda0 = arma::cumsum(dlambdat);
  arma::mat zetaj(tl,ncovs); zetaj.zeros();
  
  //Estimate of adjusted survival
  arma::rowvec Sstar(tl); Sstar.zeros();
  
  for(i=0; i < tl; ++i){
    double Sstartemp = 0;
    for(j=0; j < nctotal; ++j){
      arma::rowvec zj = allcovariates.row(j);
      Sstartemp +=  exp(-newlambda(i) * exp(arma::as_scalar(betasadjusted*zj.t())) ) ;
    }
    Sstar(i) =  Sstartemp/nctotal;
  }
  
  //arma::rowvec nuj(nobs); nuj.zeros();
  for(i=0; i < tl; ++i){
    double nujtemp = 0;
    for(j=0; j < nctotal; ++j){
      arma::rowvec zj = allcovariates.row(j);
      nujtemp += exp(-newlambda(i) * exp(arma::as_scalar(betasadjusted*zj.t())) + arma::as_scalar(betasadjusted*zj.t()) ) ;
    }
    nuj(i) = nujtemp/nctotal;
  }
  
  
  for(i=0; i < tl; ++i){
    arma::rowvec zetajtemp(ncovs); zetajtemp.zeros();
    for(j=0; j < nctotal; ++j){
      arma::rowvec zj = allcovariates.row(j);
      zetajtemp += zj * exp(-newlambda(i) * exp(arma::as_scalar(betasadjusted*zj.t())) + arma::as_scalar(betasadjusted*zj.t()) ) ;
    }
    zetaj.row(i) = zetajtemp/nctotal;
  }
  
  //Estimate of S(t|Zpred). Added on 11/03/2021
  arma::rowvec Spred(tl); Spred.zeros();
  arma::rowvec Spredfirst(tl); Spredfirst.zeros();
  for(i=0; i < tl; ++i){
    Spred(i) = exp(-newlambda(i)*exp(arma::as_scalar(betas*Zpred.t())));
    Spredfirst(i) = exp(arma::as_scalar(betas*Zpred.t()) - newlambda(i)*exp(arma::as_scalar(betas*Zpred.t())));
  }
  
  for(t=0; t < tl; ++t){
    int uuu = idxtlambda(t);		
    W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
    dlt = dlambdat.cols(0,uuu-1);
    hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
    
    W3.row(t) = hLambda.t() * (iI*totalclusters) * (etai);
    //    Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
    
    //Sstart(t) = Sstar(uuu-1);
    
    //Variance of adjusted CIF
    arma::rowvec zj = zetaj.row(t); //THis is where the isssue is
    arma::colvec zjc = zj.t();
    hSstar = myRowsums( (mycbind(zjc,uuu) -  nuj(t) * S1byS0hat.cols(0,uuu-1)) % myrbind(dlt,ncovs));
    W3Sstar.row(t) = hSstar.t() * (iI*totalclusters) * (etai);      
    W1Sstar.col(t) = nuj(t)*W1.col(t);
    
    // Added on 11/03/2021
    //Variance of S(t|Z0)
    arma::colvec Zpredc = Zpred.t();
    hLambdapred = myRowsums( (mycbind(Zpredc,uuu) - S1byS0hat.cols(0,uuu-1) ) % myrbind(dlt,ncovs));
    W3pred.row(t) = hLambdapred.t() * (iI*totalclusters) * (etai);
  }
  
  Rcpp::List res(6);
  //res[0]=etai;
  //res[1]=dlambdat;
  //res[2]=W1;
  //res[3]=W3;
  //res[4]=Lambda;
  res[0]=W1Sstar;
  res[1]=W3Sstar;
  //res[2]=Sstart;
  res[2]=Sstar;
  res[3]=Spred;
  res[4]=W3pred;
  res[5]=Spredfirst;
  return(res);
  
  
}

// Rcpp::List se_adjusted_strata_COX( 
//     const arma::rowvec& times,
//     const arma::rowvec& causes,
//     const arma::mat& covariates,
//     const int& nobs, 
//     const int& ncovs,
//     const arma::rowvec& betas,
//     const int& nc,
//     const arma::mat& iI,
//     const arma::rowvec& idxtlambda,
//     const int& nctotal,//Total number of observations from all stratum. If unstratified then nobs=nctotal
//     const arma::mat& etai,
//     const int& totalclusters,
//     const arma::rowvec& newlambda,
//     const arma::mat& allcovariates,
//     const arma::rowvec& betasadjusted){
//   
//   int i,j,t;
//   
//   int tl=idxtlambda.n_cols;
//   arma::rowvec S0hat(nobs);S0hat.zeros();
//   arma::mat S1hat(ncovs,nobs);S1hat.zeros();
//   arma::mat S1byS0hat(ncovs,nobs);S1byS0hat.zeros();
//   
//   for(i=0; i<nobs; ++i){
//     double s0=0;
//     arma::colvec s1(ncovs);
//     s1.zeros();
//     arma::mat s2(ncovs,ncovs);
//     s2.zeros();
//     double wye=0;
//     
//     arma::rowvec zi = covariates.row(i);
//     
//     for(j=0; j<nobs; ++j){
//       if((times(j) >= times(i))){ //Risk set
//         arma::rowvec zj = covariates.row(j);
//         wye = exp(arma::as_scalar(betas*zj.t()));
//         s0 += wye;
//         s1 += wye * zj.t();
//         s2 += wye * zj.t() * zj;
//       }
//     }	
//     S0hat(i) = s0/nc;
//     S1hat.col(i) = s1/nc;
//     S1byS0hat.col(i) = s1/s0;
//     
//   }
//   
//   //dlambda10(t)
//   arma::rowvec dlambdat(nobs);dlambdat.zeros();
//   for(j=0; j < nobs; ++j){
//     double dlambdatemp=0;
//     if(causes(j)!=1) continue;
//     for(i=0; i < nobs; ++i){
//       if(times(j) == times(i)){
//         dlambdatemp += 1/(S0hat(j)*nc);
//       }
//     }
//     dlambdat(j)=dlambdatemp;
//   }
//   
//   
//   //dM
//   arma::mat dM(nobs,nobs);dM.zeros();
//   for(i=0; i < nobs; ++i){
//     arma::rowvec zi = covariates.row(i);
//     for(j=0; j < nobs; ++j){
//       double first=0;
//       double second=0;
//       if(causes(i)==1 && times(i)==times(j)){
//         first = 1;
//       }
//       if(times(j) <= times(i)){
//         second = exp(arma::as_scalar(betas*zi.t())) * dlambdat(j);
//       }
//       dM(i,j) = first - second;
//     }
//   }
//   
//   
//   arma::mat first_term_W = dM/myrbind(S0hat,nobs);
//   arma::mat W1(nobs,tl); W1.zeros();
//   arma::mat W3(tl,nctotal); W3.zeros();
//   arma::mat W3Sstar(tl,nctotal); W3Sstar.zeros();	arma::mat W1Sstar(nobs,tl); W1Sstar.zeros();
//   arma::vec hLambda;
//   arma::vec hSstar;
//   arma::rowvec dlt;
//   arma::rowvec Lambda(tl); Lambda.zeros();
//   arma::rowvec Sstart(tl); Sstart.zeros();
//   arma::rowvec nuj(tl); 
//   
//   arma::rowvec CumLambda0(nobs); CumLambda0.zeros();
//   CumLambda0 = arma::cumsum(dlambdat);
//   arma::mat zetaj(tl,ncovs); zetaj.zeros();
//   
//   //Estimate of adjusted survival
//   arma::rowvec Sstar(tl); Sstar.zeros();
//   
//   for(i=0; i < tl; ++i){
//     double Sstartemp = 0;
//     for(j=0; j < nctotal; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       Sstartemp +=  exp(-newlambda(i) * exp(arma::as_scalar(betasadjusted*zj.t())) ) ;
//     }
//     Sstar(i) =  Sstartemp/nctotal;
//   }
//   
//   //arma::rowvec nuj(nobs); nuj.zeros();
//   for(i=0; i < tl; ++i){
//     double nujtemp = 0;
//     for(j=0; j < nctotal; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       nujtemp += exp(-newlambda(i) * exp(arma::as_scalar(betasadjusted*zj.t())) + arma::as_scalar(betasadjusted*zj.t()) ) ;
//     }
//     nuj(i) = nujtemp/nctotal;
//   }
//   
//   
//   for(i=0; i < tl; ++i){
//     arma::rowvec zetajtemp(ncovs); zetajtemp.zeros();
//     for(j=0; j < nctotal; ++j){
//       arma::rowvec zj = allcovariates.row(j);
//       zetajtemp += zj * exp(-newlambda(i) * exp(arma::as_scalar(betasadjusted*zj.t())) + arma::as_scalar(betasadjusted*zj.t()) ) ;
//     }
//     zetaj.row(i) = zetajtemp/nctotal;
//   }
//   
//   for(t=0; t < tl; ++t){
//     int uuu = idxtlambda(t);		
//     W1.col(t) = myRowsums(first_term_W.cols(0,uuu-1));
//     dlt = dlambdat.cols(0,uuu-1);
//     hLambda = myRowsums(-1 * S1byS0hat.cols(0,uuu-1) % myrbind(dlt,ncovs));
//     
//     W3.row(t) = hLambda.t() * (iI*totalclusters) * (etai);
//     //    Lambda(t) = arma::accu(dlambdat.cols(0,uuu-1));
//     
//     //Sstart(t) = Sstar(uuu-1);
//     
//     //Variance of adjusted CIF
//     arma::rowvec zj = zetaj.row(t); //THis is where the isssue is
//     arma::colvec zjc = zj.t();
//     hSstar = myRowsums( (mycbind(zjc,uuu) -  nuj(t) * S1byS0hat.cols(0,uuu-1)) % myrbind(dlt,ncovs));
//     W3Sstar.row(t) = hSstar.t() * (iI*totalclusters) * (etai);      
//     W1Sstar.col(t) = nuj(t)*W1.col(t);
//   }
//   
//   Rcpp::List res(3);
//   //res[0]=etai;
//   //res[1]=dlambdat;
//   //res[2]=W1;
//   //res[3]=W3;
//   //res[4]=Lambda;
//   res[0]=W1Sstar;
//   res[1]=W3Sstar;
//   //res[2]=Sstart;
//   res[2]=Sstar;
//   return(res);
//   
//   
// }


// [[Rcpp::export]]

Rcpp::List mybetaestCOX( 
    const arma::rowvec& times,
    const arma::rowvec& deltas,
    const arma::mat& covariates,
    const arma::rowvec& betas){
  
  int i,j;
  const int nobs=covariates.n_rows;
  const int ncovs=covariates.n_cols;
  
  double loglikli=0;
  arma::colvec score(ncovs);
  score.fill(0.0);
  arma::mat infor(ncovs,ncovs);
  infor.fill(0.0);
  
  
  for(i=0; i<nobs; ++i){
    double s0=0;
    arma::colvec s1(ncovs);
    s1.fill(0.0);
    arma::mat s2(ncovs,ncovs);
    s2.fill(0.0);
    double ye=0;
    
    if(deltas(i)!=1) continue;//if deltas is not 1 then skip to next i
    
    arma::rowvec zi = covariates.row(i);
    loglikli += arma::as_scalar(betas*zi.t());
    score += zi.t();
    
    for(j=0; j<nobs; ++j){
      if((times(j) < times(i))) continue; //If risk set is not 1 then skip to next j
      arma::rowvec zj = covariates.row(j);
      ye = exp(arma::as_scalar(betas*zj.t()));
      s0 += ye;
      s1 += ye * zj.t();
      s2 += ye * zj.t() * zj;
    }
    loglikli -= log(s0);
    score -= s1/s0;
    infor += s2/s0 - s1*s1.t()/(s0*s0);
    
    
  }
  
  Rcpp::List res(4);
  res[0]=betas;
  res[1]=loglikli;
  res[2]=score;
  res[3]=infor;
  
  return(res);
  
}














































































