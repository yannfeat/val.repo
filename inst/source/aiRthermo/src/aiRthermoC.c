/*

aiRthermoC.c

Diagnostics for vertical atmospheric soundings

Copyright (C) 2017, Jon Saenz, Sheila Carreno and Santi Gonzalez-Roji

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

#include <string.h>
/* #include <stdio.h> */
#include <math.h>
#include <stdlib.h>
/* #include <assert.h> */

#include "aiRthermoC.h"

/*****************************************************************/

#define USE_R_INTERFACE 1
/* #undef USE_R_INTERFACE */

#ifdef USE_R_INTERFACE
#include <R.h>

void saturation_pressure_H2O_Rworld( double *Ts , int *nelems ,
				     double *result )
{
  int i;
  for(i=0;i<*nelems;i++)
    result[i]=saturation_pressure_H2O(Ts[i]);
}

void latent_heat_H2O_Rworld( double *T , int *nelems , double *Ls)
{
  int i;
  for(i=0;i<*nelems;i++)
    Ls[i]=latent_heat_H2O(T[i]);
}

void gamma_sat_P_Rworld( double *P , double *T, int *nelems , double *gamma ){
  int i;
  for(i=0;i<*nelems;i++)
    gamma[i] = gamma_s_P( P[i] , T[i] );
}



void kindex_Rworld( double *Ps , double *Ts , 
                    double *ws , int *nelems ,
		    int *doLog , double *result )
{
  *result=kindex(Ps,Ts,ws,*nelems,*doLog);
}


void TTindex_Rworld(double *Pvalues, double *Tvalues, double *wvalues, 
		      int *nlevels,int *doLog, double *result)
{
  *result=TTindex(Pvalues, Tvalues, wvalues,*nlevels,*doLog);
}

void Sindex_Rworld(double *Pvalues, double *Tvalues, double *wvalues,
	      int *nlevels , int *doLog , double *deltaP, double *result )
{
  *result=Sindex(Pvalues, Tvalues, wvalues,*nlevels , *doLog , *deltaP);
}


void LIindex_Rworld(double *Pvalues, double *Tvalues, double *wvalues,
	       int *nlevels,double *Psurface,int *doLog,double *deltaP,
	       double *PWIDTH, double *result){
  *result=LIindex(Pvalues,Tvalues,wvalues,
		  *nlevels,*Psurface,*doLog,*deltaP,
		  *PWIDTH);
}

void CAPE_CIN_Rworld( double *p0 , double *t0 , double *w0,
		      int *usePTW0 , double *PlowTop, int *precool,
		      double *airStart , double *ps , double *ts, double *ws,
		      int *nelevs , double *cape , double *cin,
		      double *apLCL, double *apLFC, double *apEL ,
		      int *dolog , double *deltaP , int *gotLCL ,
		      int *gotLFC , int *gotEL , double *Pl,
		      double *Tl, double *wl , int *Nlifted,
		      int *Olifted , int *result , int *upToTop ,
		      int *checkBouyancy ){
  *result=CAPE_CIN_C(*p0,*t0,*w0,*usePTW0,*PlowTop,*precool,
		     (AirParcelPtr) airStart, ps,ts,ws,*nelevs,cape,cin,
		     (AirParcelPtr)apLCL,(AirParcelPtr)apLFC,
		     (AirParcelPtr)apEL,*dolog,*deltaP,gotLCL,gotLFC,gotEL,
		     Pl,Tl,wl,*Nlifted,Olifted,*upToTop,*checkBouyancy);
}

void adiabatic_ascent_Rworld( double *Pstart , double *Tstart,
			      double *wstart , double *Pend ,
			      double *Tend , double *wend,
			      double *deltaP )
{
  /* 
     Pstart, Tstart, wstart, deltaP are doubles in
     the R caller routine and go back unchanged.
     Tend and wend are doubles in the R caller routine
     and hold the values of T and w at the end of the ascent.
     They are passed by reference.
  */
  adiabatic_ascent( *Pstart , *Tstart , *wstart ,
		       *Pend , Tend , wend ,
		       *deltaP );
}

void find_lcl_Rworld( double *Ptop, double *p , double *t, double *w ,
		     double *plcl , double *tlcl , double *wlcl ,
	       double *theta_lcl , double *deltaP , int *gotIT )
{
  *gotIT=find_lcl( *Ptop, *p , *t, *w ,plcl,tlcl,wlcl ,
		   theta_lcl , *deltaP );
}

void vertical_average_Rworld( double *X , double *Ptop , double *P,
		       double *T, double *ws , int *NP , int *whichvar ,
		       double *Xave )
{
  *Xave=vertical_average(X,*Ptop,P,T,ws,*NP,*whichvar);
}

void any_adiabatic_down_Rworld( double *pstart , double *tstart,
				double *wstart , double *wcstart ,
				double *pend, double *dP , double *tend,
				double *wend , double *wcend  )
{
  any_adiabatic_down( *pstart , *tstart, *wstart ,
		      *wcstart , *pend , *dP , tend,
		      wend , wcend );
}

#endif

/*************************************************/


/* #define MYDEBUG 1 */
#undef MYDEBUG 

#ifdef MYDEBUG
void dump_log(char *str, double d)
{
  FILE *optr=fopen("diagc.log","a");
  fprintf(optr,"%s = %g\n",str,d);
  fclose(optr);
} 

#endif


/*

Here starts "normal" (non-pythonic) C code

Constants are taken from Bohren & Albrecht, 1998, but are "mostly"
consistent with Petty (Atmospheric Thermodynamics), Jacobson,
Erukhimova & North and Davies-Jones. Some differences are due to truncation
or the use of specific heats at different temperatures in
different sources

*/
static double Rv=461.5;            /* J/(K kg) */ /* Bohren Albrecht */
static double Rd=287.04;            /* J/(K kg) */ /* Bohren Albrecht */
static double T0= 273.15;         /* C -> K */ 
static double es0=611.;           /* Pa */
static double P1000=100000.;      /* (Pa) for theta */
static double cp=1005.;           /*  (J/kg/K) */ /* Bohren Albrecht */
static double cv=718.;           /*  (J/kg/K) */ /* Bohren Albrecht */
static double g=9.807;             /* Gravity m/s/s */ /* Bohren Albrecht */
static double MISSING_VALUE=-99999999.;
/* A value to start with, it is computed just in case */
static double epsilon=0.622;        /* Rd/Rv */ 


#ifdef USE_R_INTERFACE

void export_constants_Rworld( double *constants )
{
  constants[0]=Rd;
  constants[1]=Rv;
  constants[2]=T0;
  constants[3]=P1000;
  constants[4]=es0;
  constants[5]=cp;
  constants[6]=g;
  epsilon=Rd/Rv;
  constants[7]=epsilon;
  constants[8]=MISSING_VALUE;
  constants[9]=Rd/cp;
  constants[10]=g/cp;
  constants[11]=cv;
}


#endif




/* 
Polynomial curve fits to Table 2.1. R. R. Rogers; M. K. Yau (1989). 
A Short Course in Cloud Physics (3rd ed.). Pergamon Press. p. 16. 
ISBN 0-7506-3215-1.
 */ 
static double Lice( double Tk )
{
  /* Fit in K from -65C to 0 C*/
  double Tc=Tk-T0;
  return (2834.4-0.26*Tc-0.0038*Tc*Tc)*1000;
}
static double Lwater( double Tk )
{
  /* Fit from -40 to 40C */
  double Tc=Tk-T0;
  return (2500.5-2.37*Tc+0.002*Tc*Tc-0.00006*Tc*Tc*Tc)*1000.;
}

/*
  L is considered over water if T>0C
  L is considered over ice if T < Tice C
  L is linearly combined between water and ice in the interval [Tice,0] C
*/
double latent_heat_H2O( double T )
{
  double Tlow;
  double wwater;
  double L;
  double Tice=-20.;

  Tlow=T0+Tice;
  if (T>=T0){
    /* Pure water above 0 C */
    L=Lwater(T);
  }else if (T<Tlow){
    /* Pure ice below Tice C */
    L=Lice(T);
  }else{
    /* Otherwise, linear weight, water disappears at Tice C */
    wwater=1.-(T0-T)/(T0-Tlow);
    L=wwater*Lwater(T)+(1.-wwater)*Lice(T);
  }
  return L;
}



/* 
cp for moist air, considering mixing ratio
*/
double moistCp( double w )
{
  return cp*(1+0.87*w2q(w));
}
/* 
cv for moist air, considering mixing ratio
*/
double moistCv( double w )
{
  return cp/1.398*(1+0.97*w2q(w));
}


/* Bohren, Albrecht (2000), pages 197-200, return es in Pa */
double saturation_pressure_over_ice( double Tkelvin )
{
  return es0*exp(6293.*(1./T0-1./Tkelvin)-0.555*log(Tkelvin/T0));
}

/* Bohren, Albrecht (2000), pages 197-200, return es in Pa */
double saturation_pressure_over_water( double Tkelvin )
{
  return es0*exp(6808.*(1./T0-1./Tkelvin)-5.09*log(Tkelvin/T0));
}

/* Buck's equation above 30 C */
double saturation_pressure_over_water_over_30C( double Tkelvin )
{
  double Tc;
  Tc=Tkelvin-T0;
  return 611.21*exp((18.678-Tc/234.5)*(Tc/(257.14+Tc)));
}

/******
Saturation pressure over water or ice, depending on the 
value of temperature
******/
#define doCorrectZeroes 1
/* #undef  doCorrectZeroes */
double saturation_pressure_H2O( double Tkelvin  )
{
  double T30;
  /* 
     In some cases, due to soundings with rows where w=0 due to 
     rounding off of very small values, the input temp might be
     extremely low (T=0 if dew point temperature is input from w2Td,
     for instance). These values are OK from the point of view of the
     input data but they are not correct from the point of view of the
     thermodynamics of the problem. If Tc is extremely low (below 70 K in
     Earth's atmosphere is freaking cold), just return 0.000001 Pa
     If you DON'T WANT this to be done, just undef doCorrectZeroes
     above
  */
#ifdef  doCorrectZeroes
  if (Tkelvin<70)
    return 0.000001;
#endif
  
  T30=30.+T0;
  if (Tkelvin>=T0){
    if (Tkelvin>=T30){
      return saturation_pressure_over_water_over_30C(Tkelvin);
    } else{
      return saturation_pressure_over_water(Tkelvin);
    }
  } else
    return saturation_pressure_over_ice(Tkelvin);
}

/* 
Saturation mixing ratio from pressure and T (K)
See Wallace and Hobbs
TeX: \[
TeX: w_s=\frac{R_d}{R_v}\frac{e_s}{p-e_s}
TeX: \]
*/
double saturation_mixing_ratio(double P, double Tkelvin)
{
  double es=saturation_pressure_H2O(Tkelvin);
  epsilon=Rd/Rv;
  return epsilon*es/(P-es);
}

/*
From relative humidity (%), Pressure (Pa) and temperature (K), 
get mixing ratio
TeX: $w=\frac{w_s rh}{100}$
*/
double rh2w( double rh , double P, double T)
{
  return saturation_mixing_ratio(P,T)*rh/100.;
}
/*
From relative humidity (%), Pressure (Pa) and temperature (K), 
get specific humidity
TeX: $q=\frac{w}{1+w}$ after having computed w from rh
*/
double rh2shum( double rh , double P, double T)
{
  double w=rh2w(rh,P,T);
  return w2q(w);
}

/* Working with dew points */
double TTd2dpd( double T , double Td )
{
  return (T-Td);
}
double dpdT2Td( double dpd , double T )
{
  return (T-dpd);
}

/*
From the dew point temperature (K), pressure (Pa) and
temperature (K), get relative humidity
TeX: $Td=T-DPD$ and $rh=100\frac{w_s(P,T_d)}{w_s(P,T)}$
*/
double Td2rh( double Td, double P, double T )
{
  return 100*saturation_mixing_ratio(P,Td)/saturation_mixing_ratio(P,T);
}

/*
From the dew point depression (K), pressure (Pa) and
temperature (K), get relative humidity
TeX: $Td=T-DPD$ and $rh=100\frac{w_s(P,T_d)}{w_s(P,T)}$
*/
double dpd2rh(double dpd, double P, double T)
{
  double Td;
  Td=dpdT2Td(dpd,T);
  return Td2rh(Td,P,T);
}

/*
From specific humidity (kg/kg) and pressure (Pa), get partial 
pressure (Pa) of water vapour
TeX: $e=\frac{qp}{\varepsilon(1-q)+q}
*/
double q2e(double q , double P)
{
  epsilon=Rd/Rv;
  return q*P/(epsilon*(1.-q)+q);
}

/*
From mixing ratio to specific humidity
TeX: $q=\frac{w}{w+1}$
*/
double w2q(double w)
{
  return w/(1.+w);
}

/*

From specific humidity to mixing ratio

TeX: $w=\frac{q}{1-q}$

*/
double q2w(double q)
{
  return q/(1.-q);
}

/*

From T(K), w(kg/kg) and P (Pa), get virtual temperature

TeX: $T_v=\frac{T}{1-(e/P)(1-\varepsilon)}$

*/
double virtual_temperature( double T, double w , double p)
{
  double Tv,e;
  epsilon=Rd/Rv;
  e=q2e(w2q(w),p);
  Tv=T/(1.-(e/p)*(1.-epsilon));
  return Tv;
}

/*
Density from P and T (if Tv given, for moist air too)
TeX: $\rho=\frac{P}{R_d T}$
*/
double density( double P, double T)
{
  return P/(Rd*T);
}
double densityH2O( double Pw, double T)
{
  return Pw/(Rv*T);
}


/*

In the inversion of es -> T, use the 
approximate expression 5.68 in Bohren

*/
double w2Td( double w , double P )
{
  epsilon=Rd/Rv;
  return 5417./(19.83+log(es0)+log(w+epsilon)-log(w*P));
}

double celsius2kelvin( double tc )
{
  return tc + T0;
}

double kelvin2celsius( double tk )
{
  return tk - T0;
}

double F2celsius( double Tf )
{
  return (Tf-32.)*5./9.;
}

double celsius2F( double Tc )
{
  return Tc*9./5.+32.;
}

double F2kelvin( double Tf )
{
  return (Tf+459.67)*5./9.;
}

double kelvin2F( double Tk )
{
  return (Tk*9/5.-459.67);
}



/*

TeX: $\theta=T\left( \frac{P_0}{P}\right)^\frac{R_d}{c_p}

*/
double theta( double T, double P , double w )
{
  return T*pow(P1000/P,Rd/moistCp(w));
}

/*
TeX: $T=\theta\left( \frac{P}{P_0}\right)^\frac{R_d}{c_p}
*/
double thetaP2T( double theta, double P , double w )
{
  return theta*pow(P/P1000,Rd/moistCp(w));
}

/*
TeX: $P=P_0\left( \frac{T}{\theta}\right)^\frac{c_p}{R_d}
*/
double thetaT2P( double theta, double T , double w )
{
  return P1000*pow(T/theta,moistCp(w)/Rd);
}

/*
  From a parcel at pressure p (Pa), temperature t (K), and mixing ratio
  w (kg/kg), make an adiabatic ascent (dry) until
  the parcel is saturated (LCL). Then, return the values of plcl (Pa),
  temperature at the LCL tlcl (K), (saturated) mixing ratio at LCL wlcl
  (kg/kg) and potential temperature at LCL theta_lcl (K). Ptop (Pa) is
  the maximum pressure that the adiabatic ascent proceeds in order to find 
  the LCL (top of the sounding is a good guess). It returns 1 (found LCL) 
  or 0 (not found LCL when arrived to Ptop). deltaP (Pa) reflects the step 
  used in the individual vertical ascents (must be +).
*/
int find_lcl( double Ptop, double p , double t, double w ,
		     double *plcl , double *tlcl , double *wlcl ,
	      double *theta_lcl , double deltaP )
{
  double the_p,the_t;
  double wsat=0;
  int gotit=0;
  double pot_temp,dP;

  dP=fabs(deltaP); /* It must be >0 */
  the_p=p;
  the_t=t;
  pot_temp=theta(t,p,w);
  wsat=saturation_mixing_ratio(the_p,the_t);
  /* Ascent from p up to the last level */
  while(the_p>Ptop){
    /* Dry adiabat to this level to find new T (first pass repeated)*/
    the_t=thetaP2T(pot_temp,the_p,w);
    /* If the point is saturated, then, return it */
    wsat=saturation_mixing_ratio(the_p,the_t);
    if ( w >= wsat ){
      gotit=1;
      break;
    }
    /* Adiabatic ascent, potential temperature is constant */
    the_p-=dP;
    /* In case Ptop!= Pinitial-n*deltaP break at the requested value of Ptop */
    if (the_p<Ptop)
      the_p=Ptop;
  }
  *plcl=the_p;
  *tlcl=the_t;
  *wlcl=wsat;
  /* Conserved until LCL */
  *theta_lcl=pot_temp;
  return gotit;
}


/*
  Saturated adiabatic profile in T (K), P (Pa) coordinates, see
  page 116, eq 7.71 in Tsonis, An introduction to atmospheric
  thermodynamics. It has been modified considering that the environment is
  in hydrostatic equilibrium to dT/dP instead of dT/dz by myself.
*/
double gamma_s_P( double P , double T )
{
  double gamma;
  double rho;
  double es;
  double cpmix;
  double L;
  double ws;

  epsilon=Rd/Rv;
  ws=saturation_mixing_ratio(P,T);
  cpmix=moistCp(ws);
  L=latent_heat_H2O(T);
  es=ws*P/(ws+epsilon);
  rho=(es/Rv+(P-es)/Rd)/T;
  gamma=(1+L*ws/Rd/T)/(1+L*L*ws/cpmix/Rv/T/T)/cpmix/rho;
  return gamma;
}


/* Integrate one step of the ODE given by dT/dP=-gamma(T,P) using RK4 */
static double TRK4( double P , double T , double dP )
{
  double h=-dP;
  double k1, k2, k3,k4;
  double P2,T2,T3,P4,T4;
  
  k1=gamma_s_P(P,T);
  
  T2=T+h*k1/2;
  P2=P+h/2;
  k2=gamma_s_P(P2,T2);
  
  T3=T+h*k2/2;
  k3=gamma_s_P(P2,T3);
  
  P4=P+h;
  T4=T+h*k3;
  k4=gamma_s_P(P4,T4);
  return T+h*(k1+2*k2+2*k3+k4)/6.;
}

/*
  Saturated adiabatic profile in T (K), P (Pa) coordinates, see
  page 113, eq 7.29 in Tsonis, An introduction to atmospheric
  thermodynamics. It has been modified considering that the environment is
  in hydrostatic equilibrium to dT/dP instead of dT/dz by myself.
*/
double gamma_d_P( double P , double T , double w )
{
  double Tv;
  double rho;
  Tv=virtual_temperature(T,w,P);
  rho=density(P,Tv);
  return 1./moistCp(w)/rho;
}

void evaporate( double *w , double *wc, double wsat )
{
  double evap;
  evap=wsat-*w;
  /* Limit evaporation to the maximum available */
  if(evap>=*wc){
    evap=*wc;
  }
  *wc=*wc-evap;
  *w=*w+evap;
}

void condensate( double *w , double *wc, double wsat )
{
  double cond;
  cond=*w-wsat;
  /* Limit condensation to the maximum available */
  if(cond>=*w){
    cond=*w;
  }
  *wc=*wc+cond;
  *w=*w-cond;
}

typedef enum{ kUp , kDown } UPDOWN;

/* 
Integrate one step of the ODE given by dT/dP=-gamma(T,P) using RK4 
only working for Down by now
*/
static double dTdPslope( double *P , double *T, double *w , double *wc )
{
  double k,wsat;
  
  wsat=saturation_mixing_ratio(*P,*T);
  /* Different profiles if saturated */
  if (*w>=wsat){
    k=gamma_s_P(*P,*T);
  }else{
    k=gamma_d_P(*P,*T,*w);
  }
  return k;
}

void TRK4CondDown( double *P , double *T , double *w ,
			double *wc , double dP )
{
  double h=dP;
  double k1, k2, k3,k4;
  double P2,T2,T3,P4,T4;
  double wsat;

  k1=dTdPslope(P,T,w,wc);
  T2=*T+h*k1/2;
  P2=*P+h/2;

  k2=dTdPslope(&P2,&T2,w,wc);
  T3=*T+h*k2/2;

  k3=dTdPslope(&P2,&T3,w,wc);
  P4=*P+h;
  T4=*T+h*k3;

  k4=dTdPslope(&P4,&T4,w,wc);

  /* New values */
  /* T by RK4 */
  *T=(*T+h*(k1+2*k2+2*k3+k4)/6.);
  /* P increased as requested by dP */
  *P+=h;
  /* Just in case, recalculate new wsat/cond */
  wsat=saturation_mixing_ratio(*P,*T);
  if (*w < wsat){
     evaporate(w,wc,wsat);
  }
  if (*w > wsat){
     condensate(w,wc,wsat);
  }
}

/*
In this case, we start from a parcel at pressure pstart ,
temperature tstart and mixing ratio wstart, with some condensates
(water/ice, we do not discuss that, L is computed from T)
and the parcel goes down evaporating the condensates or
by means of a dry adiabat (depending on the level and so on)
until the level pend, where it will have a
temperature *Tend, mixing ratio (vapour) *wend 
and (may be still some condensate) *wcend
using steps of pressure dP (always +)
*/
void any_adiabatic_down( double pstart , double tstart, double wstart ,
			 double wcstart , double pend, double dP ,
			 double *tend, double *wend , double *wcend )
{
  double P=pstart;
  double T=tstart;
  double w=wstart;
  double wc=wcstart;
  double deltaP=dP;
  int nextOut=0;
  
  while(P<=pend){
    /* If next step is the last one, fix deltaP */
    if ((P+dP)>pend){
      deltaP=pend-P;
      nextOut=1;
    }
    TRK4CondDown( &P , &T , &w , &wc , deltaP );
    if (nextOut)
      break;
  }
  *tend=T;
  *wend=w;
  *wcend=wc;
}



/*
  A saturated parcel at p0 (Pa), t0 (K) undergoes a saturated 
  adiabatic evolution to a final point at pend (Pa). 
  Returns the temperature (K) of the parcel after the saturated evolution.
  deltaP (Pa) reflects the step  used in the individual vertical 
  ascents (must be +).
*/
#undef USE_EULER_4_ODE
#define USE_RK4_4_ODE 1
double t_after_saturated_adiabat(double p0, double t0,
				 double pend , double deltaP )
{
  double p=p0;
  double t=t0;
#ifdef  USE_EULER_4_ODE
  double dTdP;
#endif
  double dP;

  dP=fabs(deltaP);
  while (p>pend){
    /* Modify the equation for gamma_s so that it works in P coordinates */
#ifdef USE_EULER_4_ODE
    dTdP=gamma_s_P(p,t);
    t=t-dTdP*dP;
#endif
#ifdef USE_RK4_4_ODE
    t=TRK4(p , t , dP );
#endif
    p-=dP;
  }
  /* The ascent is done */
  return t;
}

double t_after_saturated_adiabat_down(double p0, double t0,
				 double pend , double deltaP )
{
  double p=p0;
  double t=t0;
#ifdef  USE_EULER_4_ODE
  double dTdP;
#endif
  double dP;

  dP=fabs(deltaP);
  while (p<pend){
    /* Modify the equation for gamma_s so that it works in P coordinates */
#ifdef USE_EULER_4_ODE
    dTdP=gamma_s_P(p,t);
    t=t+dTdP*dP;
#endif
#ifdef USE_RK4_4_ODE
    t=TRK4(p , t , -dP );
#endif
    p+=dP;
  }
  /* The ascent is done */
  return t;
}

/* 
   A parcel at Pstart (Pa), Tstart (K) and wstart mixing ratio (kg/kg)
   adiabatically ascends to Pend (Pa) under hydrostatic equilibrium
   and keeping w constant until saturation and saturated afterwards
*/
void adiabatic_ascent( double Pstart , double Tstart , double wstart ,
		       double Pend , double *Tend , double *wend ,
		       double deltaP )
{
  double p=Pstart;
  double t=Tstart;
  double w=wstart;
  double ws;
  double dP;
  double thet,wtemp;

  /* Ensure dP is >0 */
  dP=fabs(deltaP);
  while (p>Pend){
    /* Be sure to finish exactly at the appropriate pressure */
    if ((p-dP) < Pend ){
      /* Next time, we will finish exactly at the level requested */
      dP=p-Pend;
    }
    /* Now, the state of the parcel is p,t,w */
    ws=saturation_mixing_ratio(p,t);
    if (w<ws){
      /* Use potential temperature */
      thet=theta(t,p,w);
      /* New temperature */
      t=thetaP2T(thet,p-dP,w);
    }else{
      /* saturated adiabat, new temperature */
      t=TRK4(p , t , dP );
    }
    /* We have already computed t at P-dP, new state P-dP , T */
    p=p-dP;
    wtemp=saturation_mixing_ratio(p,t);
    if (w>=wtemp){
      *wend=wtemp;
    }else{
      *wend=w;
    }
    *Tend=t;
  }
}

  

/*
Property X[nlevs] is placed at different vertical pressure levels
Plevs[nlevs] (Pa). This function returns the value corresponding to
level P (Pa) by means of interpolation. The interpolation is
linear (doItLog==0) or logarithmic (if doItLog!=0). If P is not
in range [Pmin,Pmax] given by the sounding values in *Plevs,
out_of_range is set to one.
*/
double interpolate_in_p( double *X,
				double *Plevs,
				int nlevs,
				double P, 
				int *out_of_range , int doItLog )
{
  int i;
  int iA,iB;
  double interp_val=-99999;

  /* A points to low and B to high */
  if (Plevs[0]>Plevs[1]){
    iB=0;
    iA=nlevs-1;
  } else {
    iA=0;
    iB=nlevs-1;
  }

  for(i=0;i<nlevs;i++){
    if (Plevs[i]==P){
      *out_of_range=0;
      return X[i];
    }
    if ((Plevs[i] < P) && (abs(iA-iB)>1)){
      iA=i;
    }
    if ((Plevs[i] > P ) && (abs(iA-iB)>1)){
      iB=i;
    }
  }
#ifdef MYDEBUG
  dump_log("interpolate_in_p() -> iA",iA);
  dump_log("interpolate_in_p() -> P[iA]",Plevs[iA]);
  dump_log("interpolate_in_p() -> i",i);
  dump_log("interpolate_in_p() -> P[iB]",Plevs[iB]);
  dump_log("interpolate_in_p() -> iB",iB);
  dump_log("interpolate_in_p() -> P",P);
  dump_log("END",0);
#endif
  if ((Plevs[iA]<=P)&&(P<=Plevs[iB])){
    *out_of_range=0;
    if (doItLog){
      interp_val=X[iA]+(log(P)-log(Plevs[iA]))/
        (log(Plevs[iB])-log(Plevs[iA]))*(X[iB]-X[iA]);
    } else {
      interp_val=X[iA]+(P-Plevs[iA])/
        (Plevs[iB]-Plevs[iA])*(X[iB]-X[iA]);
    }
  } else
    *out_of_range=1;
  return interp_val;
}

/* 
For a sounding provided in arrays of pressure Pvalues[nlevels] (Pa),
temperature in Tvalues[nlevels] (K) and mix ratio Wvalues[nlevels] (kg/kg),
return the value of the K index of instability using linear (doLog=0)
or logarithmic (doLog=1) interpolation. Since levels in K index are mandatory,
they are probably included in the original sounding and the use
of doLog usually doesn't affect the results
*/
double kindex(double *Pvalues, double *Tvalues,double *wvalues,
		     int nlevels , int doLog )
{
  int out_any, out_point;
  double t850,w850,td850;
  double t500;
  double t700,w700,td700;
  double Kindex;

  out_any=0;
  t850=interpolate_in_p(Tvalues,Pvalues,nlevels,85000.,&out_point,doLog);
  out_any+=out_point;
  t700=interpolate_in_p(Tvalues,Pvalues,nlevels,70000.,&out_point,doLog);
  out_any+=out_point;
  t500=interpolate_in_p(Tvalues,Pvalues,nlevels,50000.,&out_point,doLog);
  out_any+=out_point;
  w850=interpolate_in_p(wvalues,Pvalues,nlevels,85000.,&out_point,doLog);
  out_any+=out_point;
  w700=interpolate_in_p(wvalues,Pvalues,nlevels,70000.,&out_point,doLog);
  out_any+=out_point;
  if (out_any)
    Kindex=MISSING_VALUE;
  else{
    td850=w2Td(w850,85000.);
    td700=w2Td(w700,70000.);
    Kindex=kelvin2celsius(t850)-
      kelvin2celsius(t500)+kelvin2celsius(td850)-kelvin2celsius(t700)+
      kelvin2celsius(td700);
  }
  return Kindex;
}

/* 
For a sounding provided in arrays of pressure Pvalues[nlevels] (Pa),
temperature in Tvalues[nlevels] (K) and mix ratio Wvalues[nlevels] (kg/kg),
return the value of the Total-Totals index of instability using linear (doLog=0)
or logarithmic (doLog=1) interpolation. Since levels in Total-Totals 
index are mandatory, they are probably included in the original sounding 
and the use of doLog usually doesn't affect the results
*/
double TTindex(double *Pvalues, double *Tvalues, double *wvalues, 
		      int nlevels,int doLog)
{
  int out_any;
  int out_point;
  double t850,t500,w850,td850,ttindex;

  out_any=0;
  t850=interpolate_in_p(Tvalues,Pvalues,nlevels,85000.,&out_point,doLog);
  out_any+=out_point;
  t500=interpolate_in_p(Tvalues,Pvalues,nlevels,50000.,&out_point,doLog);
  out_any+=out_point;
  w850=interpolate_in_p(wvalues,Pvalues,nlevels,85000.,&out_point,doLog);
  out_any+=out_point;

  /* printf("%g %g %g\n",t850,t500,w850); */

  if (out_any)
    ttindex=MISSING_VALUE;
  else{
    td850=w2Td(w850,85000.);
    ttindex=kelvin2celsius(td850)+
      kelvin2celsius(t850)-2*kelvin2celsius(t500);
  }
  return ttindex;
}

/* 
Given a sounding "layer" from Pb (Pa) to Pt (Pa)
with Tvb (K) at bottom and Tvt (K) at top,
get its thickness taking fully into account
that T varies in the vertical
*/
static double deltaZ_fromP( double Pb , double Pt , double Tvb , double Tvt )
{
  double A;
  double B;
  double deltaT;
  double deltaP;
  double dZ;
  
  deltaP=Pt-Pb;
  deltaT=Tvt-Tvb;
  B=deltaT/deltaP;
  A=Tvb-B*Pb;
  dZ=Rd/g*(A*log(Pb/Pt)-deltaT);
  return dZ;
}

/* 
Given a sounding "layer" from Pb (Pa) to Pt (Pa)
with Tvb (K) at bottom and Tvt (K) at top,
get the average of X (Xb to Xt) taking fully into account
that T varies in the vertical (returned multiplied by dZ)
If it is mixratio, convert to density of vapour
*/
static double dZ_by_aveX( double Pb , double Pt , double Tvb , double Tvt ,
			  double xb , double xt , int whichvar )
{
  double At;
  double Bt;
  double deltaT,deltaX;
  double deltaP;
  double Ax,Bx;
  double rho;
  double Xb,Xt;

  if (whichvar==1){
    /* In this case (mix-ratio), convert to rho_v w->q and q*rho */
    rho=density(Pb,Tvb);
    Xb=w2q(xb)*rho;
    rho=density(Pt,Tvt);
    Xt=w2q(xt)*rho;
  }else if (whichvar==2){
    /* In this case (density), convert to rho */
    rho=density(Pb,Tvb);
    Xb=rho;
    rho=density(Pt,Tvt);
    Xt=rho;
  }else{
    Xb=xb;
    Xt=xt;
  }
  deltaT=Tvt-Tvb;
  deltaP=Pt-Pb;
  Bt=deltaT/deltaP;
  At=Tvb-Bt*Pb;
  deltaX=Xt-Xb;
  Bx=deltaX/deltaP;
  Ax=Xb-Bx*Pb;
  return Rd/g*(Ax*At*log(Pb/Pt)-deltaP*(At*Bx+Ax*Bt)+Bx*Bt*(Pb*Pb-Pt*Pt)/2.);
}


/* 
Given an array of values X[NP] at isobaric levels P[NP],, return a vertical
average of X from PSurface (Pa) to Pt (Pa), performing vertical integrations
by accumulating slabs
\[
\overbar{X}=\frac{R_d}{g\Delta Z}\int_{P_t}^{P_{SFC}} \frac{X T_v}{P} dP
\]
whichvar switches some behaviours for mixed ratio or density
*/
double vertical_average(double *X, double Ptop , double *P ,
			double *T , double *ws ,int NP , int whichvar )
{
  int k;
  double Pb,Pt,Tb,Tt,wt,wb,Tvb,Tvt,Xb,Xt;
  double accum=0.0;
  int dI,iStart;
  double dZ;

  if (P[0]>P[1]){
    dI=1;
    iStart=0;
  } else {
    dI=-1;
    iStart=NP-1;
  }
  /* At least one always */
  Pb=P[iStart];
  Pt=P[iStart+dI];
  Tb=T[iStart];
  Tt=T[iStart+dI];
  Xb=X[iStart];
  Xt=X[iStart+dI];
  wb=ws[iStart];
  wt=ws[iStart+dI];
  Tvb=virtual_temperature(Tb,wb,Pb);
  Tvt=virtual_temperature(Tt,wt,Pt);
  accum=dZ_by_aveX(Pb , Pt , Tvb , Tvt , Xb , Xt , whichvar );
  dZ=deltaZ_fromP( Pb , Pt , Tvb , Tvt );
  
  /* Next ones... depending */
  k=iStart+dI;
  while (1){
    Pb=P[k];
    Pt=P[k+dI];
    /* If bottom in this level is over Ptop, then exit */
    if (Pb<Ptop)
      break;
    /* If top is below Ptop, consider it in full */
    if (Pt<=Ptop){
      Tb=T[k];
      Tt=T[k+dI];
      Xb=X[k];
      Xt=X[k+dI];
      wb=ws[k];
      wt=ws[k+dI];
      Tvb=virtual_temperature(Tb,wb,Pb);
      Tvt=virtual_temperature(Tt,wt,Pt);
      accum+=dZ_by_aveX(Pb , Pt , Tvb , Tvt , Xb , Xt , whichvar );
      dZ+=deltaZ_fromP( Pb , Pt , Tvb , Tvt );
    } else {
      /* Only part of the slab must be integrated */
      Pt=Ptop;
      Tb=T[k];
      Tt=Tb+(T[k+dI]-Tb)*(Pt-Pb)/(P[k+dI]-Pb);
      Xb=X[k];
      Xt=Xb+(X[k+dI]-Xb)*(Pt-Pb)/(P[k+dI]-Pb);
      wb=ws[k];
      wt=wb+(ws[k+dI]-wb)*(Pt-Pb)/(P[k+dI]-Pb);
      Tvb=virtual_temperature(Tb,wb,Pb);
      Tvt=virtual_temperature(Tt,wt,Pt);
      accum+=dZ_by_aveX(Pb , Pt , Tvb , Tvt , Xb , Xt , whichvar );
      dZ+=deltaZ_fromP( Pb , Pt , Tvb , Tvt );
    }
    k+=dI;
    if ((k==NP)||(k<1)){
      break;
    }
  }
  return accum/dZ;
}

/* 
For a sounding provided in arrays of pressure Pvalues[nlevels] (Pa),
temperature in Tvalues[nlevels] (K) and mix ratio Wvalues[nlevels] (kg/kg),
return the value of the Showalter index of instability using linear (doLog=0)
or logarithmic (doLog=1) interpolation. Since top levels used in 
Showalter index are mandatory, they are probably included in the original 
soundsounding and the use of doLog usually doesn't affect the results
*/
double Sindex(double *Pvalues, double *Tvalues, double *wvalues,
	      int nlevels , int doLog , double deltaP )
{
  int out_point,out_any,gotlcl;
  double t500,t500star,t850,p850,w850;
  double p_lcl,t_lcl,w_lcl,theta_lcl;
  double sindex=MISSING_VALUE;
  double Pmin;
  double dP;

  dP=fabs(deltaP);

  /*
    First of all, get the values at 850 hPa
    (as perceived by the model)
  */
  p850=85000.;
  out_any=0;
  t850=interpolate_in_p(Tvalues,Pvalues,nlevels,p850,&out_point,doLog);
  out_any+=out_point;
  w850=interpolate_in_p(wvalues,Pvalues,nlevels,p850,&out_point,doLog);
  out_any+=out_point;
  if (out_any){
    sindex=MISSING_VALUE;
  } else {
    /*
      Let's find the LCL
    */
    Pmin=Pvalues[0]<Pvalues[nlevels-1]?Pvalues[0]:Pvalues[nlevels-1];
    gotlcl=find_lcl(Pmin,p850,t850,w850,
		    &p_lcl,&t_lcl,&w_lcl,&theta_lcl,dP);
    if (gotlcl){
      if (p_lcl>50000){
	t500star=t_after_saturated_adiabat( p_lcl, t_lcl, 50000.,dP);
      } else {
	/* Quite unlikely, but it might be... */
	t500star=thetaP2T(theta_lcl,50000.,w850);
      }
      out_any=0;
      t500=interpolate_in_p(Tvalues,Pvalues,nlevels,50000.,&out_point,doLog);
      out_any+=out_point;
      if (out_any)
	sindex=MISSING_VALUE;
      else
	sindex=t500-t500star;
    } else{
      sindex=MISSING_VALUE;
    }
  }
  return sindex;
}

static void getBottomAverage(double *P_ave_bottom,double *T_ave_bottom,
			double *w_ave_bottom,double *Pvalues,
			double *Tvalues,double *wvalues,
			     int nlevels,double PlowTop,double Psurface)
{
  double rhoave;
  double rhovave;
  double wsat;

  *T_ave_bottom=vertical_average(Tvalues,PlowTop,Pvalues,
				 Tvalues,wvalues,nlevels,0);
  *P_ave_bottom=vertical_average(Pvalues,PlowTop,Pvalues,
				 Tvalues,wvalues,nlevels,0);
  wsat=saturation_mixing_ratio(*P_ave_bottom,*T_ave_bottom);
  rhoave=vertical_average(Tvalues,PlowTop,Pvalues,Tvalues,wvalues,nlevels,2);
  rhovave=vertical_average(wvalues,PlowTop,Pvalues,Tvalues,wvalues,nlevels,1);
  /* (rhovave / rhoave) is specific humidity at the average point */
  *w_ave_bottom=q2w(rhovave/rhoave);
  if (*w_ave_bottom>wsat)
    *w_ave_bottom=wsat;
}

/* #define MYDEBUG 1 */

/* 
For a sounding provided in arrays of pressure Pvalues[nlevels] (Pa),
temperature in Tvalues[nlevels] (K) and mix ratio Wvalues[nlevels] (kg/kg),
return the value of the Lifted index of instability using linear (doLog=0)
or logarithmic (doLog=1) interpolation. Since top levels used in 
Lifted index are mandatory, they are probably included in the original 
sounding and the use of doLog usually doesn't affect the results
deltaP (Pa) reflects the step  used in the individual vertical 
ascents (must be +). PWIDTH (Pa, >0) is the width of the troposphere
that must be averaged in the low levels, 5000 or 10000 Pa are good values.
*/
double LIindex(double *Pvalues, double *Tvalues, double *wvalues,
	       int nlevels,double Psurface,int doLog,double deltaP,
	       double PWIDTH)
{
  int out_point,out_any,gotlcl;
  double t500,Pmin;
  double T_ave_bottom, w_ave_bottom,P_ave_bottom;
  double p_lcl,t_lcl,w_lcl,theta_lcl,t500star;
  double liindex=MISSING_VALUE;
  double dP, Pmax;
  
  dP=fabs(deltaP);
#ifdef MYDEBUG
  {double Pdot, Pdotdot;
    Pdot=(Psurface+(Psurface-PWIDTH))/2.;
    Pdotdot=vertical_average(Pvalues,Psurface-PWIDTH,Pvalues, Tvalues,
			     wvalues,nlevels);
    printf("Average pressure: %g vs %g vs %g [%g,%g]\n",Pdot,
	   Pdotdot,P_ave_bottom,Psurface,Psurface-PWIDTH);
  }
#endif

  /*
    Initialize starting parcel
  */

  if (Pvalues[0]>Pvalues[nlevels-1]){
    Pmin=Pvalues[nlevels-1];
    Pmax=Pvalues[0];
    /* Just in case if not updated later */
    P_ave_bottom=Pmax;
    T_ave_bottom=Tvalues[0];
    w_ave_bottom=wvalues[0];
  }else{
    Pmin=Pvalues[0];
    Pmax=Pvalues[nlevels-1];
    /* Just in case if not updated later */
    P_ave_bottom=Pmax;
    T_ave_bottom=Tvalues[nlevels-1];
    w_ave_bottom=wvalues[nlevels-1];
  }

  if (PWIDTH>0){
    getBottomAverage(&P_ave_bottom,&T_ave_bottom,&w_ave_bottom,
		   Pvalues,Tvalues,wvalues,nlevels,Pmax-PWIDTH,Pmax);
  }
  
  /*
    Let's find the LCL
  */
  gotlcl=find_lcl(Pmin,P_ave_bottom,T_ave_bottom,w_ave_bottom,
		  &p_lcl,&t_lcl,&w_lcl,&theta_lcl,dP);
  if (gotlcl){
    if (p_lcl>50000.){
      t500star=t_after_saturated_adiabat( p_lcl, t_lcl, 50000.,dP);
    }else{
      t500star=thetaP2T(theta_lcl,50000.,w_ave_bottom);
    }
    out_any=0;
    t500=interpolate_in_p(Tvalues,Pvalues,nlevels,50000.,&out_point,doLog);
    out_any+=out_point;
    if (out_any)
      liindex=MISSING_VALUE;
    else
      liindex=t500-t500star;
  } else{
    liindex=MISSING_VALUE;
  }
  return liindex;
}

static void getAB( double Tb , double Tu , double Pb, double Pu , double *A ,
	      double *B )
{
  double deltaP=Pb-Pu;

  *A=-(Tb*Pu-Tu*Pb)/deltaP;
  *B=-(Tu-Tb)/deltaP;
}

/*
Compute the energy (J/m2) of a slab with low pressure pb (Pa) limited
by ambient (bottom tvab|top tvau) virtual temperature (K)
and lifted parcel  (bottom tvlb|top tvlu) virtual temperature (K)
*/
double energy_area( double pb, 
				  double tvab , double tvau,
		    double tvlb, double tvlu , double deltaP )
{
  /*  double thelog;
  double dP;
  dP=fabs(deltaP);
  thelog=log(pb/(pb-dP));
  return Rd*(
	     (tvlb-tvab)*thelog+
	     ((tvau-tvlu)-(tvab-tvlb))/dP*(dP-pb*thelog)
	     );
  */
  double dP=fabs(deltaP);
  double Al,Bl,Aa,Ba;
  getAB(tvlb,tvlu,pb,pb-dP,&Al,&Bl);
  getAB(tvab,tvau,pb,pb-dP,&Aa,&Ba);
  return (Rd*(Al-Aa)*log(pb/(pb-dP))+Rd*(Bl-Ba)*dP);
}

/* 
Make the state of the parcel internally consistent
from stored values of T (K), P (Pa) and w (kg/kg), not exported
outside this module, do NOT try to find it in *.h
*/
void setParcelState( AirParcelPtr ap )
{
  ap->theta=theta(ap->t,ap->p,ap->w);
  ap->Tv=virtual_temperature(ap->t,ap->w,ap->p);
  ap->wsat=saturation_mixing_ratio(ap->p,ap->t);
}


/* 
Get the value for the environment of the parcel
ap that is lifting accross the sounding
given by pvalues[nlevels] (Pa), Tvalues[nlevels] (K)
and wvalues[nlevels] (kg/kg), at pressure pto. If the computation
performs properly, OK is assigned 1. 
The vertical interpolation is logarithmic if doLog==1.
*/
void environment_data( AirParcelPtr ap , double *pvalues,
				     double *Tvalues, double *wvalues,
				     int nlevels , int *OK , double pto,
                                     int doLog)
{
  int out;

  *OK=1;

  /* set target pressure */
  ap->p=pto;

  ap->t=interpolate_in_p(Tvalues,pvalues,nlevels,pto,&out,doLog);
  if (out){
    *OK=0;
  }
  ap->w=interpolate_in_p(wvalues,pvalues,nlevels,pto,&out,doLog);
  if (out){
    *OK=0;
  }
  setParcelState(ap);
}

static int isBuoyant( AirParcelPtr  liftParcel , AirParcelPtr ambParcel )
{
  return ((liftParcel->Tv)>=(ambParcel->Tv)) ;
}

/*
Get values of the air parcel ap after a change in pressure dP (Pa).
This function knows whether dry or saturated adiabatic must be used.
*/
void adiabatic_evolution( AirParcelPtr ap , double dP , int goDown )
{
  double pfrom=ap->p;
  double pto;
  double wsat;
  double thesign=1.;

  if(goDown)
    thesign=-1.;

  /* We DO NOT enforce dP>0 here, but we still assume it, be CAREFUL */
  pto=ap->p-thesign*dP;
  /* ascent */
  ap->p=pto;

  /* Adjust T and w */
  if (ap->wsat<=ap->w){
    if (goDown)
      ap->t=t_after_saturated_adiabat_down(pfrom,ap->t,pto,dP);
    else
      ap->t=t_after_saturated_adiabat(pfrom,ap->t,pto,dP);
    
    /* w and theta at the new level */
    wsat=ap->w=saturation_mixing_ratio(pto,ap->t);
    ap->theta=theta(ap->t,ap->p,ap->w);
  } else{
    ap->t=thetaP2T(ap->theta,ap->p,ap->w);
    wsat=saturation_mixing_ratio(pto,ap->t);
    /* ap->w and ap->theta must not be changed in this case */
  }
  /* Tv is changed in any case, given new values of T and w */
  ap->Tv=virtual_temperature(ap->t,ap->w,ap->p);
  ap->wsat=wsat;
}


/* #define MYDEBUG 1 */
#undef MYDEBUG 

#ifdef MYDEBUG
void displayParcels( AirParcelPtr lb , AirParcelPtr lu,
		     AirParcelPtr ab , AirParcelPtr au,
		     int stage , double dE , double *cin,
		     double *cape)
{
    FILE *ofile=fopen("capecin.log","a");
    char *fmstr="%s %11.6f hPa %11.6f K %9.6f g/kg %9.6f %% %9.6f kg/m3\n";
    AirParcelPtr app;

    app=lb;
    fprintf(ofile,fmstr,"LB",
	    app->p/100,app->t,app->w*1000,app->w/app->wsat,
	    density(app->p,virtual_temperature(app->t,app->w,app->p)));
    app=ab;
    fprintf(ofile,fmstr,"AB",
	    app->p/100,app->t,app->w*1000,app->w/app->wsat,
	    density(app->p,virtual_temperature(app->t,app->w,app->p)));
    app=lu;
    fprintf(ofile,fmstr,"LU",
	    app->p/100,app->t,app->w*1000,app->w/app->wsat,
	    density(app->p,virtual_temperature(app->t,app->w,app->p)));
    app=au;
    fprintf(ofile,fmstr,"AU",
	    app->p/100,app->t,app->w*1000,app->w/app->wsat,
	    density(app->p,virtual_temperature(app->t,app->w,app->p)));
    fprintf(ofile,"Is lifted Buoyant? L: %d U: %d\n",
	    isBuoyant(lb,ab),isBuoyant(lu,au));

    fprintf(ofile,"dE %5.2f J CAPE %5.2f CIN %5.2f stage: %d\n\n",dE,
	    *cin,*cape , stage);
    fclose(ofile);
}
#endif

int save2lifted( AirParcelPtr ap, double *Pl, double *Tl,
		 double *wl, int Nl, int *ol , int nline )
{
  if ((Pl!=NULL) && (Tl!=NULL) && (wl!=NULL)){
    if ((Nl>0) && (*ol<Nl)){
      Pl[*ol]=ap->p;
      Tl[*ol]=ap->t;
      wl[*ol]=ap->w;
      *ol=*ol+1;
    } else
      *ol=0;
  }
#ifdef MYDEBUG
  if (ap->t<99){
    fprintf(stderr,"Line: %d with T < 100 in %d\n",nline,*ol);
  }
#endif
  /* Avoid compiler messages */
  nline=0;
  return *ol;
}

void checkLCL( int *gotLCL, AirParcelPtr apLCL , AirParcelPtr lb,
	       AirParcelPtr lu , AirParcelPtr ab, AirParcelPtr au )
{
  double deltaWu, deltaWb;
  double pb,pu;
  double tb,tu;
  double wb,wu;
  
  if (*gotLCL){
    return;
  }
  deltaWb=lb->w-lb->wsat;
  deltaWu=lu->w-lu->wsat;
  pb=lb->p;
  pu=lu->p;
  tb=lb->t;
  tu=lu->t;
  wb=lb->w;
  wu=lu->w;
  if ((deltaWb<0)&&(deltaWu>=0)){
    *gotLCL=1;
    apLCL->p=pb-deltaWb*(pu-pb)/(deltaWu-deltaWb);
    apLCL->t=tb+(tu-tb)*(apLCL->p-pb)/(pu-pb);
    apLCL->w=wb+(wu-wb)*(apLCL->p-pb)/(pu-pb);
    setParcelState(apLCL);
  }
}


/*
From a parcel at p0 (Pa), t0 (K) and w0 (kg/kg), perform a
vertical average of temperature and a mass-weighted vertical
average of mixing ratio to identify the characteristics
of the initial status of the parcel after (eventually)
performing isobaric or adiabatic precooling if needed, according
to the value in pre_cool_type. The state of the parcel
at the start is stored in apstart. The state of the ambient air is
provided by the sounding pvalues[nlevels] (Pa), Tvalues [nlevels] (K)
and wvalues[nlevels] (kg/kg). The values of CAPE and CIN are stored
at *cape and *cin (J/kg). The properties of LCL, LFC and EL (if found)
is stored in apLCL, apLFC and apEL. if doLog!=0, the vertical interpolation
is performed using logarithm of pressure. deltaP is the size of the
vertical displacement in presure for the integral, that is
performed using finite slabs. (see energy function above).
It returns a numeric code that describes the different possibilities
(everything is OK, returned 0, no LCL found, returned 1, noLFC found ...
and so on). The evolution of the lifted particle
is returned in Plifted[Nlifted], Tlifted[Nlifted] and wlifted[Nlifted],
with *Olifted holding the real number of levels on output
*/
#define ADIABATIC_PRE_COOL 1
#define ISOBARIC_PRE_COOL  2

/*
Lifted particle crosses the sounding from Right to left
*/
static int isR2LCrossing(AirParcelPtr ab,
		    AirParcelPtr au,
		    AirParcelPtr lb,
		    AirParcelPtr lu )
{
  return (((lb->Tv) > (ab->Tv))&&((au->Tv) >= (lu->Tv)));
}

/*
Lifted particle crosses the sounding from left to right
*/
/*
static int isL2RCrossing(AirParcelPtr ab,
		    AirParcelPtr au,
		    AirParcelPtr lb,
		    AirParcelPtr lu )
{
  return ((lb->Tv<ab->Tv)&&((lu->Tv)>=(au->Tv)));
}
*/


#define DEBUG_ENTRY 1
/* undef DEBUG_ENTRY */
int CAPE_CIN_C( double p0,
		double t0,
		double w0,
		int usePTW0,
		double PlowTop,
		int pre_cool_type ,
		AirParcelPtr apstart,
		double *pvalues,
		double *Tvalues,
		double *wvalues,
		int nlevels,
		double *cape,
		double *cin,
		AirParcelPtr apLCL,
		AirParcelPtr apLFC,
		AirParcelPtr apEL,
		int doLog ,
		double deltaP,
		int *gotLCL,
		int *gotLFC,
		int *gotEL,
		double *Plifted ,
		double *Tlifted ,
		double *wlifted ,
		int Nlifted ,
		int *Olifted ,
		int upToTop ,
		int checkBuoyancy 
		)
{
  int OK;
  double Ptop;
  double Plow;
  /*double PLFC,PEL;*/
  double dE;
  AirParcel ambient;
  AirParcel start;
  AirParcel lb,lu;
  AirParcel ab,au;
  int ilow;
  double dP;
  double negEtemp;
  double nextP;
  double Pmiddle;

#ifdef MYDEBUG
  int stage=0;
#endif

  /*
  printf("p0 %g , t0 %g , w0 %g usePTW0 %d \n",p0,t0,w0,usePTW0);
  printf("PlowTop %g precool %d deltaP %g\n",PlowTop,pre_cool_type,deltaP);
  */
  
  /* Initialize outputs to sensible values before proceeding */
  /* Reference levels not found (yet), be positive, they will appear */
  *gotLCL=0;
  *gotLFC=0;
  *gotEL=0;
  *Olifted=0;
  *cape=MISSING_VALUE;
  *cin=MISSING_VALUE;

  dP=fabs(deltaP);
  Ptop=(pvalues[0]>pvalues[nlevels-1])?pvalues[nlevels-1]:pvalues[0];
  Plow=(pvalues[0]>pvalues[nlevels-1])?pvalues[0]:pvalues[nlevels-1];
  ilow=(pvalues[0]>pvalues[nlevels-1])?0:(nlevels-1);

#ifdef MYDEBUG
  {
    FILE *ofile=fopen("capecin.log","a");
    fprintf(ofile,
	    "# Input: %11.5f Pa, %11.5f K, %11.5f g/kg PlowTop %9.5f precool %d usePTW0 %d\n",
	    p0,t0,w0*1000,PlowTop,pre_cool_type,usePTW0);
    fclose(ofile);
  }
#endif

  /* printf("Got upToTop==%d\n",upToTop);*/

  /* Starting conditions come from calling routines */
  /* printf("%g %g %g %d\n",p0,t0,w0,usePTW0); */
  if (usePTW0){
    /* You seem to know what you want */
    if ((Plow<p0) || (Plow < PlowTop) || (p0<Ptop) ){
      /* 
	 BUT ... You are outside the sounding, your initial conditions are 
	 not reasonable 
      */
      return 1;
    }else{
      /* you know your starting point and seems OK */
      start.p=p0;
      start.t=t0;
      start.w=w0;
      setParcelState(&start);
    }
  } else {
    /* This is probably the expected case for unknown initial conditions,
       perform a vertical average */
    if (Plow>PlowTop){
      /* start.p=(Plow+PlowTop)/2.; */
      getBottomAverage(&(start.p),&(start.t),&(start.w),pvalues,Tvalues,
		       wvalues,nlevels,PlowTop,Plow);
    } else {
      /* Hummmm just take the lowest point but this seems risky */
      start.p=pvalues[ilow];
      start.t=Tvalues[ilow];
      start.w=wvalues[ilow];
    }
      setParcelState(&start);
  }
  /* This will go back to caller, save the data in memory into the passed
     structure */
  memcpy(apstart,&start,sizeof(AirParcel));

#ifdef MYDEBUG
  {
    FILE *ofile=fopen("capecin.log","a");
    fprintf(ofile,
	    "# Start values: %11.5f Pa, %11.5f K, %11.5f g/kg %9.5g g/kg\n",
	    start.p,start.t,start.w*1000,
	    saturation_mixing_ratio(start.p,start.t)
	    );
    fclose(ofile);
  }
#endif

  /* Initialize ambient parcel at the beginning state */
  /* printf("start.p %g %g %g\n",start.p,pvalues[0],pvalues[1]); */
  environment_data(&ambient,pvalues,Tvalues,wvalues,nlevels,
		   &OK,start.p,doLog);
  if(OK!=1){
    *cape=MISSING_VALUE;
    *cin=MISSING_VALUE;
    return 2;
  }
  
  /* 
     We must pre-cool the starting parcel. It might be too hot for its
     original level, due to the vertical averaging and, in this case,
     CIN is never computed. Only if the use of fixed initial
     conditions is NOT enforced and only if ambient cooler than lifted
  */
  if ((!usePTW0) || (Plow>PlowTop)){
    /************ Adiabatic precooling *******/
    if (pre_cool_type==ADIABATIC_PRE_COOL){
      /* Cycle adiabatically upwards */
      while ((ambient.p>=Ptop) && (start.t>ambient.t)){
	/* These two must go hand in hand, since the particle lifts and 
	   the ambient temperature might get cooler */
	adiabatic_evolution(&start,dP,0);
	environment_data(&ambient,pvalues,Tvalues,wvalues,nlevels,
			 &OK,start.p,doLog);
	if (!OK){
	  break;
	}
      }
      /* Already cooled, but check out code from interpolations for ambient */
      if (!OK){
	*cape=MISSING_VALUE;
	*cin=MISSING_VALUE;
	return 3;
      }
    }
    /* End of adiabatic precooling block */
    
    /* Second option for precooling (preconditioning the air parcel) */
    if (pre_cool_type==ISOBARIC_PRE_COOL){
      /* Correct moisture in this case, keep original w at the sounding */
      environment_data(&start,pvalues,Tvalues,wvalues,nlevels,&OK,start.p,
		       doLog);
      if (!OK){
	*cape=MISSING_VALUE;
	*cin=MISSING_VALUE;
	return 4;
      }      
      /* Proper environment mix ratio here */
      environment_data(&ambient,pvalues,Tvalues,wvalues,nlevels,&OK,start.p,
		       doLog);
      if (!OK){
	return 5;
      }      
    }
    /* End of isobaric precooling */
  } /* End of precooling */
#ifdef MYDEBUG
  {
    FILE *ofile=fopen("capecin.log","a");
    fprintf(ofile,	    
	    "# Parcel after precool: %11.5f Pa, %11.5f K, %11.5f g/kg %9.5g g/kg\n",
	    start.p,start.t,start.w*1000,
    	    saturation_mixing_ratio(start.p,start.t)
	    );
    fprintf(ofile,
	    "# Ambient after precool: %11.5f Pa, %11.5f K, %11.5f g/kg %9.5g g/kg\n",
	    ambient.p,ambient.t,ambient.w*1000,
    	    saturation_mixing_ratio(start.p,start.t)
	    );
    fclose(ofile);
  }
#endif
  
  /* If needed, store to return profile of lifted */
  *Olifted=save2lifted(&start,Plifted,Tlifted,wlifted,Nlifted,Olifted,__LINE__);

  /*

    We should by now be ready to get CIN. Copy the values of the starting
    (precooled) point to the base points and initialize the lifted "upper
    parcel"
    This means that start becomes the real point from which CAPE/CIN
    is computed.

  */
  
  /* Bottom part of the segment copied from current data */
  memcpy((void*) (&lb),(void*) (&start),sizeof(AirParcel));
  memcpy((void*) (&ab),(void*) (&ambient),sizeof(AirParcel));

  /* 
     Initialize the "upper part of the segment" for the lifted parcel.
     1. Copy data from bottom first, copy all bits
     2. set values of P/T/w from an adiabatic ascent
  */
  memcpy((void*) (&lu),(void*) (&start),sizeof(AirParcel));
  adiabatic_evolution(&lu,dP,0);

  /* Find ambient data at the top */
  environment_data(&au,pvalues,Tvalues,wvalues,nlevels,&OK,lu.p,doLog);
  if (!OK){
    *cape=MISSING_VALUE;
    *cin=MISSING_VALUE;
    return 6;
  }
  checkLCL(gotLCL,apLCL,&lb,&lu,&ab,&au);

  /* Main CYCLE */
  /* Initialize values for holders and process the full sounding */
  *cin=0.0;
  *cape=0.0;
  negEtemp=0.0;
  while((lu.p>Ptop)){
    /* Store if needed */
    *Olifted=save2lifted(&lu,Plifted,Tlifted,wlifted,Nlifted,Olifted,__LINE__);
    /* Compute energy from slab */
    dE=energy_area(ab.p,ab.Tv,au.Tv,lb.Tv,lu.Tv,dP);
#ifdef MYDEBUG
    printf("dE,%g %g %g %g %g %g %g %g\n",dE,ab.p,ab.Tv,au.Tv,lb.Tv,lu.Tv,*cape,*cin);
#endif

    /* Assign energy of the slab depending on its sign */
    if (dE<0){
      /* For CIN and upToTop, accumulate into CIN only if it is
	 later buoyant somewhere. Otherwise, stability after
	 EL inflates CIN a lot
      */
      if(upToTop)
	negEtemp+=dE;
      else{
	if (*gotLFC==0)
	  *cin+=dE;
      }
    }else{
      *cape+=dE;
    }
    /* Get LFC from buoyancy */
    if (isBuoyant(&lu,&au)){
      if(! *gotLFC){
	*gotLFC=1;
	/* Record LFC for output */
	Pmiddle=(ab.p+au.p)/2.;
	environment_data(apLFC,
			 pvalues,Tvalues,wvalues,nlevels,&OK,Pmiddle,doLog);
	if (!OK){
	  *cape=MISSING_VALUE;
	  *cin=MISSING_VALUE;
	  return 7;
	}
	setParcelState(apLFC);
      }
      /* 
	 If some negative energy remains from previous steps
	 and now the parcel is again buoyant, pass it to CIN
      */
      if(upToTop){
	*cin+=negEtemp;
	negEtemp=0.0;
      }
    }
    /* Find EL from sounding crossing */
    if (isR2LCrossing(&ab,&au,&lb,&lu )){
      if ((upToTop)||(*gotEL==0)){
	/* Record always the last EL if upToTop*/
	*gotEL=1;
	Pmiddle=(ab.p+au.p)/2.;
	environment_data(apEL,
			 pvalues,Tvalues,wvalues,nlevels,&OK,Pmiddle,doLog);
	if (!OK){
	  *cape=MISSING_VALUE;
	  *cin=MISSING_VALUE;
	  return 7;
	}
	setParcelState(apEL);
      }	  
    }
    /* If not requested to top but gotEL, it's finished */
    if ((*gotEL) && (!upToTop)){
      break;
    }
    /* next step UP, limiting access to top level */
    nextP=lu.p-deltaP;
    if(nextP<Ptop){
      nextP=Ptop;
    }
    
    /* Find next parcels, first, UP parcels stored as bottom */
    memcpy((void*)(&ab),(void*)(&au),sizeof(AirParcel));
    memcpy((void*)(&lb),(void*)(&lu),sizeof(AirParcel));
    
    /* Bottom lifted to next lifted by an adiabatic evolution, ensure to Ptop */
    adiabatic_evolution(&lu,(lu.p-nextP),0);
    /* New environment from sounding at new upper */
    environment_data(&au,pvalues,Tvalues,wvalues,nlevels,&OK,lu.p,doLog);
    if (!OK){
      *cape=MISSING_VALUE;
      *cin=MISSING_VALUE;
      return 7;
    }
    /* LCL is always checked */
    checkLCL(gotLCL,apLCL,&lb,&lu,&ab,&au);
    /* This must always happen for pressures, but check just in case */
    if ((lb.p!=ab.p)||(lu.p!=au.p))
      return 8;
  }
  /* End of the main CYCLE */
  return 0;
}

