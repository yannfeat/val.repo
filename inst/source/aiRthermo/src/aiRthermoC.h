/*

aiRthermoC.h

Thermodynamical diagnostics for atmospheric vertical soundings

Copyright (C) 2017, Jon Saenz, Sheila Carreno and Santi Gonzalez-Roji

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation, version 2.
 
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifndef __aiRthermoC__
#define __aiRthermoC__ 1

/* Bohren, Albrecht (2000), pages 197-200, return es in Pa */
double saturation_pressure_over_ice( double Tkelvin );

/* Bohren, Albrecht (2000), pages 197-200, return es in Pa */
double saturation_pressure_over_water( double Tkelvin );

/* Buck's eq over 30 C, return es in Pa */
double saturation_pressure_over_water_over30C( double Tkelvin );

/******
Saturation pressure over water or ice, depending on the 
value of temperature
******/
double saturation_pressure_H2O( double Tkelvin );
void saturation_pressure_H2O_Rworld( double *Ts , int *nelems,double *result);

/* 
cp for moist air, input: mixing ratio (kg/kg) 
*/
double moistCp( double w );
/* 
cv for moist air, input: mixing ratio (kg/kg) 
*/
double moistCv( double w );

/*
  L is considered over water if T>0C
  L is considered over ice if T < Tice C
  L is linearly combined between water and ice in the interval [Tice,0] C
*/
double latent_heat_H2O( double T );

/* 
Saturation mixing ratio from pressure and T (K)
See Wallace and Hobbs
TeX: \[
TeX: w_s=\frac{R_d}{R_v}\frac{e_s}{p-e_s}
TeX: \]
*/
double saturation_mixing_ratio(double P, double Tkelvin);

/*
From relative humidity (%), Pressure (Pa) and temperature (K), 
get mixing ratio
TeX: $w=\frac{w_s rh}{100}$
*/
double rh2w( double rh , double P, double T);

/*
From relative humidity (%), Pressure (Pa) and temperature (K), 
get specific humidity
TeX: $q=\frac{w}{1+w}$ after having computed w from rh
*/
double rh2shum( double rh , double P, double T);

/* from T and Td, get dew point depression */
double TTd2dpd( double T , double Td );

/* From dew point depression and T, get Td */
double dpdT2Td( double dpd , double T );

/*
From the dew point temperature (K), pressure (Pa) and
temperature (K), get relative humidity
TeX: $Td=T-DPD$ and $rh=100\frac{w_s(P,T_d)}{w_s(P,T)}$
*/
double Td2rh( double Td, double P, double T );

/*
From the dew point depression (K), pressure (Pa) and
temperature (K), get relative humidity
TeX: $Td=T-DPD$ and $rh=100\frac{w_s(P,T_d)}{w_s(P,T)}$
*/
double dpd2rh(double dpd, double P, double T);

/*
From specific humidity (kg/kg) and pressure (Pa), get partial 
pressure (Pa) of water vapour
TeX: $e=\frac{qp}{\varepsilon(1-q)+q}
*/
double q2e(double q , double P);

/*
From mixing ration to specific humidity
TeX: $q=\frac{w}{w+1}$
*/
double w2q(double w);

/*
From specific humidity to mixing ratio
TeX: $w=\frac{q}{1-q}$
*/
double q2w(double q);

/*
From T(K), w(kg/kg) and P (Pa), get virtual temperature
TeX: $T_v=\frac{T}{1-(e/P)(1-\varepsilon)}$
*/
double virtual_temperature( double T, double w , double p);

/*
Density from P and T (if Tv given, for moist air too)
TeX: $\rho=\frac{P}{R_d T}$
*/
double density( double P, double T);
double densityH2O( double Pw, double T);

/*
In the inversion of es -> T, use the 
approximate expression 5.68 in Bohren
*/
double w2Td( double w , double P );

/*
Conversion between temperature scales, not really used inside the C 
part, but it doesn't hurt anyway
*/
double celsius2kelvin( double tc );
double kelvin2celsius( double tk );
double F2celsius( double Tf );
double celsius2F( double Tc );
double F2kelvin( double Tf );
double kelvin2F( double Tk );


/*
TeX: $\theta=T\left( \frac{P_0}{P}\right)^\frac{R_d}{c_p(w)}
*/
double theta( double T, double P , double w );

/*
TeX: $T=\theta\left( \frac{P}{P_0}\right)^\frac{R_d}{c_p(w)}
*/
double thetaP2T( double theta, double P , double w );

/*
TeX: $P=P_0\left( \frac{T}{\theta}\right)^\frac{c_p(w)}{R_d}
*/
double thetaT2P( double theta, double P , double w );

/*
  From a parcel at pressure p (Pa), temperature t (K), and mixing ratio
  w (kg/kg), make an adiabatic ascent (dry) until
  the parcel is saturated (LCL). Then, return the values of plcl (Pa),
  temperature at the LCL tlcl (K), (saturated) mixing ratio at LCL wlcl
  (kg/kg) and potential temperature at LCL theta_lcl (K). Ptop (Pa) is
  the maximum pressure that the adiabatic ascent proceeds in order to find 
  the LCL (top of the sounding is a good guess). It returns 1 (found LCL) 
  or 0 (not found LCL when arrived to Ptop).  deltaP (Pa) reflects the step 
  used in the individual vertical ascents.
*/
int find_lcl( double Ptop,double p , double t, double w ,
		     double *plcl , double *tlcl , double *wlcl ,
		     double *theta_lcl , double deltaP );

/*
  Unsaturated adiabatic profile in T (K), P (Pa) coordinates, see
  page 116, eq 7.71 in Tsonis, An introduction to atmospheric
  thermodynamics. It has been modified considering that the environment is
  in hydrostatic equilibrium to dT/dP instead of dT/dz by myself.
*/
double gamma_s_P( double P , double T );

/*
  Saturated adiabatic profile in T (K), P (Pa) coordinates, see
  page 113, eq 7.29 in Tsonis, An introduction to atmospheric
  thermodynamics. It has been modified considering that the environment is
  in hydrostatic equilibrium to dT/dP instead of dT/dz by myself.
*/
double gamma_d_P( double P , double T , double w );


/*
  A saturated parcel at p0 (Pa), t0 (K) undergoes a saturated 
  adiabatic evolution to a final point at pend (Pa). 
  Returns the temperature (K) of the parcel after the saturated evolution.
  deltaP (Pa) reflects the step  used in the individual vertical 
  evolutions.
*/
double t_after_saturated_adiabat(double p0, double t0,
				 double pend , double deltaP );
double t_after_saturated_adiabat_down(double p0, double t0,
				 double pend , double deltaP );

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
				int *out_of_range, int doItLog);

/* 
For a sounding provided in arrays of pressure Pvalues[nlevels] (Pa),
temperature in Tvalues[nlevels] (K) and mix ratio Wvalues[nlevels] (kg/kg),
return the value of the K index of instability using linear (doLog=0)
or logarithmic (doLog=1) interpolation. Since levels in K index are mandatory,
they are probably included in the original sounding and the use
of doLog usually doesn't affect the results
*/
double kindex(double *Pvalues, double *Tvalues,double *wvalues,
		     int nlevels ,int doLog );
void kindex_Rworld( double *Ps , double *Ts , 
                    double *ws , int *nelems ,
		    int *doLog , double *result ) ;

/* 
For a sounding provided in arrays of pressure Pvalues[nlevels] (Pa),
temperature in Tvalues[nlevels] (K) and mix ratio Wvalues[nlevels] (kg/kg),
return the value of the Total-Totals index of instability using linear (doLog=0)
or logarithmic (doLog=1) interpolation. Since levels in Total-Totals 
index are mandatory, they are probably included in the original sounding 
and the use of doLog usually doesn't affect the results
*/
double TTindex(double *Pvalues, double *Tvalues, double *wvalues, 
		      int nlevels,int doLog);

/* 
Given an array of values X[NP] at isobaric levels P[NP],, return a vertical
average of X from PSurface (Pa) to Pt (Pa), performing vertical integrations
by accumulating slabs
\[
\overbar{X}=\frac{R_d}{g\Delta Z}\int_{P_t}^{P_{SFC}} \frac{X T_v}{P} dP
\]
whichvar switches some special behaviours for moisture (w) or density
*/
double vertical_average(double *x,double Pt , double *P ,
			double *T , double *ws , int NP , int whichVar );

/* 
For a sounding provided in arrays of pressure Pvalues[nlevels] (Pa),
temperature in Tvalues[nlevels] (K) and mix ratio Wvalues[nlevels] (kg/kg),
return the value of the Showalter index of instability using linear (doLog=0)
or logarithmic (doLog=1) interpolation. Since top levels used in 
Showalter index are mandatory, they are probably included in the original 
sounding and the use of doLog usually doesn't affect the results
*/
double Sindex(double *Pvalues, double *Tvalues, double *wvalues,
	      int nlevels , int doLog , double deltaP );
/* 
For a sounding provided in arrays of pressure Pvalues[nlevels] (Pa),
temperature in Tvalues[nlevels] (K) and mix ratio Wvalues[nlevels] (kg/kg),
return the value of the Lifted index of instability using linear (doLog=0)
or logarithmic (doLog=1) interpolation. Since top levels used in 
Lifted index are mandatory, they are probably included in the original 
sounding and the use of doLog usually doesn't affect the results.
deltaP (Pa) reflects the step  used in the individual vertical 
ascents (must be +). PWIDTH (Pa, >0) is the width of the troposphere
that must be averaged in the low levels, 5000 or 10000 Pa are good values.
*/
double LIindex(double *Pvalues, double *Tvalues, double *wvalues,
	       int nlevels,double Psurface,int doLog,double deltaP,
	       double PWIDTH);

/*
Compute the energy of a slab with low pressure pb (Pa) and
thickness deltaP (Pa) limited by ambient (bottom tvab|top tvau) 
virtual temperature (K) and lifted parcel described by
(bottom tvlb|top tvlu) virtual temperatures (K)
*/
double energy_area( double pb, 
				  double tvab , double tvau,
		    double tvlb, double tvlu , double deltaP );

/*
Full thermodynamical properties of an Air Parcel, helpful in the
evaluation of vertical evolutions during CAPE/CIN computations
*/
typedef struct{
  double p;
  double t;
  double w;
  double theta;
  double Tv;
  double wsat;
} AirParcel, *AirParcelPtr;

void setParcelState( AirParcelPtr ap );

/* 
Get the value for the environment of the parcel
ap that is lifting accross the sounding
given by pvalues[nlevels] (Pa), Tvalues[nlevels] (K)
and wvalues[nlevels] (kg/kg), at pressure pto ("pto=Pressure going TO"). 
If the computation  performs properly, OK is assigned 1.
The vertical interpolation is logarithmic if doLog==1.
*/
void environment_data( AirParcelPtr ap , double *pvalues,
				     double *Tvalues, double *wvalues,
				     int nlevels , int *OK , double pto,
		       int doLog );

/*
Get values of the air parcel ap after a change in pressure deltaP (Pa).
This function knows whether dry or saturated adiabatic must be used.
*/
void adiabatic_evolution( AirParcelPtr ap , double deltaP , int goDown );

/* 
   A parcel at Pstart (Pa), Tstart (K) and wstart mixing ratio (kg/kg)
   adiabatically ascends to Pend (Pa) under hydrostatic equilibrium
   and keeping w constant until saturation and saturated afterwards
*/
void adiabatic_ascent( double Pstart , double Tstart , double wstart ,
		       double Pend , double *Tend , double *wend ,
		       double deltaP );
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
			 double wcstart , double pend, double dP  ,
			 double *tend, double *wend , double *wcend );


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
and so on).  The evolution of the lifted particle
is returned in Plifted[Nlifted], Tlifted[Nlifted] and wlifted[nLIFTED]
with *Olifted holding the real number of levels on output.
*/
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
		double *wlifted,
		int Nlifted ,
                int *Olifted,
		int upToTop,
		int checkBouyancy
		);

#endif
