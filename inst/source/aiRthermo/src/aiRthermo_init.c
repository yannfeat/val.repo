#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void adiabatic_ascent_Rworld(void *, void *, void *, void *, void *, void *, void *);
extern void any_adiabatic_down_Rworld(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void CAPE_CIN_Rworld(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void * );
extern void export_constants_Rworld(void *);
extern void find_lcl_Rworld(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void gamma_sat_P_Rworld(void *, void *, void *, void *);
extern void kindex_Rworld(void *, void *, void *, void *, void *, void *);
extern void latent_heat_H2O_Rworld(void *, void *, void *);
extern void LIindex_Rworld(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void saturation_pressure_H2O_Rworld(void *, void *, void *);
extern void Sindex_Rworld(void *, void *, void *, void *, void *, void *, void *);
extern void TTindex_Rworld(void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"adiabatic_ascent_Rworld",        (DL_FUNC) &adiabatic_ascent_Rworld,         7},
    {"any_adiabatic_down_Rworld",      (DL_FUNC) &any_adiabatic_down_Rworld,       9},
    {"CAPE_CIN_Rworld",                (DL_FUNC) &CAPE_CIN_Rworld,                29},
    {"export_constants_Rworld",        (DL_FUNC) &export_constants_Rworld,         1},
    {"find_lcl_Rworld",                (DL_FUNC) &find_lcl_Rworld,                10},
    {"gamma_sat_P_Rworld",             (DL_FUNC) &gamma_sat_P_Rworld,              4},
    {"kindex_Rworld",                  (DL_FUNC) &kindex_Rworld,                   6},
    {"latent_heat_H2O_Rworld",         (DL_FUNC) &latent_heat_H2O_Rworld,          3},
    {"LIindex_Rworld",                 (DL_FUNC) &LIindex_Rworld,                  9},
    {"saturation_pressure_H2O_Rworld", (DL_FUNC) &saturation_pressure_H2O_Rworld,  3},
    {"Sindex_Rworld",                  (DL_FUNC) &Sindex_Rworld,                   7},
    {"TTindex_Rworld",                 (DL_FUNC) &TTindex_Rworld,                  6},
    {NULL, NULL, 0}
};

void R_init_aiRthermo(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
