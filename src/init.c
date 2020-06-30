#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP Cdist(SEXP, SEXP, SEXP, SEXP);
extern SEXP pKolmogorov2x(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"Cdist",         (DL_FUNC) &Cdist,         4},
    {"pKolmogorov2x", (DL_FUNC) &pKolmogorov2x, 2},
    {NULL, NULL, 0}
};

void R_init_BoutrosLab_plotting_general(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
