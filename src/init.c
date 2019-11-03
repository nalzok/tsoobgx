/* Copyright (c) 2015 by Contributors
 *
 * This file was initially generated using the following R command:
 * tools::package_native_routine_registration_skeleton('.', con = 'src/init.c', character_only = F)
 * and edited to conform to tsoobgx C linter requirements. For details, see
 * https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Registering-native-routines
 */
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP retsooBGXBoostOneIter_R(SEXP, SEXP, SEXP, SEXP);
extern SEXP retsooBGXCreate_R(SEXP);
extern SEXP retsooBGXDumpModel_R(SEXP, SEXP, SEXP, SEXP);
extern SEXP retsooBGXEvalOneIter_R(SEXP, SEXP, SEXP, SEXP);
extern SEXP retsooBGXGetAttrNames_R(SEXP);
extern SEXP retsooBGXGetAttr_R(SEXP, SEXP);
extern SEXP retsooBGXLoadModelFromRaw_R(SEXP, SEXP);
extern SEXP retsooBGXLoadModel_R(SEXP, SEXP);
extern SEXP retsooBGXModelToRaw_R(SEXP);
extern SEXP retsooBGXPredict_R(SEXP, SEXP, SEXP, SEXP);
extern SEXP retsooBGXSaveModel_R(SEXP, SEXP);
extern SEXP retsooBGXSetAttr_R(SEXP, SEXP, SEXP);
extern SEXP retsooBGXSetParam_R(SEXP, SEXP, SEXP);
extern SEXP retsooBGXUpdateOneIter_R(SEXP, SEXP, SEXP);
extern SEXP XGCheckNullPtr_R(SEXP);
extern SEXP XGDMatrixCreateFromCSC_R(SEXP, SEXP, SEXP, SEXP);
extern SEXP XGDMatrixCreateFromFile_R(SEXP, SEXP);
extern SEXP XGDMatrixCreateFromMat_R(SEXP, SEXP);
extern SEXP XGDMatrixGetInfo_R(SEXP, SEXP);
extern SEXP XGDMatrixNumCol_R(SEXP);
extern SEXP XGDMatrixNumRow_R(SEXP);
extern SEXP XGDMatrixSaveBinary_R(SEXP, SEXP, SEXP);
extern SEXP XGDMatrixSetInfo_R(SEXP, SEXP, SEXP);
extern SEXP XGDMatrixSliceDMatrix_R(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"retsooBGXBoostOneIter_R",     (DL_FUNC) &retsooBGXBoostOneIter_R,     4},
  {"retsooBGXCreate_R",           (DL_FUNC) &retsooBGXCreate_R,           1},
  {"retsooBGXDumpModel_R",        (DL_FUNC) &retsooBGXDumpModel_R,        4},
  {"retsooBGXEvalOneIter_R",      (DL_FUNC) &retsooBGXEvalOneIter_R,      4},
  {"retsooBGXGetAttrNames_R",     (DL_FUNC) &retsooBGXGetAttrNames_R,     1},
  {"retsooBGXGetAttr_R",          (DL_FUNC) &retsooBGXGetAttr_R,          2},
  {"retsooBGXLoadModelFromRaw_R", (DL_FUNC) &retsooBGXLoadModelFromRaw_R, 2},
  {"retsooBGXLoadModel_R",        (DL_FUNC) &retsooBGXLoadModel_R,        2},
  {"retsooBGXModelToRaw_R",       (DL_FUNC) &retsooBGXModelToRaw_R,       1},
  {"retsooBGXPredict_R",          (DL_FUNC) &retsooBGXPredict_R,          4},
  {"retsooBGXSaveModel_R",        (DL_FUNC) &retsooBGXSaveModel_R,        2},
  {"retsooBGXSetAttr_R",          (DL_FUNC) &retsooBGXSetAttr_R,          3},
  {"retsooBGXSetParam_R",         (DL_FUNC) &retsooBGXSetParam_R,         3},
  {"retsooBGXUpdateOneIter_R",    (DL_FUNC) &retsooBGXUpdateOneIter_R,    3},
  {"XGCheckNullPtr_R",            (DL_FUNC) &XGCheckNullPtr_R,            1},
  {"XGDMatrixCreateFromCSC_R",    (DL_FUNC) &XGDMatrixCreateFromCSC_R,    4},
  {"XGDMatrixCreateFromFile_R",   (DL_FUNC) &XGDMatrixCreateFromFile_R,   2},
  {"XGDMatrixCreateFromMat_R",    (DL_FUNC) &XGDMatrixCreateFromMat_R,    2},
  {"XGDMatrixGetInfo_R",          (DL_FUNC) &XGDMatrixGetInfo_R,          2},
  {"XGDMatrixNumCol_R",           (DL_FUNC) &XGDMatrixNumCol_R,           1},
  {"XGDMatrixNumRow_R",           (DL_FUNC) &XGDMatrixNumRow_R,           1},
  {"XGDMatrixSaveBinary_R",       (DL_FUNC) &XGDMatrixSaveBinary_R,       3},
  {"XGDMatrixSetInfo_R",          (DL_FUNC) &XGDMatrixSetInfo_R,          3},
  {"XGDMatrixSliceDMatrix_R",     (DL_FUNC) &XGDMatrixSliceDMatrix_R,     2},
  {NULL, NULL, 0}
};

#if defined(_WIN32)
__declspec(dllexport)
#endif  // defined(_WIN32)
void R_init_tsoobgx(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
