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
extern SEXP tsooBGXerBoostOneIter_R(SEXP, SEXP, SEXP, SEXP);
extern SEXP tsooBGXerCreate_R(SEXP);
extern SEXP tsooBGXerDumpModel_R(SEXP, SEXP, SEXP, SEXP);
extern SEXP tsooBGXerEvalOneIter_R(SEXP, SEXP, SEXP, SEXP);
extern SEXP tsooBGXerGetAttrNames_R(SEXP);
extern SEXP tsooBGXerGetAttr_R(SEXP, SEXP);
extern SEXP tsooBGXerLoadModelFromRaw_R(SEXP, SEXP);
extern SEXP tsooBGXerLoadModel_R(SEXP, SEXP);
extern SEXP tsooBGXerModelToRaw_R(SEXP);
extern SEXP tsooBGXerPredict_R(SEXP, SEXP, SEXP, SEXP);
extern SEXP tsooBGXerSaveModel_R(SEXP, SEXP);
extern SEXP tsooBGXerSetAttr_R(SEXP, SEXP, SEXP);
extern SEXP tsooBGXerSetParam_R(SEXP, SEXP, SEXP);
extern SEXP tsooBGXerUpdateOneIter_R(SEXP, SEXP, SEXP);
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
  {"tsooBGXerBoostOneIter_R",     (DL_FUNC) &tsooBGXerBoostOneIter_R,     4},
  {"tsooBGXerCreate_R",           (DL_FUNC) &tsooBGXerCreate_R,           1},
  {"tsooBGXerDumpModel_R",        (DL_FUNC) &tsooBGXerDumpModel_R,        4},
  {"tsooBGXerEvalOneIter_R",      (DL_FUNC) &tsooBGXerEvalOneIter_R,      4},
  {"tsooBGXerGetAttrNames_R",     (DL_FUNC) &tsooBGXerGetAttrNames_R,     1},
  {"tsooBGXerGetAttr_R",          (DL_FUNC) &tsooBGXerGetAttr_R,          2},
  {"tsooBGXerLoadModelFromRaw_R", (DL_FUNC) &tsooBGXerLoadModelFromRaw_R, 2},
  {"tsooBGXerLoadModel_R",        (DL_FUNC) &tsooBGXerLoadModel_R,        2},
  {"tsooBGXerModelToRaw_R",       (DL_FUNC) &tsooBGXerModelToRaw_R,       1},
  {"tsooBGXerPredict_R",          (DL_FUNC) &tsooBGXerPredict_R,          4},
  {"tsooBGXerSaveModel_R",        (DL_FUNC) &tsooBGXerSaveModel_R,        2},
  {"tsooBGXerSetAttr_R",          (DL_FUNC) &tsooBGXerSetAttr_R,          3},
  {"tsooBGXerSetParam_R",         (DL_FUNC) &tsooBGXerSetParam_R,         3},
  {"tsooBGXerUpdateOneIter_R",    (DL_FUNC) &tsooBGXerUpdateOneIter_R,    3},
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
