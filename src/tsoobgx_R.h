/*!
 * Copyright 2014 (c) by Contributors
 * \file tsoobgx_R.h
 * \author Tianqi Chen
 * \brief R wrapper of tsoobgx
 */
#ifndef TSOOBGX_R_H_ // NOLINT(*)
#define TSOOBGX_R_H_ // NOLINT(*)


#include <Rinternals.h>
#include <R_ext/Random.h>
#include <Rmath.h>

#include <tsoobgx/c_api.h>

/*!
 * \brief check whether a handle is NULL
 * \param handle
 * \return whether it is null ptr
 */
BGX_DLL SEXP XGCheckNullPtr_R(SEXP handle);

/*!
 * \brief load a data matrix
 * \param fname name of the content
 * \param silent whether print messages
 * \return a loaded data matrix
 */
BGX_DLL SEXP XGDMatrixCreateFromFile_R(SEXP fname, SEXP silent);

/*!
 * \brief create matrix content from dense matrix
 * This assumes the matrix is stored in column major format
 * \param data R Matrix object
 * \param missing which value to represent missing value
 * \return created dmatrix
 */
BGX_DLL SEXP XGDMatrixCreateFromMat_R(SEXP mat,
                                      SEXP missing);
/*!
 * \brief create a matrix content from CSC format
 * \param indptr pointer to column headers
 * \param indices row indices
 * \param data content of the data
 * \param num_row numer of rows (when it's set to 0, then guess from data)
 * \return created dmatrix
 */
BGX_DLL SEXP XGDMatrixCreateFromCSC_R(SEXP indptr,
                                      SEXP indices,
                                      SEXP data,
                                      SEXP num_row);

/*!
 * \brief create a new dmatrix from sliced content of existing matrix
 * \param handle instance of data matrix to be sliced
 * \param idxset index set
 * \return a sliced new matrix
 */
BGX_DLL SEXP XGDMatrixSliceDMatrix_R(SEXP handle, SEXP idxset);

/*!
 * \brief load a data matrix into binary file
 * \param handle a instance of data matrix
 * \param fname file name
 * \param silent print statistics when saving
 * \return R_NilValue
 */
BGX_DLL SEXP XGDMatrixSaveBinary_R(SEXP handle, SEXP fname, SEXP silent);

/*!
 * \brief set information to dmatrix
 * \param handle a instance of data matrix
 * \param field field name, can be label, weight
 * \param array pointer to float vector
 * \return R_NilValue
 */
BGX_DLL SEXP XGDMatrixSetInfo_R(SEXP handle, SEXP field, SEXP array);

/*!
 * \brief get info vector from matrix
 * \param handle a instance of data matrix
 * \param field field name
 * \return info vector
 */
BGX_DLL SEXP XGDMatrixGetInfo_R(SEXP handle, SEXP field);

/*!
 * \brief return number of rows
 * \param handle an instance of data matrix
 */
BGX_DLL SEXP XGDMatrixNumRow_R(SEXP handle);

/*!
 * \brief return number of columns
 * \param handle an instance of data matrix
 */
BGX_DLL SEXP XGDMatrixNumCol_R(SEXP handle);

/*!
 * \brief create tsoobgx learner
 * \param dmats a list of dmatrix handles that will be cached
 */
BGX_DLL SEXP retsooBGXCreate_R(SEXP dmats);

/*!
 * \brief set parameters
 * \param handle handle
 * \param name  parameter name
 * \param val value of parameter
 * \return R_NilValue
 */
BGX_DLL SEXP retsooBGXSetParam_R(SEXP handle, SEXP name, SEXP val);

/*!
 * \brief update the model in one round using dtrain
 * \param handle handle
 * \param iter current iteration rounds
 * \param dtrain training data
 * \return R_NilValue
 */
BGX_DLL SEXP retsooBGXUpdateOneIter_R(SEXP ext, SEXP iter, SEXP dtrain);

/*!
 * \brief update the model, by directly specify gradient and second order gradient,
 *        this can be used to replace UpdateOneIter, to support customized loss function
 * \param handle handle
 * \param dtrain training data
 * \param grad gradient statistics
 * \param hess second order gradient statistics
 * \return R_NilValue
 */
BGX_DLL SEXP retsooBGXBoostOneIter_R(SEXP handle, SEXP dtrain, SEXP grad, SEXP hess);

/*!
 * \brief get evaluation statistics for tsoobgx
 * \param handle handle
 * \param iter current iteration rounds
 * \param dmats list of handles to dmatrices
 * \param evname name of evaluation
 * \return the string containing evaluation stats
 */
BGX_DLL SEXP retsooBGXEvalOneIter_R(SEXP handle, SEXP iter, SEXP dmats, SEXP evnames);

/*!
 * \brief make prediction based on dmat
 * \param handle handle
 * \param dmat data matrix
 * \param option_mask output_margin:1 predict_leaf:2
 * \param ntree_limit limit number of trees used in prediction
 */
BGX_DLL SEXP retsooBGXPredict_R(SEXP handle, SEXP dmat, SEXP option_mask, SEXP ntree_limit);
/*!
 * \brief load model from existing file
 * \param handle handle
 * \param fname file name
 * \return R_NilValue
 */
BGX_DLL SEXP retsooBGXLoadModel_R(SEXP handle, SEXP fname);

/*!
 * \brief save model into existing file
 * \param handle handle
 * \param fname file name
 * \return R_NilValue
 */
BGX_DLL SEXP retsooBGXSaveModel_R(SEXP handle, SEXP fname);

/*!
 * \brief load model from raw array
 * \param handle handle
 * \return R_NilValue
 */
BGX_DLL SEXP retsooBGXLoadModelFromRaw_R(SEXP handle, SEXP raw);

/*!
 * \brief load individual model from raw array
 * \param handle handle
 * \return R_NilValue
 */
BGX_DLL SEXP retsooBGXLoadIndividualModelFromRaw_R(SEXP handle, SEXP raw, SEXP k);

/*!
 * \brief save model into R's raw array
 * \param handle handle
 * \return raw array
   */
BGX_DLL SEXP retsooBGXModelToRaw_R(SEXP handle);

/*!
 * \brief dump model into a string
 * \param handle handle
 * \param fmap  name to fmap can be empty string
 * \param with_stats whether dump statistics of splits
 * \param dump_format the format to dump the model in
 */
BGX_DLL SEXP retsooBGXDumpModel_R(SEXP handle, SEXP fmap, SEXP with_stats, SEXP dump_format);

/*!
 * \brief get learner attribute value
 * \param handle handle
 * \param name  attribute name
 * \return character containing attribute value
 */
BGX_DLL SEXP retsooBGXGetAttr_R(SEXP handle, SEXP name);

/*!
 * \brief set learner attribute value
 * \param handle handle
 * \param name  attribute name
 * \param val attribute value; NULL value would delete an attribute
 * \return R_NilValue
 */
BGX_DLL SEXP retsooBGXSetAttr_R(SEXP handle, SEXP name, SEXP val);

/*!
 * \brief get the names of learner attributes
 * \return string vector containing attribute names
 */
BGX_DLL SEXP retsooBGXGetAttrNames_R(SEXP handle);

#endif  // TSOOBGX_WRAPPER_R_H_ // NOLINT(*)
