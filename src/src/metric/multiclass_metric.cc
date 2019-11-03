/*!
 * Copyright 2019 tsooBGX contributors
 */
// Dummy file to keep the CUDA conditional compile trick.

#if !defined(TSOOBGX_USE_CUDA)
#include "multiclass_metric.cu"
#endif  // !defined(TSOOBGX_USE_CUDA)
