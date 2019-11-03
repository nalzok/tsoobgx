/*!
 * Copyright 2018 tsooBGX contributors
 */
// Dummy file to keep the CUDA conditional compile trick.

#if !defined(TSOOBGX_USE_CUDA)
#include "elementwise_metric.cu"
#endif  // !defined(TSOOBGX_USE_CUDA)
