/*!
 * Copyright 2018 tsooBGX contributors
 */

// Dummy file to keep the CUDA conditional compile trick.

#include <dmlc/registry.h>
namespace tsoobgx {
namespace obj {

DMLC_REGISTRY_FILE_TAG(regression_obj);

}  // namespace obj
}  // namespace tsoobgx

#ifndef TSOOBGX_USE_CUDA
#include "regression_obj.cu"
#endif  // TSOOBGX_USE_CUDA
