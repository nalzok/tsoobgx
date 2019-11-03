/*!
 * Copyright 2018 tsooBGX contributors
 */

// Dummy file to keep the CUDA conditional compile trick.

#include <dmlc/registry.h>
namespace tsoobgx {
namespace obj {

DMLC_REGISTRY_FILE_TAG(hinge_obj);

}  // namespace obj
}  // namespace tsoobgx

#ifndef TSOOBGX_USE_CUDA
#include "hinge.cu"
#endif  // TSOOBGX_USE_CUDA
