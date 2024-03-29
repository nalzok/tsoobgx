/*!
 * Copyright 2018 tsooBGX contributors
 */
#include "common.h"

namespace tsoobgx {

int AllVisibleImpl::AllVisible() {
  int n_visgpus = 0;
  try {
    // When compiled with CUDA but running on CPU only device,
    // cudaGetDeviceCount will fail.
    dh::safe_cuda(cudaGetDeviceCount(&n_visgpus));
  } catch(const dmlc::Error &except) {
    return 0;
  }
  return n_visgpus;
}

}  // namespace tsoobgx
