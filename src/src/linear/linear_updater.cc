/*!
 * Copyright 2018
 */
#include <tsoobgx/linear_updater.h>
#include <dmlc/registry.h>
#include "./param.h"

namespace dmlc {
DMLC_REGISTRY_ENABLE(::tsoobgx::LinearUpdaterReg);
}  // namespace dmlc

namespace tsoobgx {

LinearUpdater* LinearUpdater::Create(const std::string& name) {
  auto *e = ::dmlc::Registry< ::tsoobgx::LinearUpdaterReg>::Get()->Find(name);
  if (e == nullptr) {
    LOG(FATAL) << "Unknown linear updater " << name;
  }
  return (e->body)();
}

}  // namespace tsoobgx

namespace tsoobgx {
namespace linear {
DMLC_REGISTER_PARAMETER(LinearTrainParam);

// List of files that will be force linked in static links.
DMLC_REGISTRY_LINK_TAG(updater_shotgun);
DMLC_REGISTRY_LINK_TAG(updater_coordinate);
#ifdef TSOOBGX_USE_CUDA
DMLC_REGISTRY_LINK_TAG(updater_gpu_coordinate);
#endif  // TSOOBGX_USE_CUDA
}  // namespace linear
}  // namespace tsoobgx
