/*!
 * Copyright 2015 by Contributors
 * \file gbm.cc
 * \brief Registry of gradient boosters.
 */
#include <tsoobgx/gbm.h>
#include <dmlc/registry.h>

namespace dmlc {
DMLC_REGISTRY_ENABLE(::tsoobgx::GradientBoosterReg);
}  // namespace dmlc

namespace tsoobgx {
GradientBooster* GradientBooster::Create(
    const std::string& name,
    const std::vector<std::shared_ptr<DMatrix> >& cache_mats,
    bst_float base_margin) {
  auto *e = ::dmlc::Registry< ::tsoobgx::GradientBoosterReg>::Get()->Find(name);
  if (e == nullptr) {
    LOG(FATAL) << "Unknown gbm type " << name;
  }
  return (e->body)(cache_mats, base_margin);
}

}  // namespace tsoobgx

namespace tsoobgx {
namespace gbm {
// List of files that will be force linked in static links.
DMLC_REGISTRY_LINK_TAG(gblinear);
DMLC_REGISTRY_LINK_TAG(gbtree);
}  // namespace gbm
}  // namespace tsoobgx
