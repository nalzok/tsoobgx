/*!
 * Copyright 2015 by Contributors
 * \file objective.cc
 * \brief Registry of all objective functions.
 */
#include <tsoobgx/objective.h>
#include <dmlc/registry.h>

#include "../common/host_device_vector.h"

namespace dmlc {
DMLC_REGISTRY_ENABLE(::tsoobgx::ObjFunctionReg);
}  // namespace dmlc

namespace tsoobgx {
// implement factory functions
ObjFunction* ObjFunction::Create(const std::string& name) {
  auto *e = ::dmlc::Registry< ::tsoobgx::ObjFunctionReg>::Get()->Find(name);
  if (e == nullptr) {
    for (const auto& entry : ::dmlc::Registry< ::tsoobgx::ObjFunctionReg>::List()) {
      LOG(INFO) << "Objective candidate: " << entry->name;
    }
    LOG(FATAL) << "Unknown objective function " << name;
  }
  return (e->body)();
}

}  // namespace tsoobgx

namespace tsoobgx {
namespace obj {
// List of files that will be force linked in static links.
#ifdef TSOOBGX_USE_CUDA
DMLC_REGISTRY_LINK_TAG(regression_obj_gpu);
DMLC_REGISTRY_LINK_TAG(hinge_obj_gpu);
DMLC_REGISTRY_LINK_TAG(multiclass_obj_gpu);
#else
DMLC_REGISTRY_LINK_TAG(regression_obj);
DMLC_REGISTRY_LINK_TAG(hinge_obj);
DMLC_REGISTRY_LINK_TAG(multiclass_obj);
#endif  // TSOOBGX_USE_CUDA
DMLC_REGISTRY_LINK_TAG(rank_obj);
}  // namespace obj
}  // namespace tsoobgx
