/*
 * Copyright 2018 by Contributors
 */
#pragma once

#include <dmlc/registry.h>
#include <tsoobgx/base.h>
#include <tsoobgx/data.h>
#include <functional>
#include <string>
#include <utility>
#include <vector>
#include "../../src/gbm/gblinear_model.h"
#include "../../src/common/host_device_vector.h"

namespace tsoobgx {
/*!
 * \brief interface of linear updater
 */
class LinearUpdater {
 public:
  /*! \brief virtual destructor */
  virtual ~LinearUpdater() = default;
  /*!
   * \brief Initialize the updater with given arguments.
   * \param args arguments to the objective function.
   */
  virtual void Init(
      const std::vector<std::pair<std::string, std::string> >& args) = 0;

  /**
   * \brief Updates linear model given gradients.
   *
   * \param in_gpair            The gradient pair statistics of the data.
   * \param data                Input data matrix.
   * \param model               Model to be updated.
   * \param sum_instance_weight The sum instance weights, used to normalise l1/l2 penalty.
   */

  virtual void Update(HostDeviceVector<GradientPair>* in_gpair, DMatrix* data,
                      gbm::GBLinearModel* model,
                      double sum_instance_weight) = 0;

  /*!
   * \brief Create a linear updater given name
   * \param name Name of the linear updater.
   */
  static LinearUpdater* Create(const std::string& name);
};

/*!
 * \brief Registry entry for linear updater.
 */
struct LinearUpdaterReg
    : public dmlc::FunctionRegEntryBase<LinearUpdaterReg,
                                        std::function<LinearUpdater*()> > {};

/*!
 * \brief Macro to register linear updater.
 */
#define TSOOBGX_REGISTER_LINEAR_UPDATER(UniqueId, Name)                        \
  static DMLC_ATTRIBUTE_UNUSED ::tsoobgx::LinearUpdaterReg&                    \
      __make_##LinearUpdaterReg##_##UniqueId##__ =                             \
          ::dmlc::Registry< ::tsoobgx::LinearUpdaterReg>::Get()->__REGISTER__( \
              Name)

}  // namespace tsoobgx
