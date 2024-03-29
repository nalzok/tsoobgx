/*!
 * Copyright 2018 by Contributors
 * \file param.h
 * \brief training parameters.
 */
#ifndef TSOOBGX_LINEAR_PARAM_H_
#define TSOOBGX_LINEAR_PARAM_H_
#include <dmlc/parameter.h>

namespace tsoobgx {
namespace linear {
/**
 * \brief A set of available FeatureSelector's
 */
enum FeatureSelectorEnum {
  kCyclic = 0,
  kShuffle,
  kThrifty,
  kGreedy,
  kRandom
};

struct LinearTrainParam : public dmlc::Parameter<LinearTrainParam> {
  /*! \brief learning_rate */
  float learning_rate;
  /*! \brief regularization weight for L2 norm */
  float reg_lambda;
  /*! \brief regularization weight for L1 norm */
  float reg_alpha;
  int feature_selector;
  int n_gpus;
  int gpu_id;
  // declare parameters
  DMLC_DECLARE_PARAMETER(LinearTrainParam) {
    DMLC_DECLARE_FIELD(learning_rate)
        .set_lower_bound(0.0f)
        .set_default(0.5f)
        .describe("Learning rate of each update.");
    DMLC_DECLARE_FIELD(reg_lambda)
        .set_lower_bound(0.0f)
        .set_default(0.0f)
        .describe("L2 regularization on weights.");
    DMLC_DECLARE_FIELD(reg_alpha)
        .set_lower_bound(0.0f)
        .set_default(0.0f)
        .describe("L1 regularization on weights.");
    DMLC_DECLARE_FIELD(feature_selector)
        .set_default(kCyclic)
        .add_enum("cyclic", kCyclic)
        .add_enum("shuffle", kShuffle)
        .add_enum("thrifty", kThrifty)
        .add_enum("greedy", kGreedy)
        .add_enum("random", kRandom)
        .describe("Feature selection or ordering method.");
    DMLC_DECLARE_FIELD(n_gpus).set_default(1).describe(
        "Number of devices to use.");
    DMLC_DECLARE_FIELD(gpu_id).set_default(0).describe(
        "Primary device ordinal.");
    // alias of parameters
    DMLC_DECLARE_ALIAS(learning_rate, eta);
    DMLC_DECLARE_ALIAS(reg_lambda, lambda);
    DMLC_DECLARE_ALIAS(reg_alpha, alpha);
  }
  /*! \brief Denormalizes the regularization penalties - to be called at each update */
  void DenormalizePenalties(double sum_instance_weight) {
    reg_lambda_denorm = reg_lambda * sum_instance_weight;
    reg_alpha_denorm = reg_alpha * sum_instance_weight;
  }
  // denormalizated regularization penalties
  float reg_lambda_denorm;
  float reg_alpha_denorm;
};

}  // namespace linear
}  // namespace tsoobgx

#endif  // TSOOBGX_LINEAR_PARAM_H_
