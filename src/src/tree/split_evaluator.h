/*!
 * Copyright 2018 by Contributors
 * \file split_evaluator.h
 * \brief Used for implementing a loss term specific to decision trees. Useful for custom regularisation.
 * \author Henry Gouk
 */

#ifndef TSOOBGX_TREE_SPLIT_EVALUATOR_H_
#define TSOOBGX_TREE_SPLIT_EVALUATOR_H_

#include <dmlc/registry.h>
#include <tsoobgx/base.h>
#include <functional>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#define ROOT_PARENT_ID (-1 & ((1U << 31) - 1))

namespace tsoobgx {
namespace tree {

// Should GradStats be in this header, rather than param.h?
struct GradStats;

class SplitEvaluator {
 public:
  // Factory method for constructing new SplitEvaluators
  static SplitEvaluator* Create(const std::string& name);

  virtual ~SplitEvaluator() = default;

  // Used to initialise any regularisation hyperparameters provided by the user
  virtual void Init(
      const std::vector<std::pair<std::string, std::string> >& args);

  // Resets the SplitEvaluator to the state it was in after the Init was called
  virtual void Reset();

  // This will create a clone of the SplitEvaluator in host memory
  virtual SplitEvaluator* GetHostClone() const = 0;

  // Computes the score (negative loss) resulting from performing this split
  virtual bst_float ComputeSplitScore(bst_uint nodeid,
                                      bst_uint featureid,
                                      const GradStats& left_stats,
                                      const GradStats& right_stats,
                                      bst_float left_weight,
                                      bst_float right_weight) const = 0;

  virtual bst_float ComputeSplitScore(bst_uint nodeid,
                                      bst_uint featureid,
                                      const GradStats& left_stats,
                                      const GradStats& right_stats) const;

  // Compute the Score for a node with the given stats
  virtual bst_float ComputeScore(bst_uint parentid,
                                const GradStats &stats,
                                bst_float weight) const = 0;

  // Compute the weight for a node with the given stats
  virtual bst_float ComputeWeight(bst_uint parentid, const GradStats& stats)
      const = 0;

  virtual void AddSplit(bst_uint nodeid,
                        bst_uint leftid,
                        bst_uint rightid,
                        bst_uint featureid,
                        bst_float leftweight,
                        bst_float rightweight);

  // Check whether a given feature is feasible for a given node.
  // Use this function to narrow the search space for split candidates
  virtual bool CheckFeatureConstraint(bst_uint nodeid,
                                      bst_uint featureid) const = 0;
};

struct SplitEvaluatorReg
    : public dmlc::FunctionRegEntryBase<SplitEvaluatorReg,
        std::function<SplitEvaluator* (std::unique_ptr<SplitEvaluator>)> > {};

/*!
 * \brief Macro to register tree split evaluator.
 *
 * \code
 * // example of registering a split evaluator
 * TSOOBGX_REGISTER_SPLIT_EVALUATOR(SplitEval, "splitEval")
 * .describe("Some split evaluator")
 * .set_body([]() {
 *     return new SplitEval();
 *   });
 * \endcode
 */
#define TSOOBGX_REGISTER_SPLIT_EVALUATOR(UniqueID, Name) \
  static DMLC_ATTRIBUTE_UNUSED ::tsoobgx::tree::SplitEvaluatorReg& \
  __make_ ## SplitEvaluatorReg ## _ ## UniqueID ## __ = \
      ::dmlc::Registry< ::tsoobgx::tree::SplitEvaluatorReg>::Get()->__REGISTER__(Name)  //NOLINT

}  // namespace tree
}  // namespace tsoobgx

#endif  // TSOOBGX_TREE_SPLIT_EVALUATOR_H_
