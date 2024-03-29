/*!
 * Copyright 2015 by Contributors
 * \file math.h
 * \brief additional math utils
 * \author Tianqi Chen
 */
#ifndef TSOOBGX_COMMON_MATH_H_
#define TSOOBGX_COMMON_MATH_H_

#include <tsoobgx/base.h>

#include <utility>
#include <vector>
#include <cmath>
#include <algorithm>
#include <utility>

namespace tsoobgx {
namespace common {
/*!
 * \brief calculate the sigmoid of the input.
 * \param x input parameter
 * \return the transformed value.
 */
TSOOBGX_DEVICE inline float Sigmoid(float x) {
  return 1.0f / (1.0f + expf(-x));
}

/*!
 * \brief Do inplace softmax transformaton on start to end
 *
 * \tparam Iterator Input iterator type
 *
 * \param start Start iterator of input
 * \param end end iterator of input
 */
template <typename Iterator>
TSOOBGX_DEVICE inline void Softmax(Iterator start, Iterator end) {
  static_assert(std::is_same<bst_float,
                typename std::remove_reference<
                  decltype(std::declval<Iterator>().operator*())>::type
                >::value,
                "Values should be of type bst_float");
  bst_float wmax = *start;
  for (Iterator i = start+1; i != end; ++i) {
    wmax = fmaxf(*i, wmax);
  }
  double wsum = 0.0f;
  for (Iterator i = start; i != end; ++i) {
    *i = expf(*i - wmax);
    wsum += *i;
  }
  for (Iterator i = start; i != end; ++i) {
    *i /= static_cast<float>(wsum);
  }
}

/*!
 * \brief Find the maximum iterator within the iterators
 * \param begin The begining iterator.
 * \param end The end iterator.
 * \return the iterator point to the maximum value.
 * \tparam Iterator The type of the iterator.
 */
template<typename Iterator>
TSOOBGX_DEVICE inline Iterator FindMaxIndex(Iterator begin, Iterator end) {
  Iterator maxit = begin;
  for (Iterator it = begin; it != end; ++it) {
    if (*it > *maxit) maxit = it;
  }
  return maxit;
}

/*!
 * \brief perform numerically safe logsum
 * \param x left input operand
 * \param y right input operand
 * \return  log(exp(x) + exp(y))
 */
inline float LogSum(float x, float y) {
  if (x < y) {
    return y + std::log(std::exp(x - y) + 1.0f);
  } else {
    return x + std::log(std::exp(y - x) + 1.0f);
  }
}

/*!
 * \brief perform numerically safe logsum
 * \param begin The begining iterator.
 * \param end The end iterator.
 * \return the iterator point to the maximum value.
 * \tparam Iterator The type of the iterator.
 */
template<typename Iterator>
inline float LogSum(Iterator begin, Iterator end) {
  float mx = *begin;
  for (Iterator it = begin; it != end; ++it) {
    mx = std::max(mx, *it);
  }
  float sum = 0.0f;
  for (Iterator it = begin; it != end; ++it) {
    sum += std::exp(*it - mx);
  }
  return mx + std::log(sum);
}

// comparator functions for sorting pairs in descending order
inline static bool CmpFirst(const std::pair<float, unsigned> &a,
                            const std::pair<float, unsigned> &b) {
  return a.first > b.first;
}
inline static bool CmpSecond(const std::pair<float, unsigned> &a,
                             const std::pair<float, unsigned> &b) {
  return a.second > b.second;
}

#if TSOOBGX_STRICT_R_MODE
// check nan
bool CheckNAN(double v);
#else
template<typename T>
inline bool CheckNAN(T v) {
#ifdef _MSC_VER
  return (_isnan(v) != 0);
#else
  return std::isnan(v);
#endif  // _MSC_VER
}
#endif  // TSOOBGX_STRICT_R_MODE_

// GPU version is not uploaded in CRAN anyway.
// Specialize only when using R with CPU.
#if TSOOBGX_STRICT_R_MODE && !defined(TSOOBGX_USE_CUDA)
double LogGamma(double v);

#else  // Not R or R with GPU.

template<typename T>
TSOOBGX_DEVICE inline T LogGamma(T v) {
#ifdef _MSC_VER

#if _MSC_VER >= 1800
  return lgamma(v);
#else
#pragma message("Warning: lgamma function was not available until VS2013"\
                ", poisson regression will be disabled")
  utils::Error("lgamma function was not available until VS2013");
  return static_cast<T>(1.0);
#endif  // _MSC_VER >= 1800

#else
  return lgamma(v);
#endif  // _MSC_VER
}

#endif  // TSOOBGX_STRICT_R_MODE && !defined(TSOOBGX_USE_CUDA)

}  // namespace common
}  // namespace tsoobgx
#endif  // TSOOBGX_COMMON_MATH_H_
