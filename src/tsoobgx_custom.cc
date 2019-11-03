// Copyright (c) 2015 by Contributors
// This file contains the customization implementations of R module
// to change behavior of libtsoobgx

#include <tsoobgx/logging.h>
#include "../../src/common/random.h"
#include "./tsoobgx_R.h"

// redirect the messages to R's console.
namespace dmlc {
void CustomLogMessage::Log(const std::string& msg) {
  Rprintf("%s\n", msg.c_str());
}
}  // namespace dmlc

// implements rabit error handling.
extern "C" {
  void tsooBGXAssert_R(int exp, const char *fmt, ...);
  void tsooBGXCheck_R(int exp, const char *fmt, ...);
}

namespace rabit {
namespace utils {
extern "C" {
  void (*Printf)(const char *fmt, ...) = Rprintf;
  void (*Assert)(int exp, const char *fmt, ...) = tsooBGXAssert_R;
  void (*Check)(int exp, const char *fmt, ...) = tsooBGXCheck_R;
  void (*Error)(const char *fmt, ...) = error;
}
}
}

namespace tsoobgx {
ConsoleLogger::~ConsoleLogger() {
  if (cur_verbosity_ == LogVerbosity::kIgnore ||
      cur_verbosity_ <= global_verbosity_) {
    dmlc::CustomLogMessage::Log(log_stream_.str());
  }
}
TrackerLogger::~TrackerLogger() {
  dmlc::CustomLogMessage::Log(log_stream_.str());
}
}  // namespace tsoobgx

namespace tsoobgx {
namespace common {

// redirect the nath functions.
bool CheckNAN(double v) {
  return ISNAN(v);
}
#if !defined(TSOOBGX_USE_CUDA)
double LogGamma(double v) {
  return lgammafn(v);
}
#endif  // !defined(TSOOBGX_USE_CUDA)
// customize random engine.
void CustomGlobalRandomEngine::seed(CustomGlobalRandomEngine::result_type val) {
  // ignore the seed
}

// use R's PRNG to replacd
CustomGlobalRandomEngine::result_type
CustomGlobalRandomEngine::operator()() {
  return static_cast<result_type>(
      std::floor(unif_rand() * CustomGlobalRandomEngine::max()));
}
}  // namespace common
}  // namespace tsoobgx
