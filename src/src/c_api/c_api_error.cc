/*!
 *  Copyright (c) 2015 by Contributors
 * \file c_api_error.cc
 * \brief C error handling
 */
#include <dmlc/thread_local.h>
#include "./c_api_error.h"

struct BGXAPIErrorEntry {
  std::string last_error;
};

using BGXAPIErrorStore = dmlc::ThreadLocalStore<BGXAPIErrorEntry>;

const char *BGXGetLastError() {
  return BGXAPIErrorStore::Get()->last_error.c_str();
}

void BGXAPISetLastError(const char* msg) {
  BGXAPIErrorStore::Get()->last_error = msg;
}
