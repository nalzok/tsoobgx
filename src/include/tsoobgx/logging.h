/*!
 * Copyright (c) 2015 by Contributors
 * \file logging.h
 * \brief defines console logging options for tsoobgx.
 *  Use to enforce unified print behavior.
 *  For debug loggers, use LOG(INFO) and LOG(ERROR).
 */
#ifndef TSOOBGX_LOGGING_H_
#define TSOOBGX_LOGGING_H_

#include <dmlc/logging.h>
#include <dmlc/parameter.h>
#include <dmlc/thread_local.h>
#include <sstream>
#include <map>
#include <string>
#include <utility>
#include <vector>
#include "./base.h"

namespace tsoobgx {

class BaseLogger {
 public:
  BaseLogger() {
#if TSOOBGX_LOG_WITH_TIME
    log_stream_ << "[" << dmlc::DateLogger().HumanDate() << "] ";
#endif  // TSOOBGX_LOG_WITH_TIME
  }
  std::ostream& stream() { return log_stream_; }  // NOLINT

 protected:
  std::ostringstream log_stream_;
};

// Parsing both silent and debug_verbose is to provide backward compatibility.
struct ConsoleLoggerParam : public dmlc::Parameter<ConsoleLoggerParam> {
  bool silent;  // deprecated.
  int verbosity;

  DMLC_DECLARE_PARAMETER(ConsoleLoggerParam) {
    DMLC_DECLARE_FIELD(silent)
        .set_default(false)
        .describe("Do not print information during training.");
    DMLC_DECLARE_FIELD(verbosity)
        .set_range(0, 3)
        .set_default(1)  // shows only warning
        .describe("Flag to print out detailed breakdown of runtime.");
    DMLC_DECLARE_ALIAS(verbosity, debug_verbose);
  }
};

class ConsoleLogger : public BaseLogger {
 public:
  enum class LogVerbosity {
    kSilent = 0,
    kWarning = 1,
    kInfo = 2,   // information may interests users.
    kDebug = 3,  // information only interesting to developers.
    kIgnore = 4  // ignore global setting
  };
  using LV = LogVerbosity;

 private:
  static LogVerbosity global_verbosity_;
  static ConsoleLoggerParam param_;

  LogVerbosity cur_verbosity_;
  static void Configure(const std::map<std::string, std::string>& args);

 public:
  template <typename ArgIter>
  static void Configure(ArgIter begin, ArgIter end) {
    std::map<std::string, std::string> args(begin, end);
    Configure(args);
  }

  static LogVerbosity GlobalVerbosity();
  static LogVerbosity DefaultVerbosity();
  static bool ShouldLog(LogVerbosity verbosity);

  ConsoleLogger() = delete;
  explicit ConsoleLogger(LogVerbosity cur_verb);
  ConsoleLogger(const std::string& file, int line, LogVerbosity cur_verb);
  ~ConsoleLogger();
};

class TrackerLogger : public BaseLogger {
 public:
  ~TrackerLogger();
};

// custom logging callback; disabled for R wrapper
#if !defined(TSOOBGX_STRICT_R_MODE) || TSOOBGX_STRICT_R_MODE == 0
class LogCallbackRegistry {
 public:
  using Callback = void (*)(const char*);
  LogCallbackRegistry()
    : log_callback_([] (const char* msg) { std::cerr << msg << std::endl; }) {}
  inline void Register(Callback log_callback) {
    this->log_callback_ = log_callback;
  }
  inline Callback Get() const {
    return log_callback_;
  }
 private:
  Callback log_callback_;
};
#else
class LogCallbackRegistry {
 public:
  using Callback = void (*)(const char*);
  LogCallbackRegistry() {}
  inline void Register(Callback log_callback) {}
  inline Callback Get() const {
    return nullptr;
  }
};
#endif  // !defined(TSOOBGX_STRICT_R_MODE) || TSOOBGX_STRICT_R_MODE == 0

using LogCallbackRegistryStore = dmlc::ThreadLocalStore<LogCallbackRegistry>;

// Redefines LOG_WARNING for controling verbosity
#if defined(LOG_WARNING)
#undef  LOG_WARNING
#endif  // defined(LOG_WARNING)
#define LOG_WARNING                                                            \
  if (::tsoobgx::ConsoleLogger::ShouldLog(                                     \
          ::tsoobgx::ConsoleLogger::LV::kWarning))                             \
  ::tsoobgx::ConsoleLogger(__FILE__, __LINE__,                                 \
                           ::tsoobgx::ConsoleLogger::LogVerbosity::kWarning)

// Redefines LOG_INFO for controling verbosity
#if defined(LOG_INFO)
#undef  LOG_INFO
#endif  // defined(LOG_INFO)
#define LOG_INFO                                                               \
  if (::tsoobgx::ConsoleLogger::ShouldLog(                                     \
          ::tsoobgx::ConsoleLogger::LV::kInfo))                                \
  ::tsoobgx::ConsoleLogger(__FILE__, __LINE__,                                 \
                           ::tsoobgx::ConsoleLogger::LogVerbosity::kInfo)

#if defined(LOG_DEBUG)
#undef LOG_DEBUG
#endif  // defined(LOG_DEBUG)
#define LOG_DEBUG                                                              \
  if (::tsoobgx::ConsoleLogger::ShouldLog(                                     \
          ::tsoobgx::ConsoleLogger::LV::kDebug))                               \
  ::tsoobgx::ConsoleLogger(__FILE__, __LINE__,                                 \
                           ::tsoobgx::ConsoleLogger::LogVerbosity::kDebug)

// redefines the logging macro if not existed
#ifndef LOG
#define LOG(severity) LOG_##severity.stream()
#endif  // LOG

// Enable LOG(CONSOLE) for print messages to console.
#define LOG_CONSOLE ::tsoobgx::ConsoleLogger(           \
    ::tsoobgx::ConsoleLogger::LogVerbosity::kIgnore)
// Enable LOG(TRACKER) for print messages to tracker
#define LOG_TRACKER ::tsoobgx::TrackerLogger()
}  // namespace tsoobgx.
#endif  // TSOOBGX_LOGGING_H_
