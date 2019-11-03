/*!
 * Copyright 2019 by Contributors
 * \file build_config.h
 */
#ifndef TSOOBGX_BUILD_CONFIG_H_
#define TSOOBGX_BUILD_CONFIG_H_

// These check are for Makefile.
#if !defined(TSOOBGX_MM_PREFETCH_PRESENT) && !defined(TSOOBGX_BUILTIN_PREFETCH_PRESENT)
/* default logic for software pre-fetching */
#if (defined(_MSC_VER) && (defined(_M_IX86) || defined(_M_AMD64))) || defined(__INTEL_COMPILER)
// Enable _mm_prefetch for Intel compiler and MSVC+x86
  #define TSOOBGX_MM_PREFETCH_PRESENT
  #define TSOOBGX_BUILTIN_PREFETCH_PRESENT
#elif defined(__GNUC__)
// Enable __builtin_prefetch for GCC
#define TSOOBGX_BUILTIN_PREFETCH_PRESENT
#endif  // GUARDS

#endif  // !defined(TSOOBGX_MM_PREFETCH_PRESENT) && !defined()

#endif  // TSOOBGX_BUILD_CONFIG_H_
