/*!
 * Copyright 2015 by Contributors
 * \file sparse_page_dmatrix.h
 * \brief External-memory version of DMatrix.
 * \author Tianqi Chen
 */
#ifndef TSOOBGX_DATA_SPARSE_PAGE_DMATRIX_H_
#define TSOOBGX_DATA_SPARSE_PAGE_DMATRIX_H_

#include <tsoobgx/data.h>
#include <algorithm>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "sparse_page_source.h"

namespace tsoobgx {
namespace data {

class SparsePageDMatrix : public DMatrix {
 public:
  explicit SparsePageDMatrix(std::unique_ptr<DataSource>&& source,
                             std::string cache_info)
      : row_source_(std::move(source)), cache_info_(std::move(cache_info)) {}

  MetaInfo& Info() override;

  const MetaInfo& Info() const override;

  BatchSet GetRowBatches() override;

  BatchSet GetSortedColumnBatches() override;

  BatchSet GetColumnBatches() override;

  float GetColDensity(size_t cidx) override;

  bool SingleColBlock() const override;

 private:
  // source data pointers.
  std::unique_ptr<DataSource> row_source_;
  std::unique_ptr<SparsePageSource> column_source_;
  std::unique_ptr<SparsePageSource> sorted_column_source_;
  // the cache prefix
  std::string cache_info_;
  // Store column densities to avoid recalculating
  std::vector<float> col_density_;
};
}  // namespace data
}  // namespace tsoobgx
#endif  // TSOOBGX_DATA_SPARSE_PAGE_DMATRIX_H_
