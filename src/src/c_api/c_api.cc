// Copyright (c) 2014 by Contributors

#include <tsoobgx/data.h>
#include <tsoobgx/learner.h>
#include <tsoobgx/c_api.h>
#include <tsoobgx/logging.h>

#include <dmlc/thread_local.h>
#include <rabit/rabit.h>
#include <rabit/c_api.h>

#include <cstdio>
#include <cstring>
#include <algorithm>
#include <vector>
#include <string>
#include <memory>

#include "./c_api_error.h"
#include "../data/simple_csr_source.h"
#include "../common/math.h"
#include "../common/io.h"
#include "../common/group_data.h"


namespace tsoobgx {
// booster wrapper for backward compatible reason.
class Booster {
 public:
  explicit Booster(const std::vector<std::shared_ptr<DMatrix> >& cache_mats)
      : configured_(false),
        initialized_(false),
        learner_(Learner::Create(cache_mats)) {}

  inline Learner* learner() {  // NOLINT
    return learner_.get();
  }

  inline void SetParam(const std::string& name, const std::string& val) {
    auto it = std::find_if(cfg_.begin(), cfg_.end(),
      [&name, &val](decltype(*cfg_.begin()) &x) {
        if (name == "eval_metric") {
          return x.first == name && x.second == val;
        }
        return x.first == name;
      });
    if (it == cfg_.end()) {
      cfg_.emplace_back(name, val);
    } else {
      (*it).second = val;
    }
    if (configured_) {
      learner_->Configure(cfg_);
    }
  }

  inline void LazyInit() {
    if (!configured_) {
      LoadSavedParamFromAttr();
      learner_->Configure(cfg_);
      configured_ = true;
    }
    if (!initialized_) {
      learner_->InitModel();
      initialized_ = true;
    }
  }

  inline void LoadSavedParamFromAttr() {
    // Locate saved parameters from learner attributes
    const std::string prefix = "SAVED_PARAM_";
    for (const std::string& attr_name : learner_->GetAttrNames()) {
      if (attr_name.find(prefix) == 0) {
        const std::string saved_param = attr_name.substr(prefix.length());
        if (std::none_of(cfg_.begin(), cfg_.end(),
                         [&](const std::pair<std::string, std::string>& x)
                             { return x.first == saved_param; })) {
          // If cfg_ contains the parameter already, skip it
          //   (this is to allow the user to explicitly override its value)
          std::string saved_param_value;
          CHECK(learner_->GetAttr(attr_name, &saved_param_value));
          cfg_.emplace_back(saved_param, saved_param_value);
        }
      }
    }
  }

  inline void LoadModel(dmlc::Stream* fi) {
    learner_->Load(fi);
    initialized_ = true;
  }

  bool IsInitialized() const { return initialized_; }
  void Intialize() { initialized_ = true; }

 private:
  bool configured_;
  bool initialized_;
  std::unique_ptr<Learner> learner_;
  std::vector<std::pair<std::string, std::string> > cfg_;
};

// declare the data callback.
BGX_EXTERN_C int tsooBGXNativeDataIterSetData(
    void *handle, tsooBGXBatchCSR batch);

/*! \brief Native data iterator that takes callback to return data */
class NativeDataIter : public dmlc::Parser<uint32_t> {
 public:
  NativeDataIter(DataIterHandle data_handle,
                 BGXCallbackDataIterNext* next_callback)
      :  at_first_(true), bytes_read_(0),
         data_handle_(data_handle), next_callback_(next_callback) {
  }

  // override functions
  void BeforeFirst() override {
    CHECK(at_first_) << "cannot reset NativeDataIter";
  }

  bool Next() override {
    if ((*next_callback_)(
            data_handle_,
            tsooBGXNativeDataIterSetData,
            this) != 0) {
      at_first_ = false;
      return true;
    } else {
      return false;
    }
  }

  const dmlc::RowBlock<uint32_t>& Value() const override {
    return block_;
  }

  size_t BytesRead() const override {
    return bytes_read_;
  }

  // callback to set the data
  void SetData(const tsooBGXBatchCSR& batch) {
    offset_.clear();
    label_.clear();
    weight_.clear();
    index_.clear();
    value_.clear();
    offset_.insert(offset_.end(), batch.offset, batch.offset + batch.size + 1);
    if (batch.label != nullptr) {
      label_.insert(label_.end(), batch.label, batch.label + batch.size);
    }
    if (batch.weight != nullptr) {
      weight_.insert(weight_.end(), batch.weight, batch.weight + batch.size);
    }
    if (batch.index != nullptr) {
      index_.insert(index_.end(), batch.index + offset_[0], batch.index + offset_.back());
    }
    if (batch.value != nullptr) {
      value_.insert(value_.end(), batch.value + offset_[0], batch.value + offset_.back());
    }
    if (offset_[0] != 0) {
      size_t base = offset_[0];
      for (size_t& item : offset_) {
        item -= base;
      }
    }
    block_.size = batch.size;
    block_.offset = dmlc::BeginPtr(offset_);
    block_.label = dmlc::BeginPtr(label_);
    block_.weight = dmlc::BeginPtr(weight_);
    block_.qid = nullptr;
    block_.field = nullptr;
    block_.index = dmlc::BeginPtr(index_);
    block_.value = dmlc::BeginPtr(value_);
    bytes_read_ += offset_.size() * sizeof(size_t) +
        label_.size() * sizeof(dmlc::real_t) +
        weight_.size() * sizeof(dmlc::real_t) +
        index_.size() * sizeof(uint32_t) +
        value_.size() * sizeof(dmlc::real_t);
  }

 private:
  // at the beinning.
  bool at_first_;
  // bytes that is read.
  size_t bytes_read_;
  // handle to the iterator,
  DataIterHandle data_handle_;
  // call back to get the data.
  BGXCallbackDataIterNext* next_callback_;
  // internal offset
  std::vector<size_t> offset_;
  // internal label data
  std::vector<dmlc::real_t> label_;
  // internal weight data
  std::vector<dmlc::real_t> weight_;
  // internal index.
  std::vector<uint32_t> index_;
  // internal value.
  std::vector<dmlc::real_t> value_;
  // internal Rowblock
  dmlc::RowBlock<uint32_t> block_;
};

int tsooBGXNativeDataIterSetData(
    void *handle, tsooBGXBatchCSR batch) {
  API_BEGIN();
  static_cast<tsoobgx::NativeDataIter*>(handle)->SetData(batch);
  API_END();
}
}  // namespace tsoobgx

using namespace tsoobgx; // NOLINT(*);

/*! \brief entry to to easily hold returning information */
struct BGXAPIThreadLocalEntry {
  /*! \brief result holder for returning string */
  std::string ret_str;
  /*! \brief result holder for returning strings */
  std::vector<std::string> ret_vec_str;
  /*! \brief result holder for returning string pointers */
  std::vector<const char *> ret_vec_charp;
  /*! \brief returning float vector. */
  std::vector<bst_float> ret_vec_float;
  /*! \brief temp variable of gradient pairs. */
  std::vector<GradientPair> tmp_gpair;
};

// define the threadlocal store.
using BGXAPIThreadLocalStore = dmlc::ThreadLocalStore<BGXAPIThreadLocalEntry>;

int BGXRegisterLogCallback(void (*callback)(const char*)) {
  API_BEGIN();
  LogCallbackRegistry* registry = LogCallbackRegistryStore::Get();
  registry->Register(callback);
  API_END();
}

int XGDMatrixCreateFromFile(const char *fname,
                            int silent,
                            DMatrixHandle *out) {
  API_BEGIN();
  bool load_row_split = false;
  if (rabit::IsDistributed()) {
    LOG(CONSOLE) << "tsooBGX distributed mode detected, "
                 << "will split data among workers";
    load_row_split = true;
  }
  *out = new std::shared_ptr<DMatrix>(DMatrix::Load(fname, silent != 0, load_row_split));
  API_END();
}

int XGDMatrixCreateFromDataIter(
    void* data_handle,
    BGXCallbackDataIterNext* callback,
    const char *cache_info,
    DMatrixHandle *out) {
  API_BEGIN();

  std::string scache;
  if (cache_info != nullptr) {
    scache = cache_info;
  }
  NativeDataIter parser(data_handle, callback);
  *out = new std::shared_ptr<DMatrix>(DMatrix::Create(&parser, scache));
  API_END();
}

BGX_DLL int XGDMatrixCreateFromCSREx(const size_t* indptr,
                                     const unsigned* indices,
                                     const bst_float* data,
                                     size_t nindptr,
                                     size_t nelem,
                                     size_t num_col,
                                     DMatrixHandle* out) {
  std::unique_ptr<data::SimpleCSRSource> source(new data::SimpleCSRSource());

  API_BEGIN();
  data::SimpleCSRSource& mat = *source;
  auto& offset_vec = mat.page_.offset.HostVector();
  auto& data_vec = mat.page_.data.HostVector();
  offset_vec.reserve(nindptr);
  data_vec.reserve(nelem);
  offset_vec.resize(1);
  offset_vec[0] = 0;
  size_t num_column = 0;
  for (size_t i = 1; i < nindptr; ++i) {
    for (size_t j = indptr[i - 1]; j < indptr[i]; ++j) {
      if (!common::CheckNAN(data[j])) {
        // automatically skip nan.
        data_vec.emplace_back(Entry(indices[j], data[j]));
        num_column = std::max(num_column, static_cast<size_t>(indices[j] + 1));
      }
    }
    offset_vec.push_back(mat.page_.data.Size());
  }

  mat.info.num_col_ = num_column;
  if (num_col > 0) {
    CHECK_LE(mat.info.num_col_, num_col)
        << "num_col=" << num_col << " vs " << mat.info.num_col_;
    mat.info.num_col_ = num_col;
  }
  mat.info.num_row_ = nindptr - 1;
  mat.info.num_nonzero_ = mat.page_.data.Size();
  *out = new std::shared_ptr<DMatrix>(DMatrix::Create(std::move(source)));
  API_END();
}

BGX_DLL int XGDMatrixCreateFromCSCEx(const size_t* col_ptr,
                                     const unsigned* indices,
                                     const bst_float* data,
                                     size_t nindptr,
                                     size_t nelem,
                                     size_t num_row,
                                     DMatrixHandle* out) {
  std::unique_ptr<data::SimpleCSRSource> source(new data::SimpleCSRSource());

  API_BEGIN();
  // FIXME: User should be able to control number of threads
  const int nthread = omp_get_max_threads();
  data::SimpleCSRSource& mat = *source;
  auto& offset_vec = mat.page_.offset.HostVector();
  auto& data_vec = mat.page_.data.HostVector();
  common::ParallelGroupBuilder<Entry> builder(&offset_vec, &data_vec);
  builder.InitBudget(0, nthread);
  size_t ncol = nindptr - 1;  // NOLINT(*)
  #pragma omp parallel for schedule(static)
  for (omp_ulong i = 0; i < static_cast<omp_ulong>(ncol); ++i) {  // NOLINT(*)
    int tid = omp_get_thread_num();
    for (size_t j = col_ptr[i]; j < col_ptr[i+1]; ++j) {
      if (!common::CheckNAN(data[j])) {
        builder.AddBudget(indices[j], tid);
      }
    }
  }
  builder.InitStorage();
  #pragma omp parallel for schedule(static)
  for (omp_ulong i = 0; i < static_cast<omp_ulong>(ncol); ++i) {  // NOLINT(*)
    int tid = omp_get_thread_num();
    for (size_t j = col_ptr[i]; j < col_ptr[i+1]; ++j) {
      if (!common::CheckNAN(data[j])) {
        builder.Push(indices[j],
                     Entry(static_cast<bst_uint>(i), data[j]),
                     tid);
      }
    }
  }
  mat.info.num_row_ = mat.page_.offset.Size() - 1;
  if (num_row > 0) {
    CHECK_LE(mat.info.num_row_, num_row);
    // provision for empty rows at the bottom of matrix
    auto& offset_vec = mat.page_.offset.HostVector();
    for (uint64_t i = mat.info.num_row_; i < static_cast<uint64_t>(num_row); ++i) {
      offset_vec.push_back(offset_vec.back());
    }
    mat.info.num_row_ = num_row;
    CHECK_EQ(mat.info.num_row_, offset_vec.size() - 1);  // sanity check
  }
  mat.info.num_col_ = ncol;
  mat.info.num_nonzero_ = nelem;
  *out  = new std::shared_ptr<DMatrix>(DMatrix::Create(std::move(source)));
  API_END();
}

BGX_DLL int XGDMatrixCreateFromMat(const bst_float* data,
                                   tsoobgx::bst_ulong nrow,
                                   tsoobgx::bst_ulong ncol,
                                   bst_float missing,
                                   DMatrixHandle* out) {
  std::unique_ptr<data::SimpleCSRSource> source(new data::SimpleCSRSource());

  API_BEGIN();
  data::SimpleCSRSource& mat = *source;
  auto& offset_vec = mat.page_.offset.HostVector();
  auto& data_vec = mat.page_.data.HostVector();
  offset_vec.resize(1+nrow);
  bool nan_missing = common::CheckNAN(missing);
  mat.info.num_row_ = nrow;
  mat.info.num_col_ = ncol;
  const bst_float* data0 = data;

  // count elements for sizing data
  data = data0;
  for (tsoobgx::bst_ulong i = 0; i < nrow; ++i, data += ncol) {
    tsoobgx::bst_ulong nelem = 0;
    for (tsoobgx::bst_ulong j = 0; j < ncol; ++j) {
      if (common::CheckNAN(data[j])) {
        CHECK(nan_missing)
          << "There are NAN in the matrix, however, you did not set missing=NAN";
      } else {
        if (nan_missing || data[j] != missing) {
          ++nelem;
        }
      }
    }
    offset_vec[i+1] = offset_vec[i] + nelem;
  }
  data_vec.resize(mat.page_.data.Size() + offset_vec.back());

  data = data0;
  for (tsoobgx::bst_ulong i = 0; i < nrow; ++i, data += ncol) {
    tsoobgx::bst_ulong matj = 0;
    for (tsoobgx::bst_ulong j = 0; j < ncol; ++j) {
      if (common::CheckNAN(data[j])) {
      } else {
        if (nan_missing || data[j] != missing) {
          data_vec[offset_vec[i] + matj] = Entry(j, data[j]);
          ++matj;
        }
      }
    }
  }

  mat.info.num_nonzero_ = mat.page_.data.Size();
  *out  = new std::shared_ptr<DMatrix>(DMatrix::Create(std::move(source)));
  API_END();
}

void PrefixSum(size_t *x, size_t N) {
  size_t *suma;
#pragma omp parallel
  {
    const int ithread = omp_get_thread_num();
    const int nthreads = omp_get_num_threads();
#pragma omp single
    {
      suma = new size_t[nthreads+1];
      suma[0] = 0;
    }
    size_t sum = 0;
    size_t offset = 0;
#pragma omp for schedule(static)
    for (omp_ulong i = 0; i < N; i++) {
      sum += x[i];
      x[i] = sum;
    }
    suma[ithread+1] = sum;
#pragma omp barrier
    for (omp_ulong i = 0; i < static_cast<omp_ulong>(ithread+1); i++) {
      offset += suma[i];
    }
#pragma omp for schedule(static)
    for (omp_ulong i = 0; i < N; i++) {
      x[i] += offset;
    }
  }
  delete[] suma;
}

BGX_DLL int XGDMatrixCreateFromMat_omp(const bst_float* data,  // NOLINT
                                       tsoobgx::bst_ulong nrow,
                                       tsoobgx::bst_ulong ncol,
                                       bst_float missing, DMatrixHandle* out,
                                       int nthread) {
  // avoid openmp unless enough data to be worth it to avoid overhead costs
  if (nrow*ncol <= 10000*50) {
    return(XGDMatrixCreateFromMat(data, nrow, ncol, missing, out));
  }

  API_BEGIN();
  const int nthreadmax = std::max(omp_get_num_procs() / 2 - 1, 1);
  //  const int nthreadmax = omp_get_max_threads();
  if (nthread <= 0) nthread=nthreadmax;
  int nthread_orig = omp_get_max_threads();
  omp_set_num_threads(nthread);

  std::unique_ptr<data::SimpleCSRSource> source(new data::SimpleCSRSource());
  data::SimpleCSRSource& mat = *source;
  auto& offset_vec = mat.page_.offset.HostVector();
  auto& data_vec = mat.page_.data.HostVector();
  offset_vec.resize(1+nrow);
  mat.info.num_row_ = nrow;
  mat.info.num_col_ = ncol;

  // Check for errors in missing elements
  // Count elements per row (to avoid otherwise need to copy)
  bool nan_missing = common::CheckNAN(missing);
  std::vector<int> badnan;
  badnan.resize(nthread, 0);

#pragma omp parallel num_threads(nthread)
  {
    int ithread  = omp_get_thread_num();

    // Count elements per row
#pragma omp for schedule(static)
    for (omp_ulong i = 0; i < nrow; ++i) {
      tsoobgx::bst_ulong nelem = 0;
      for (tsoobgx::bst_ulong j = 0; j < ncol; ++j) {
        if (common::CheckNAN(data[ncol*i + j]) && !nan_missing) {
          badnan[ithread] = 1;
        } else if (common::CheckNAN(data[ncol * i + j])) {
        } else if (nan_missing || data[ncol * i + j] != missing) {
          ++nelem;
        }
      }
      offset_vec[i+1] = nelem;
    }
  }
  // Inform about any NaNs and resize data matrix
  for (int i = 0; i < nthread; i++) {
    CHECK(!badnan[i]) << "There are NAN in the matrix, however, you did not set missing=NAN";
  }

  // do cumulative sum (to avoid otherwise need to copy)
  PrefixSum(&offset_vec[0], offset_vec.size());
  data_vec.resize(mat.page_.data.Size() + offset_vec.back());

  // Fill data matrix (now that know size, no need for slow push_back())
#pragma omp parallel num_threads(nthread)
  {
#pragma omp for schedule(static)
    for (omp_ulong i = 0; i < nrow; ++i) {
      tsoobgx::bst_ulong matj = 0;
      for (tsoobgx::bst_ulong j = 0; j < ncol; ++j) {
        if (common::CheckNAN(data[ncol * i + j])) {
        } else if (nan_missing || data[ncol * i + j] != missing) {
          data_vec[offset_vec[i] + matj] =
              Entry(j, data[ncol * i + j]);
          ++matj;
        }
      }
    }
  }
  // restore omp state
  omp_set_num_threads(nthread_orig);

  mat.info.num_nonzero_ = mat.page_.data.Size();
  *out  = new std::shared_ptr<DMatrix>(DMatrix::Create(std::move(source)));
  API_END();
}

enum class DTType : uint8_t {
  kFloat32 = 0,
  kFloat64 = 1,
  kBool8 = 2,
  kInt32 = 3,
  kInt8 = 4,
  kInt16 = 5,
  kInt64 = 6,
  kUnknown = 7
};

DTType DTGetType(std::string type_string) {
  if (type_string == "float32") {
    return DTType::kFloat32;
  } else if (type_string == "float64") {
    return DTType::kFloat64;
  } else if (type_string == "bool8") {
    return DTType::kBool8;
  } else if (type_string == "int32") {
    return DTType::kInt32;
  } else if (type_string == "int8") {
    return DTType::kInt8;
  } else if (type_string == "int16") {
    return DTType::kInt16;
  } else if (type_string == "int64") {
    return DTType::kInt64;
  } else {
    LOG(FATAL) << "Unknown data table type.";
    return DTType::kUnknown;
  }
}

float DTGetValue(void* column, DTType dt_type, size_t ridx) {
  float missing = std::numeric_limits<float>::quiet_NaN();
  switch (dt_type) {
    case DTType::kFloat32: {
      float val = reinterpret_cast<float*>(column)[ridx];
      return std::isfinite(val) ? val : missing;
    }
    case DTType::kFloat64: {
      double val = reinterpret_cast<double*>(column)[ridx];
      return std::isfinite(val) ? static_cast<float>(val) : missing;
    }
    case DTType::kBool8: {
      bool val = reinterpret_cast<bool*>(column)[ridx];
      return static_cast<float>(val);
    }
    case DTType::kInt32: {
      int32_t val = reinterpret_cast<int32_t*>(column)[ridx];
      return val != (-2147483647 - 1) ? static_cast<float>(val) : missing;
    }
    case DTType::kInt8: {
      int8_t val = reinterpret_cast<int8_t*>(column)[ridx];
      return val != -128 ? static_cast<float>(val) : missing;
    }
    case DTType::kInt16: {
      int16_t val = reinterpret_cast<int16_t*>(column)[ridx];
      return val != -32768 ? static_cast<float>(val) : missing;
    }
    case DTType::kInt64: {
      int64_t val = reinterpret_cast<int64_t*>(column)[ridx];
      return val != -9223372036854775807 - 1 ? static_cast<float>(val)
                                             : missing;
    }
    default: {
      LOG(FATAL) << "Unknown data table type.";
      return 0.0f;
    }
  }
}

BGX_DLL int XGDMatrixCreateFromDT(void** data, const char** feature_stypes,
                                  tsoobgx::bst_ulong nrow,
                                  tsoobgx::bst_ulong ncol, DMatrixHandle* out,
                                  int nthread) {
  // avoid openmp unless enough data to be worth it to avoid overhead costs
  if (nrow * ncol <= 10000 * 50) {
    nthread = 1;
  }

  API_BEGIN();
  const int nthreadmax = std::max(omp_get_num_procs() / 2 - 1, 1);
  if (nthread <= 0) nthread = nthreadmax;
  int nthread_orig = omp_get_max_threads();
  omp_set_num_threads(nthread);

  std::unique_ptr<data::SimpleCSRSource> source(new data::SimpleCSRSource());
  data::SimpleCSRSource& mat = *source;
  mat.page_.offset.Resize(1 + nrow);
  mat.info.num_row_ = nrow;
  mat.info.num_col_ = ncol;

  auto& page_offset = mat.page_.offset.HostVector();
#pragma omp parallel num_threads(nthread)
  {
    // Count elements per row, column by column
    for (auto j = 0u; j < ncol; ++j) {
      DTType dtype = DTGetType(feature_stypes[j]);
#pragma omp for schedule(static)
      for (omp_ulong i = 0; i < nrow; ++i) {
        float val = DTGetValue(data[j], dtype, i);
        if (!std::isnan(val)) {
          page_offset[i + 1]++;
        }
      }
    }
  }
  // do cumulative sum (to avoid otherwise need to copy)
  PrefixSum(&page_offset[0], page_offset.size());

  mat.page_.data.Resize(mat.page_.data.Size() + page_offset.back());

  auto& page_data = mat.page_.data.HostVector();

  // Fill data matrix (now that know size, no need for slow push_back())
  std::vector<size_t> position(nrow);
#pragma omp parallel num_threads(nthread)
  {
    for (tsoobgx::bst_ulong j = 0; j < ncol; ++j) {
      DTType dtype = DTGetType(feature_stypes[j]);
#pragma omp for schedule(static)
      for (omp_ulong i = 0; i < nrow; ++i) {
        float val = DTGetValue(data[j], dtype, i);
        if (!std::isnan(val)) {
          page_data[page_offset[i] + position[i]] = Entry(j, val);
          position[i]++;
        }
      }
    }
  }

  // restore omp state
  omp_set_num_threads(nthread_orig);

  mat.info.num_nonzero_ = mat.page_.data.Size();
  *out = new std::shared_ptr<DMatrix>(DMatrix::Create(std::move(source)));
  API_END();
}

BGX_DLL int XGDMatrixSliceDMatrix(DMatrixHandle handle,
                                  const int* idxset,
                                  tsoobgx::bst_ulong len,
                                  DMatrixHandle* out) {
  std::unique_ptr<data::SimpleCSRSource> source(new data::SimpleCSRSource());

  API_BEGIN();
  CHECK_HANDLE();
  data::SimpleCSRSource src;
  src.CopyFrom(static_cast<std::shared_ptr<DMatrix>*>(handle)->get());
  data::SimpleCSRSource& ret = *source;

  CHECK_EQ(src.info.group_ptr_.size(), 0U)
      << "slice does not support group structure";

  ret.Clear();
  ret.info.num_row_ = len;
  ret.info.num_col_ = src.info.num_col_;

  auto iter = &src;
  iter->BeforeFirst();
  CHECK(iter->Next());

  const auto& batch = iter->Value();
  const auto& src_labels = src.info.labels_.ConstHostVector();
  const auto& src_weights = src.info.weights_.ConstHostVector();
  const auto& src_base_margin = src.info.base_margin_.ConstHostVector();
  auto& ret_labels = ret.info.labels_.HostVector();
  auto& ret_weights = ret.info.weights_.HostVector();
  auto& ret_base_margin = ret.info.base_margin_.HostVector();
  auto& offset_vec = ret.page_.offset.HostVector();
  auto& data_vec = ret.page_.data.HostVector();

  for (tsoobgx::bst_ulong i = 0; i < len; ++i) {
    const int ridx = idxset[i];
    auto inst = batch[ridx];
    CHECK_LT(static_cast<tsoobgx::bst_ulong>(ridx), batch.Size());
    data_vec.insert(data_vec.end(), inst.data(),
                    inst.data() + inst.size());
    offset_vec.push_back(offset_vec.back() + inst.size());
    ret.info.num_nonzero_ += inst.size();

    if (src_labels.size() != 0) {
      ret_labels.push_back(src_labels[ridx]);
    }
    if (src_weights.size() != 0) {
      ret_weights.push_back(src_weights[ridx]);
    }
    if (src_base_margin.size() != 0) {
      ret_base_margin.push_back(src_base_margin[ridx]);
    }
    if (src.info.root_index_.size() != 0) {
      ret.info.root_index_.push_back(src.info.root_index_[ridx]);
    }
  }
  *out = new std::shared_ptr<DMatrix>(DMatrix::Create(std::move(source)));
  API_END();
}

BGX_DLL int XGDMatrixFree(DMatrixHandle handle) {
  API_BEGIN();
  CHECK_HANDLE();
  delete static_cast<std::shared_ptr<DMatrix>*>(handle);
  API_END();
}

BGX_DLL int XGDMatrixSaveBinary(DMatrixHandle handle,
                                const char* fname,
                                int silent) {
  API_BEGIN();
  CHECK_HANDLE();
  static_cast<std::shared_ptr<DMatrix>*>(handle)->get()->SaveToLocalFile(fname);
  API_END();
}

BGX_DLL int XGDMatrixSetFloatInfo(DMatrixHandle handle,
                          const char* field,
                          const bst_float* info,
                          tsoobgx::bst_ulong len) {
  API_BEGIN();
  CHECK_HANDLE();
  static_cast<std::shared_ptr<DMatrix>*>(handle)
      ->get()->Info().SetInfo(field, info, kFloat32, len);
  API_END();
}

BGX_DLL int XGDMatrixSetUIntInfo(DMatrixHandle handle,
                         const char* field,
                         const unsigned* info,
                         tsoobgx::bst_ulong len) {
  API_BEGIN();
  CHECK_HANDLE();
  static_cast<std::shared_ptr<DMatrix>*>(handle)
      ->get()->Info().SetInfo(field, info, kUInt32, len);
  API_END();
}

BGX_DLL int XGDMatrixSetGroup(DMatrixHandle handle,
                              const unsigned* group,
                              tsoobgx::bst_ulong len) {
  API_BEGIN();
  CHECK_HANDLE();
  auto *pmat = static_cast<std::shared_ptr<DMatrix>*>(handle);
  MetaInfo& info = pmat->get()->Info();
  info.group_ptr_.resize(len + 1);
  info.group_ptr_[0] = 0;
  for (uint64_t i = 0; i < len; ++i) {
    info.group_ptr_[i + 1] = info.group_ptr_[i] + group[i];
  }
  API_END();
}

BGX_DLL int XGDMatrixGetFloatInfo(const DMatrixHandle handle,
                                  const char* field,
                                  tsoobgx::bst_ulong* out_len,
                                  const bst_float** out_dptr) {
  API_BEGIN();
  CHECK_HANDLE();
  const MetaInfo& info = static_cast<std::shared_ptr<DMatrix>*>(handle)->get()->Info();
  const std::vector<bst_float>* vec = nullptr;
  if (!std::strcmp(field, "label")) {
    vec = &info.labels_.HostVector();
  } else if (!std::strcmp(field, "weight")) {
    vec = &info.weights_.HostVector();
  } else if (!std::strcmp(field, "base_margin")) {
    vec = &info.base_margin_.HostVector();
  } else {
    LOG(FATAL) << "Unknown float field name " << field;
  }
  *out_len = static_cast<tsoobgx::bst_ulong>(vec->size());  // NOLINT
  *out_dptr = dmlc::BeginPtr(*vec);
  API_END();
}

BGX_DLL int XGDMatrixGetUIntInfo(const DMatrixHandle handle,
                                 const char *field,
                                 tsoobgx::bst_ulong *out_len,
                                 const unsigned **out_dptr) {
  API_BEGIN();
  CHECK_HANDLE();
  const MetaInfo& info = static_cast<std::shared_ptr<DMatrix>*>(handle)->get()->Info();
  const std::vector<unsigned>* vec = nullptr;
  if (!std::strcmp(field, "root_index")) {
    vec = &info.root_index_;
    *out_len = static_cast<tsoobgx::bst_ulong>(vec->size());
    *out_dptr = dmlc::BeginPtr(*vec);
  } else {
    LOG(FATAL) << "Unknown uint field name " << field;
  }
  API_END();
}

BGX_DLL int XGDMatrixNumRow(const DMatrixHandle handle,
                            tsoobgx::bst_ulong *out) {
  API_BEGIN();
  CHECK_HANDLE();
  *out = static_cast<tsoobgx::bst_ulong>(
      static_cast<std::shared_ptr<DMatrix>*>(handle)->get()->Info().num_row_);
  API_END();
}

BGX_DLL int XGDMatrixNumCol(const DMatrixHandle handle,
                            tsoobgx::bst_ulong *out) {
  API_BEGIN();
  CHECK_HANDLE();
  *out = static_cast<size_t>(
      static_cast<std::shared_ptr<DMatrix>*>(handle)->get()->Info().num_col_);
  API_END();
}

// tsoobgx implementation
BGX_DLL int retsooBGXCreate(const DMatrixHandle dmats[],
                    tsoobgx::bst_ulong len,
                    BoosterHandle *out) {
  API_BEGIN();
  std::vector<std::shared_ptr<DMatrix> > mats;
  for (tsoobgx::bst_ulong i = 0; i < len; ++i) {
    mats.push_back(*static_cast<std::shared_ptr<DMatrix>*>(dmats[i]));
  }
  *out = new Booster(mats);
  API_END();
}

BGX_DLL int retsooBGXFree(BoosterHandle handle) {
  API_BEGIN();
  CHECK_HANDLE();
  delete static_cast<Booster*>(handle);
  API_END();
}

BGX_DLL int retsooBGXSetParam(BoosterHandle handle,
                              const char *name,
                              const char *value) {
  API_BEGIN();
  CHECK_HANDLE();
  static_cast<Booster*>(handle)->SetParam(name, value);
  API_END();
}

BGX_DLL int retsooBGXUpdateOneIter(BoosterHandle handle,
                                   int iter,
                                   DMatrixHandle dtrain) {
  API_BEGIN();
  CHECK_HANDLE();
  auto* bst = static_cast<Booster*>(handle);
  auto *dtr =
      static_cast<std::shared_ptr<DMatrix>*>(dtrain);

  bst->LazyInit();
  bst->learner()->UpdateOneIter(iter, dtr->get());
  API_END();
}

BGX_DLL int retsooBGXBoostOneIter(BoosterHandle handle,
                                  DMatrixHandle dtrain,
                                  bst_float *grad,
                                  bst_float *hess,
                                  tsoobgx::bst_ulong len) {
  HostDeviceVector<GradientPair> tmp_gpair;
  API_BEGIN();
  CHECK_HANDLE();
  auto* bst = static_cast<Booster*>(handle);
  auto* dtr =
      static_cast<std::shared_ptr<DMatrix>*>(dtrain);
  tmp_gpair.Resize(len);
  std::vector<GradientPair>& tmp_gpair_h = tmp_gpair.HostVector();
  for (tsoobgx::bst_ulong i = 0; i < len; ++i) {
    tmp_gpair_h[i] = GradientPair(grad[i], hess[i]);
  }

  bst->LazyInit();
  bst->learner()->BoostOneIter(0, dtr->get(), &tmp_gpair);
  API_END();
}

BGX_DLL int retsooBGXEvalOneIter(BoosterHandle handle,
                                 int iter,
                                 DMatrixHandle dmats[],
                                 const char* evnames[],
                                 tsoobgx::bst_ulong len,
                                 const char** out_str) {
  std::string& eval_str = BGXAPIThreadLocalStore::Get()->ret_str;
  API_BEGIN();
  CHECK_HANDLE();
  auto* bst = static_cast<Booster*>(handle);
  std::vector<DMatrix*> data_sets;
  std::vector<std::string> data_names;

  for (tsoobgx::bst_ulong i = 0; i < len; ++i) {
    data_sets.push_back(static_cast<std::shared_ptr<DMatrix>*>(dmats[i])->get());
    data_names.emplace_back(evnames[i]);
  }

  bst->LazyInit();
  eval_str = bst->learner()->EvalOneIter(iter, data_sets, data_names);
  *out_str = eval_str.c_str();
  API_END();
}

BGX_DLL int retsooBGXPredict(BoosterHandle handle,
                             DMatrixHandle dmat,
                             int option_mask,
                             unsigned ntree_limit,
                             tsoobgx::bst_ulong *len,
                             const bst_float **out_result) {
  std::vector<bst_float>&preds =
    BGXAPIThreadLocalStore::Get()->ret_vec_float;
  API_BEGIN();
  CHECK_HANDLE();
  auto *bst = static_cast<Booster*>(handle);
  bst->LazyInit();
  HostDeviceVector<bst_float> tmp_preds;
  bst->learner()->Predict(
      static_cast<std::shared_ptr<DMatrix>*>(dmat)->get(),
      (option_mask & 1) != 0,
      &tmp_preds, ntree_limit,
      (option_mask & 2) != 0,
      (option_mask & 4) != 0,
      (option_mask & 8) != 0,
      (option_mask & 16) != 0);
  preds = tmp_preds.HostVector();
  *out_result = dmlc::BeginPtr(preds);
  *len = static_cast<tsoobgx::bst_ulong>(preds.size());
  API_END();
}

BGX_DLL int retsooBGXLoadModel(BoosterHandle handle, const char* fname) {
  API_BEGIN();
  CHECK_HANDLE();
  std::unique_ptr<dmlc::Stream> fi(dmlc::Stream::Create(fname, "r"));
  static_cast<Booster*>(handle)->LoadModel(fi.get());
  API_END();
}

BGX_DLL int retsooBGXSaveModel(BoosterHandle handle, const char* fname) {
  API_BEGIN();
  CHECK_HANDLE();
  std::unique_ptr<dmlc::Stream> fo(dmlc::Stream::Create(fname, "w"));
  auto *bst = static_cast<Booster*>(handle);
  bst->LazyInit();
  bst->learner()->Save(fo.get());
  API_END();
}

BGX_DLL int retsooBGXLoadModelFromBuffer(BoosterHandle handle,
                                 const void* buf,
                                 tsoobgx::bst_ulong len) {
  API_BEGIN();
  CHECK_HANDLE();
  common::MemoryFixSizeBuffer fs((void*)buf, len);  // NOLINT(*)
  static_cast<Booster*>(handle)->LoadModel(&fs);
  API_END();
}

BGX_DLL int retsooBGXLoadIndividualModelFromBuffer(BoosterHandle handle,
                                 const void* buf,
                                 tsoobgx::bst_ulong len,
                                 int k) {
  API_BEGIN();
  CHECK_HANDLE();
  common::MemoryFixSizeBuffer fs((void*)buf, len);  // NOLINT(*)
  static_cast<Booster*>(handle)->LoadModel(&fs);
  API_END();
}

BGX_DLL int retsooBGXGetModelRaw(BoosterHandle handle,
                         tsoobgx::bst_ulong* out_len,
                         const char** out_dptr) {
  std::string& raw_str = BGXAPIThreadLocalStore::Get()->ret_str;
  raw_str.resize(0);

  API_BEGIN();
  CHECK_HANDLE();
  common::MemoryBufferStream fo(&raw_str);
  auto *bst = static_cast<Booster*>(handle);
  bst->LazyInit();
  bst->learner()->Save(&fo);
  *out_dptr = dmlc::BeginPtr(raw_str);
  *out_len = static_cast<tsoobgx::bst_ulong>(raw_str.length());
  API_END();
}

inline void tsooBGXDumpModelImpl(
    BoosterHandle handle,
    const FeatureMap& fmap,
    int with_stats,
    const char *format,
    tsoobgx::bst_ulong* len,
    const char*** out_models) {
  std::vector<std::string>& str_vecs = BGXAPIThreadLocalStore::Get()->ret_vec_str;
  std::vector<const char*>& charp_vecs = BGXAPIThreadLocalStore::Get()->ret_vec_charp;
  auto *bst = static_cast<Booster*>(handle);
  bst->LazyInit();
  str_vecs = bst->learner()->DumpModel(fmap, with_stats != 0, format);
  charp_vecs.resize(str_vecs.size());
  for (size_t i = 0; i < str_vecs.size(); ++i) {
    charp_vecs[i] = str_vecs[i].c_str();
  }
  *out_models = dmlc::BeginPtr(charp_vecs);
  *len = static_cast<tsoobgx::bst_ulong>(charp_vecs.size());
}
BGX_DLL int retsooBGXDumpModel(BoosterHandle handle,
                       const char* fmap,
                       int with_stats,
                       tsoobgx::bst_ulong* len,
                       const char*** out_models) {
  return retsooBGXDumpModelEx(handle, fmap, with_stats, "text", len, out_models);
}
BGX_DLL int retsooBGXDumpModelEx(BoosterHandle handle,
                       const char* fmap,
                       int with_stats,
                       const char *format,
                       tsoobgx::bst_ulong* len,
                       const char*** out_models) {
  API_BEGIN();
  CHECK_HANDLE();
  FeatureMap featmap;
  if (strlen(fmap) != 0) {
    std::unique_ptr<dmlc::Stream> fs(
        dmlc::Stream::Create(fmap, "r"));
    dmlc::istream is(fs.get());
    featmap.LoadText(is);
  }
  tsooBGXDumpModelImpl(handle, featmap, with_stats, format, len, out_models);
  API_END();
}

BGX_DLL int retsooBGXDumpModelWithFeatures(BoosterHandle handle,
                                   int fnum,
                                   const char** fname,
                                   const char** ftype,
                                   int with_stats,
                                   tsoobgx::bst_ulong* len,
                                   const char*** out_models) {
  return retsooBGXDumpModelExWithFeatures(handle, fnum, fname, ftype, with_stats,
                                   "text", len, out_models);
}
BGX_DLL int retsooBGXDumpModelExWithFeatures(BoosterHandle handle,
                                   int fnum,
                                   const char** fname,
                                   const char** ftype,
                                   int with_stats,
                                   const char *format,
                                   tsoobgx::bst_ulong* len,
                                   const char*** out_models) {
  API_BEGIN();
  CHECK_HANDLE();
  FeatureMap featmap;
  for (int i = 0; i < fnum; ++i) {
    featmap.PushBack(i, fname[i], ftype[i]);
  }
  tsooBGXDumpModelImpl(handle, featmap, with_stats, format, len, out_models);
  API_END();
}

BGX_DLL int retsooBGXGetAttr(BoosterHandle handle,
                     const char* key,
                     const char** out,
                     int* success) {
  auto* bst = static_cast<Booster*>(handle);
  std::string& ret_str = BGXAPIThreadLocalStore::Get()->ret_str;
  API_BEGIN();
  CHECK_HANDLE();
  if (bst->learner()->GetAttr(key, &ret_str)) {
    *out = ret_str.c_str();
    *success = 1;
  } else {
    *out = nullptr;
    *success = 0;
  }
  API_END();
}

BGX_DLL int retsooBGXSetAttr(BoosterHandle handle,
                     const char* key,
                     const char* value) {
  auto* bst = static_cast<Booster*>(handle);
  API_BEGIN();
  CHECK_HANDLE();
  if (value == nullptr) {
    bst->learner()->DelAttr(key);
  } else {
    bst->learner()->SetAttr(key, value);
  }
  API_END();
}

BGX_DLL int retsooBGXGetAttrNames(BoosterHandle handle,
                     tsoobgx::bst_ulong* out_len,
                     const char*** out) {
  std::vector<std::string>& str_vecs = BGXAPIThreadLocalStore::Get()->ret_vec_str;
  std::vector<const char*>& charp_vecs = BGXAPIThreadLocalStore::Get()->ret_vec_charp;
  auto *bst = static_cast<Booster*>(handle);
  API_BEGIN();
  CHECK_HANDLE();
  str_vecs = bst->learner()->GetAttrNames();
  charp_vecs.resize(str_vecs.size());
  for (size_t i = 0; i < str_vecs.size(); ++i) {
    charp_vecs[i] = str_vecs[i].c_str();
  }
  *out = dmlc::BeginPtr(charp_vecs);
  *out_len = static_cast<tsoobgx::bst_ulong>(charp_vecs.size());
  API_END();
}

BGX_DLL int retsooBGXLoadRabitCheckpoint(BoosterHandle handle,
                                 int* version) {
  API_BEGIN();
  CHECK_HANDLE();
  auto* bst = static_cast<Booster*>(handle);
  *version = rabit::LoadCheckPoint(bst->learner());
  if (*version != 0) {
    bst->Intialize();
  }
  API_END();
}

BGX_DLL int retsooBGXSaveRabitCheckpoint(BoosterHandle handle) {
  API_BEGIN();
  CHECK_HANDLE();
  auto* bst = static_cast<Booster*>(handle);
  if (bst->learner()->AllowLazyCheckPoint()) {
    rabit::LazyCheckPoint(bst->learner());
  } else {
    rabit::CheckPoint(bst->learner());
  }
  API_END();
}

/* hidden method; only known to C++ test suite */
const std::map<std::string, std::string>&
QueryBoosterConfigurationArguments(BoosterHandle handle) {
  CHECK_HANDLE();
  auto* bst = static_cast<Booster*>(handle);
  bst->LazyInit();
  return bst->learner()->GetConfigurationArguments();
}

// force link rabit
static DMLC_ATTRIBUTE_UNUSED int TSOOBGX_LINK_RABIT_C_API_ = RabitLinkTag();
