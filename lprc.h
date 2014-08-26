// -*- C++ -*-

#pragma once

#include <cstddef>

namespace lpp {

template <typename SubClass>
class RefCount {
 public:
  const SubClass *IncRef() const {
    ++refcount_;
    return reinterpret_cast<const SubClass *>(this);
  }

  void DecRef() const {
    if (--refcount_ == 0) delete this;
  }

 protected:
  RefCount() : refcount_(1) {}
  virtual ~RefCount() {}

 private:
  mutable size_t refcount_;
};

}  // namespace lpp
