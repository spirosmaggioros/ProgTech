#ifndef __CONTAINER_HPP__
#define __CONTAINER_HPP__

#include <typeinfo>

template <typename T>
class Container {
public:
  virtual ~Container() {}
  virtual int size() const = 0;
  virtual bool empty() const { return size() == 0; }
  virtual void clear() = 0;
};

template <typename T>
class Iterator {
public:
  Iterator(const Iterator &i) : impl(i.impl->clone()) {}
  ~Iterator() { delete impl; }
  T & operator*() { return impl->access(); }
  // prefix ++, i.e. ++i
  Iterator & operator++() {
    impl->advance();
    return *this;
  }
  // postfix ++, i.e. i++
  Iterator operator++(int) {
    Iterator result(*this);
    impl->advance();
    return result;
  }

  Iterator & operator=(const Iterator &i) {
    delete impl;
    impl = i.impl->clone();
    return *this;
  }

  bool operator==(const Iterator &i) const {
    return typeid(*this) == typeid(i) && impl->equal(*(i.impl));
  }
  bool operator!=(const Iterator &i) const {
    return !(*this == i);
  };

  class Impl {
  public:
    virtual ~Impl() {}
    virtual Impl * clone() const = 0;
    virtual T & access() const = 0;
    virtual void advance() = 0;
    virtual bool equal(const Impl &i) const = 0;
  };

  Iterator(Impl *i) : impl(i) {}
  const Impl * getImpl() const { return impl; }

private:
  Impl *impl;
};

template <typename T>
class Iterable {
public:
  virtual Iterator<T> begin() = 0;
  virtual Iterator<T> end() = 0;
};

#endif
