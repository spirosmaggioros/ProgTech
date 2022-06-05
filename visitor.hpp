#ifndef __VISITOR_HPP__
#define __VISITOR_HPP__

template <typename T>
class Visitor {
public:
  virtual ~Visitor() {}
  virtual void visit(T &x) = 0;
  virtual bool finished() const { return false; }
};

template <typename T>
class Visitable {
public:
  virtual void accept(Visitor<T> &visitor) = 0;
};

#endif
