# Dreaded Diamond

The "dreaded diamond" refers to a class structure in which a particular class appears more than once in a class's
inheritance hierarchy. For example,

<pre>
class Base {
public:
  _..._
protected:
  int data_;
};

class Der1 : public Base { ... };

class Der2 : public Base { ... };

class Join : public Der1, public Der2 {
public:
  void method()
  {
     data_ = 1;  _<big>←</big> bad: this is ambiguous; see below_
  }
};

int main()
{
  Join* j = new Join();
  Base* b = j;   _<big>←</big> bad: this is ambiguous; see below_
}
</pre>

Forgive the ASCII-art, but the inheritance hierarchy looks something like this:

<pre>
                         Base
                         /  \
                        /    \
                       /      \
                    Der1      Der2
                       \      /
                        \    /
                         \  /
                         Join
</pre>

Before we explain why the dreaded diamond is dreaded, it is important to note that C++ provides techniques to deal with
each of the "dreads." In other words, this structure is often _called_ the dreaded diamond, but it really isn't dreaded;
it's more just something to be aware of.

The key is to realize that <tt>Base</tt> is inherited twice, which means any data members declared in <tt>Base</tt>,
such as <tt>data*</tt> above, will appear twice within a <tt>Join</tt> object. This can create ambiguities: which
<tt>data*</tt> did you want to change? For the same reason the conversion from <tt>Join*</tt> to <tt>Base*</tt>, or from
<tt>Join&amp;</tt> to <tt>Base&amp;</tt>, is ambiguous: which <tt>Base</tt> class subobject did you want?
