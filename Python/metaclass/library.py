# library.py

# Method 1
class BaseMeta(type):
    def __new__(cls, name, bases, body):
        if name != 'Base' and 'bar' not in body:
            raise TypeError("bad user class")
        return super().__new__(cls, name, bases, body)

# Method 2
class Base(metaclass=BaseMeta):
    def foo(self):
        return self.bar()

    def __init_subclass__(cls, **kw) -> None:
        assert hasattr(cls, "bar")
        return super().__init_subclass__(**kw)

# Method 3
# old_bc = __build_class__
# def my_bc(func, name, base=None, **kw):
#     if base is Base:
#         print('Check if bar method defined')
#     return old_bc(func, name, base, **kw)
# import builtins

# builtins.__build_class__ = my_bc
