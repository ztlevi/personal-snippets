# more examples https://krzysztofzuraw.com/blog/2016/python-class-decorators.html
class decorator:
    def __init__(self, func):
        self.func = func
 
    def __call__(self, *args):
        print('Called {func} with args: {args}'.format(func=self.func.__name__,
                                                       args=args))
        return self.func(*args)
 
@decorator
def func(x,y):
    return x,y
 
if __name__ == '__main__':
    func(1,2)
