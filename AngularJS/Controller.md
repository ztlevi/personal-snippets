1. $inject标记：要允许压缩类库重命名函数参数，同时注入器又能正确处理依赖的话，函数需要使用$inject 属
   性。这个属性是一个包含依赖的名称的数组。

```
var MyController = function(renamed$scope, renamedGreeter) {}
MyController.$inject = ['$scope', 'greeter'];
```

注意 \$inject 标记里的值和函数声明的参数是对应的。这种方式适合用于控制器的声明，因为控制器有了明确的
声明标记。

2. 行内标记

```
someModule.factory('greeter', ['$window', function(renamed$window) {...}]);
```
