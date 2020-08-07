## https://code.angularjs.org/1.7.0/docs/tutorial/step_11

# Built-in filters

- all filters can be found at https://code.angularjs.org/1.7.0/docs/api/ng/filter
- examples

```
{{'lower cap string' | uppercase}}
{{{foo: 'bar', baz: 42} | json}}
{{1459461289000 | date}}
{{1459461289000 | date:'MM/dd/yyyy @ h:mma'}}
```

# Custom filters

## The checkmark Filter

Since this filter is generic (i.e. it is not specific to any view or component), we are going to register it in a core
module, which contains "application-wide" features.

app/core/core.module.js:

```
angular.module('core', []);
```

app/core/checkmark/checkmark.filter.js:

```
angular.
  module('core').
  filter('checkmark', function() {
    return function(input) {
      return input ? '\u2713' : '\u2718';
    };
  });
```

As you may have noticed, we (unsurprisingly) gave our file a .filter suffix.

The name of our filter is "checkmark". The input evaluates to either true or false, and we return one of the two unicode
characters we have chosen to represent true (\\u2713 -> ✓) and false (\\u2718 -> ✘).
