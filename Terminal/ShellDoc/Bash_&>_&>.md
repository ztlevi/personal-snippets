The operators we are using here are:

- `>` Syntax: _file_descriptor_<sub>opt</sub> `>` _file_name_
- `>&` Syntax: _file_descriptor_<sub>opt</sub> `>&` _file_descriptor_
- `&>` Syntax: `&>` _file_name_

If the file descriptor is omitted, the default is `0` (stdin) for input, or `1` (stdout) for output. `2` means stderr.

So we have:

- `>name` means `1>name` -- redirect stdout to the file `name`
- `&>name` is like `1>name 2>name` -- redirect stdout and stderr to the file `name` (however `name` is only opened once;
  if you actually wrote `1>name 2>name` it'd try to open `name` twice and perhaps malfunction).
