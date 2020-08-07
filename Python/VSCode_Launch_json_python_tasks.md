# Tasks

```json
({
  "name": "Python: XXX",
  "type": "python",
  "request": "launch",
  "program": "/code/detection/python/private/experiments/traffic_lights/experiment.py",
  "args": [],
  "env": {
    "AA": "BB"
  },
  "console": "integratedTerminal"
},
{
  "name": "PyTest AAA",
  "type": "python",
  "request": "launch",
  "module": "pytest",
  "args": ["-sv"],
  "program": "${workspaceFolder}/.../AAA.py::test_function_B",
  "console": "integratedTerminal"
})
```

## Fix start client

```python
import multiprocessing

multiprocessing.set_start_method("spawn", True)

```
