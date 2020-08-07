## OS

```python
import os
myCmd = 'ls -la'
os.system(myCmd)
```

```python
import os
myCmd = os.popen('ls -la').read()
print(myCmd)
```

## subprocess

### Print in the end

```python
import subprocess

proc = subprocess.Popen(["cat", "/etc/services"], stdout=subprocess.PIPE, shell=True)
(out, err) = proc.communicate()
print("program output:", out)
```

### Print in realtime

```python
from subprocess import Popen, PIPE

def run_command(command):
    process = Popen(command, stdout=PIPE, shell=True)
    while True:
        line = process.stdout.readline().rstrip()
        if not line:
            break
        yield line
```
