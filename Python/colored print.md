## ‘termcolor’ module

termcolor is a python module for ANSII Color formatting for output in terminal.

```
# Python program to print
# colored text and background
import sys
from termcolor import colored, cprint

text = colored('Hello, World!', 'red', attrs=['reverse', 'blink'])
print(text)
cprint('Hello, World!', 'green', 'on_red')

print_red_on_cyan = lambda x: cprint(x, 'red', 'on_cyan')
print_red_on_cyan('Hello, World!')
print_red_on_cyan('Hello, Universe!')

for i in range(10):
    cprint(i, 'magenta', end=' ')

cprint("Attention!", 'red', attrs=['bold'], file=sys.stderr)
```

## ‘colorama’ module

Cross-platform printing of colored text can then be done using Colorama’s constant shorthand for
ANSI escape sequences:

```
# Python program to print
# red text with green background

from colorama import Fore, Back, Style
print(Fore.RED + 'some red text')
print(Back.GREEN + 'and with a green background')
print(Style.DIM + 'and in dim text')
print(Style.RESET_ALL)
print('back to normal now')
filter_none
edit
play_arrow

brightness_4
# Python program to print
# green text with red background

from colorama import init
from termcolor import colored

init()

print(colored('Hello, World!', 'green', 'on_red'))
```
