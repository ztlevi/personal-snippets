# run python file with input and output
python test.py < input.txt > output.txt

# Multi-line input
import sys 
for line in sys.stdin: 
  for value in line.split(): 
    print value

# python 2
name = raw_input() # string type
num = input(35) # input gets evaluated
>>> num = 35
 
# python3 uses input()
name = input() # string type
num = eval(input(35)) 
>>> num = 35