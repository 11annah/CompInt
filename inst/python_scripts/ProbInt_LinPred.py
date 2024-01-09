from functools import reduce
import operator
import torch
with open('inst/python_scripts/InverseFunctions.py', 'r') as f:
    script_code = f.read()

exec(script_code)
