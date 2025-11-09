#!/usr/bin/env python3

import numpy as np
import random

# change 3 to 5 in mamca_last_2_3.txt to work with data obtained using osculating circle with r=5
with open("mamca_last_2_3.txt") as f: #this file is the file where all components are merged together (see section "Application")
    lines = f.readlines()
    for i in range(50): #50 for test
        random.seed(i)
        with open(f'mamca_test_3_{i}.txt', 'w') as g: # change to test accordingly
            result = random.sample(lines, 250) #250 for mamca, 60 for masto
            g.writelines(result)



