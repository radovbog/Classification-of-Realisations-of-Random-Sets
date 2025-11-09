
#Program for convering binary images from .txt to .png format


import numpy as np
import matplotlib.pyplot as plt
import sys

arr = []

with open(sys.argv[1] + '.txt', 'r') as f:
        for line in f.readlines():
            arr.append(line.split(' '))

arr = np.array(arr)

arr[0, 0] = arr[0, 0].strip('output:')
arr = arr[:, :-1]
arr = arr.astype('int')
tmp = arr == 0
arr[arr != 0] = 0
arr[tmp] = 255
arr_min, arr_max = 1, np.max(arr)


fig = plt.figure(figsize=(20, 12))
fig = plt.imshow(arr, cmap='gray', vmin=np.min(arr), vmax=np.max(arr))
fig.axes.get_xaxis().set_visible(False)
fig.axes.get_yaxis().set_visible(False)
plt.axis('off')
plt.savefig(sys.argv[1] + '.png', bbox_inches='tight')