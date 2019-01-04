# Found at https://www.r-bloggers.com/getting-numpy-data-into-r/

import struct
import os
import numpy as np

inputfile = "./words25.npy"
outputfile = "./words25.bin"

# load from the file
mat = np.load(inputfile)

# create a binary file
binfile = open(outputfile, 'wb')

# and write out two integers with the row and column dimension
print("Matrix shape : ", mat.shape)
header = struct.pack('2I', mat.shape[0], mat.shape[1])
binfile.write(header)

# then loop over columns and write each
for i in range(mat.shape[1]):
    data = struct.pack('%id' % mat.shape[0], *mat[:,i])
    binfile.write(data)
		
binfile.close()

print("Done! Press any key...")
raw_input()