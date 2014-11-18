import pylab as plt
import numpy as np



with open('accel.txt', 'rb') as npfile:
    data = []
    data = np.loadtxt(npfile, delimiter=';')
   
            

    plt.figure(1)
    plt.subplot(311)
    plt.title('accel with timestep')
    plt.plot(data[:,0],data[:,1])
    plt.subplot(312)
    plt.plot(data[:,0],data[:,2])
    plt.subplot(313)
    plt.plot(data[:,0],data[:,3])

    plt.show()
