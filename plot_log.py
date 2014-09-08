import pylab as plt
import messages_pb2

x = messages_pb2.LogData()
f = open("300_logdata.bin", "r")
d = f.read()
f.close()
x.ParseFromString(d)

sensors = x.sensor_data
actuators = x.actuator_data




def fromTimestamp(ts):
    return float(ts.tsec) + 1e-9*float(ts.tnsec)

gyros = [y.gyro for y in sensors.data if y.type == 1]
accels = [y.accel for y in sensors.data if y.type == 1]

plt.figure(1)
plt.subplot(311)
plt.title('gyro with timestep')
plt.plot([fromTimestamp(y.timestamp) for y in gyros], [y.data.x for y in gyros])
plt.subplot(312)
plt.plot([fromTimestamp(y.timestamp) for y in gyros], [y.data.y for y in gyros])
plt.subplot(313)
plt.plot([fromTimestamp(y.timestamp) for y in gyros], [y.data.z for y in gyros])

plt.figure(2)
plt.subplot(311)
plt.title('gyro with count')
plt.plot([k for k,y in enumerate(gyros)], [y.data.x for y in gyros])
plt.subplot(312)
plt.plot([k for k,y in enumerate(gyros)], [y.data.y for y in gyros])
plt.subplot(313)
plt.plot([k for k,y in enumerate(gyros)], [y.data.z for y in gyros])

plt.figure(3)
plt.subplot(311)
plt.title('accel with timestep')
plt.plot([fromTimestamp(y.timestamp) for y in accels], [y.data.x for y in accels])
plt.subplot(312)
plt.plot([fromTimestamp(y.timestamp) for y in accels], [y.data.y for y in accels])
plt.subplot(313)
plt.plot([fromTimestamp(y.timestamp) for y in accels], [y.data.z for y in accels])

plt.figure(4)
plt.subplot(311)
plt.title('accel with count')
plt.plot([k for k,y in enumerate(accels)], [y.data.x for y in accels])
plt.subplot(312)
plt.plot([k for k,y in enumerate(accels)], [y.data.y for y in accels])
plt.subplot(313)
plt.plot([k for k,y in enumerate(accels)], [y.data.z for y in accels])

plt.show()
