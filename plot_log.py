import pylab as plt
import messages_pb2

container = messages_pb2.LogContainer()
f = open("./log_data/458_logdata_Uni1.bin", "r")
d = f.read()
f.close()
container.ParseFromString(d)

log_messages = container.log_data

sensors = [y.sensors for y in container.log_data if y.HasField('sensors')]
print "Logfile contains %i sensor logs" % len(sensors)
actuators = [y.actuators for y in container.log_data if y.HasField('actuators') ]
print "Logfile contains %i actuator logs" % len(actuators)
gps_llhs = [y.gps_llh for y in container.log_data if y.HasField('gps_llh')]
print "Logfile contains %i GPS (LAT/LONG/HEIGHT) logs" % len(gps_llhs)
servos = [y.servos for y in container.log_data if y.HasField('servos')]
print "Logfile contains %i servo logs" % len(servos)
print "#################################################"
gyros = [y.gyro for y in sensors if y.HasField('gyro')]
print "Logfile contains %i GYRO logs" % len(gyros)
accels = [y.accel for y in sensors if y.HasField('accel')]
print "Logfile contains %i ACCEL logs" % len(accels)
mags = [y.mag for y in sensors if y.HasField('mag')]
print "Logfile contains %i MAG logs" % len(mags)
gpss = [y.gps for y in sensors if y.HasField('gps')]
print "Logfile contains %i GPS logs" % len(gpss)
airspeeds = [y.airspeed for y in sensors if y.HasField('airspeed')]
print "Logfile contains %i AIRSPEED logs" % len(airspeeds)





def fromTimestamp(ts):
    return float(ts.tsec) + 1e-9*float(ts.tnsec)






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

plt.figure(5)
plt.subplot(311)
plt.title('mag with timestep')
plt.plot([fromTimestamp(y.timestamp) for y in mags], [y.data.x for y in mags])
plt.subplot(312)
plt.plot([fromTimestamp(y.timestamp) for y in mags], [y.data.y for y in mags])
plt.subplot(313)
plt.plot([fromTimestamp(y.timestamp) for y in mags], [y.data.z for y in mags])

plt.figure(6)
plt.subplot(311)
plt.title('mag with count')
plt.plot([k for k,y in enumerate(mags)], [y.data.x for y in mags])
plt.subplot(312)
plt.plot([k for k,y in enumerate(mags)], [y.data.y for y in mags])
plt.subplot(313)
plt.plot([k for k,y in enumerate(mags)], [y.data.z for y in mags])


plt.figure(7)
plt.subplot(311)
plt.title('GPS velocity with count')
plt.plot([k for k,y in enumerate(gpss)], [y.velocity.data.x for y in gpss])
plt.subplot(312)
plt.plot([k for k,y in enumerate(gpss)], [y.velocity.data.y for y in gpss])
plt.subplot(313)
plt.plot([k for k,y in enumerate(gpss)], [y.velocity.data.z for y in gpss])

plt.figure(8)
plt.subplot(311)
plt.title('GPS velocity with timestamp')
plt.plot([fromTimestamp(y.timestamp) for y in gpss], [y.velocity.data.x for y in gpss])
plt.subplot(312)
plt.plot([fromTimestamp(y.timestamp) for y in gpss], [y.velocity.data.y for y in gpss])
plt.subplot(313)
plt.plot([fromTimestamp(y.timestamp) for y in gpss], [y.velocity.data.z for y in gpss])


plt.figure(9)
plt.subplot(311)
plt.title('GPS position with count')
plt.plot([k for k,y in enumerate(gpss)], [y.position.data.x for y in gpss])
plt.subplot(312)
plt.plot([k for k,y in enumerate(gpss)], [y.position.data.y for y in gpss])
plt.subplot(313)
plt.plot([k for k,y in enumerate(gpss)], [y.position.data.z for y in gpss])

plt.figure(10)
plt.subplot(311)
plt.title('GPS position with timestamp')
plt.plot([fromTimestamp(y.timestamp) for y in gpss], [y.position.data.x for y in gpss])
plt.subplot(312)
plt.plot([fromTimestamp(y.timestamp) for y in gpss], [y.position.data.y for y in gpss])
plt.subplot(313)
plt.plot([fromTimestamp(y.timestamp) for y in gpss], [y.position.data.z for y in gpss])

plt.show()
