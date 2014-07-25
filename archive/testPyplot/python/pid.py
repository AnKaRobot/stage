import numpy as np
import matplotlib.pyplot as plt
import time
from collections import deque

proportionnalGain = 0.2
targetSpeed = 100

def pid (current, target, gain) :
    return current + (gain * error(current, target))

def error (current, target) :
    return target - current

fig1 = plt.figure()
axes1 = fig1.add_subplot(2, 1, 1) # two rows, one column, first plot

speedValues = deque([0])
timeValues = deque([0.])
currentSpeed = 0
currentTime = 0
maxSpeed = 120
minSpeed = -120
timeStep = 0.1
maxTime = timeStep
minTime = 0

line, = axes1.plot(speedValues, timeValues, color = "blue")

# del axes1.lines[0] -> remove lines

xText = axes1.set_xlabel("time")
ytext = axes1.set_ylabel("speed")

axes1.axis([minTime, maxTime,
            minSpeed, maxSpeed])

axes1.axhspan(targetSpeed, targetSpeed)

fig1.show()

timeExecutionLimit = 5 / timeStep
while timeExecutionLimit :
    time.sleep(timeStep)
    timeExecutionLimit = timeExecutionLimit - 1
    
    newSpeed = pid(currentSpeed, targetSpeed, proportionnalGain)
    newTime = currentTime + timeStep

    speedValues.appendleft(newSpeed)
    timeValues.appendleft(newTime)

    line.set_ydata(speedValues)
    line.set_xdata(timeValues)

    if newSpeed > maxSpeed : maxSpeed = newSpeed;
    elif newSpeed < minSpeed : minSpeed = newSpeed;

    if newTime > maxTime : maxTime = newTime;
    elif newTime < minTime : minTime = newTime;

    axes1.axis([minTime, maxTime,
                minSpeed, maxSpeed])

    plt.draw()
    #plt.pause(0.0001)  -> trusted to resolve some bugs sometimes

    currentSpeed = newSpeed
    currentTime = newTime


plt.show()
