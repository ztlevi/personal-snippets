from datetime import *

# string to datetime object
time = "12:22"
dt = (datetime.strptime(time, '%H:%M') +
      timedelta(minutes=1)).strftime('%H:%M')

# unix to datetime object
timestamp = 1545730073
dt_object = datetime.fromtimestamp(timestamp)
print("dt_object =", dt_object)
print("type(dt_object) =", type(dt_object))
