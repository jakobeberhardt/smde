import numpy
import simpy
import random


class MonitoredResource(simpy.Resource):
    def __init__(self, env, capacity=1):
        super().__init__(env, capacity)
        self.env = env
        self.queue_sizes = []
        self.waiting_times = []
        self.timings = []

    def request(self, *args, **kwargs):
        req = super().request(*args, **kwargs)
        self.queue_sizes.append(len(self.queue))
        self.timings.append(self.env.now)
        req.time_queued = self.env.now
        req.callbacks.append(self._record_waiting_time)
        return req

    def _record_waiting_time(self, event):
        waiting_time = self.env.now - event.time_queued
        self.waiting_times.append(waiting_time)


class Runner(object):
    def __init__(self, env, distances, queues, usage_times,  name):
        self.env = env
        # Start the run process everytime an instance is created.
        self.action = env.process(self.run())
        self.distances = distances
        self.queues = queues
        self.usage_times = usage_times
        self.name = name

    def run(self):
        # print("{} {} Start marathon".format( self.env.now, self.name))
        for i in range(0, len(self.distances)):
            # print("{} starting stage {}".format(self.env.now , i))
            yield self.env.timeout(running_time(self.distances[i]))
            yield from self.use_stations(i)
            
    def use_stations(self, index):
        keys =  list(self.queues[index].keys())
        random.shuffle(keys)
        print(keys)
        for station in keys:
            with self.queues[index][station].request() as stn:
                yield stn
                print("{} {}: start using {}".format(self.env.now, self.name, station))
                yield self.env.timeout(self.usage_times[station]())
                print("{} {}: stop using {}".format(self.env.now, self.name, station))



running_time = lambda distance: numpy.random.normal(11933.0 / 42.0 * distance, 1200 / 42.0 * distance)
water_drinking_time = lambda: max(numpy.random.normal(30, 5), 5)
meal_time = lambda: max(numpy.random.normal(60, 15), 10)
toilet_time = lambda: max(numpy.random.normal(45, 10), 10)


env = simpy.Environment()


# Resources


resources = [
    {
        "meal": MonitoredResource(env, 10),
        "water": MonitoredResource(env, 10),
        "toilet": MonitoredResource(env, 10),
    },
    {
        "meal": MonitoredResource(env, 10),
        "water": MonitoredResource(env, 10),
        "toilet": MonitoredResource(env, 10),
    },
    {
        "meal": MonitoredResource(env, 10),
        "water": MonitoredResource(env, 10),
        "toilet": MonitoredResource(env, 10),
    },
    {
        "meal": MonitoredResource(env, 10),
        "water": MonitoredResource(env, 10),
        "toilet": MonitoredResource(env, 10),
    },
]

usage_times = {"water": water_drinking_time, "meal": meal_time, "toilet": toilet_time}

distances = [10.0, 10.0, 10.0, 10.0]

# create 200 runners

for i in range(1, 2):
    Runner(env, distances= distances, queues=resources, usage_times= usage_times, name= "runner %i" % i)


# running simulation


env.run()
