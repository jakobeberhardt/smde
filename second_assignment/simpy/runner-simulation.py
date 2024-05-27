import numpy
import simpy



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
    def __init__(self, env, water_queue):
        self.env = env
        # Start the run process everytime an instance is created.
        self.action = env.process(self.run())
        self.water_queue = water_queue

    def run(self):
        print("%d Start marathon" % self.env.now)

        # to wait for it to finish
        yield self.run_stage()
        print("%d ran 10k" % self.env.now)
        with self.water_queue.request() as req:
            yield req
            print("%d starting to drink water" % self.env.now)
            yield self.env.timeout(water_drinking_time())
            print("%d finished drinking water" % self.env.now)

    def run_stage(self):
        return self.env.timeout(running_time())



running_time = lambda: numpy.random.normal(
    11933.0 / 4.2, 1200 / 4.2
)
water_drinking_time = lambda: max(numpy.random.normal(30, 5), 5)
meal_time = lambda: max(numpy.random.normal(60, 15), 10)
toilet_time = lambda: max(numpy.random.normal(45, 10), 10)



env = simpy.Environment()


# Resources


resources = [
    {
        "meal": MonitoredResource(env, 10),
        "water": MonitoredResource(env, 10),
        "toilet": MonitoredResource(env, 10)
    },
    {
        "meal": MonitoredResource(env, 10),
        "water": MonitoredResource(env, 10),
        "toilet": MonitoredResource(env, 10)
    },
    {
        "meal": MonitoredResource(env, 10),
        "water": MonitoredResource(env, 10),
        "toilet": MonitoredResource(env, 10)
    },
    {
        "meal": MonitoredResource(env, 10),
        "water": MonitoredResource(env, 10),
        "toilet": MonitoredResource(env, 10)
    }
]



# create 200 runners

for i in range(1, 1000):
    Runner(env, water_queue=resources[0]["water"])


# running simulation


env.run()
