import numpy as np
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
    def __init__(
        self,
        env,
        name,
        running_time,
        distances,
        queues,
        usage_times,
        need_functions,
        need_satisfaction,
        average_waiting_times,
        skip_if_waiting_time_higher,
    ):
        self.env = env
        # Start the run process everytime an instance is created.
        self.running_time = running_time
        self.distances = distances
        self.queues = queues
        self.usage_times = usage_times
        self.name = name
        self.need_functions = need_functions
        self.need_satisfaction = need_satisfaction
        self.needs = {k: 0.0 for k in need_functions.keys()}
        self.previous_times = {k: env.now for k in need_functions.keys()}
        self.skip_if_waiting_time_higher = skip_if_waiting_time_higher
        self.average_waiting_times = average_waiting_times
        self.skipped_needs = {k: [] for k in need_functions.keys()}
        self.current_needs = {k: [] for k in need_functions.keys()}
        self.cummulative_usage_time = {k: 0 for k in usage_times.keys()}
        self.stop_times = []

        self.action = env.process(self.run())

    def run(self):
        # print("{} {} Start marathon".format( self.env.now, self.name))
        for i in range(0, len(self.distances)):
            # print("{} starting stage {}".format(self.env.now , i))
            yield self.env.timeout(self.running_time(self.distances[i]))
            yield from self.use_stations(i)
        self.finishing_time = self.env.now

    def use_stations(self, index):
        self.stop_times.append(self.env.now)
        for station in self.skipped_needs:
            self.skipped_needs[station].append(None)
        for station in self.needs:
            self.update_need(station)
            self.current_needs[station].append(self.needs[station])

        keys = list(self.queues[index].keys())

        random.shuffle(keys)

        for station in keys:
            yield from self.use_station(index, station)

    def use_station(self, index, station):
        skip_due_to_queue_size = (
            self.get_waiting_time(index, station)
            > self.skip_if_waiting_time_higher[station]
            and not self.needs[station] >= 1.0
        )
        if (
            random.uniform(0, 1) < self.needs[station] and not skip_due_to_queue_size
        ):  # if we want to go but have to skip
            with self.queues[index][station].request() as stn:
                yield stn
                usage_time = self.usage_times[station]()
                self.cummulative_usage_time[station] += usage_time
                yield self.env.timeout(usage_time)
            self.needs[station] = self.need_satisfaction[station](self.needs[station])
            self.previous_times[station] = self.env.now
        elif skip_due_to_queue_size:
            self.skipped_needs[station][index] = self.needs[station]

    def get_waiting_time(self, index, station):
        return (
            len(self.queues[index][station].queue) * self.average_waiting_times[station]
        )

    def update_need(self, station):
        time_difference = self.env.now - self.previous_times[station]
        self.needs[station] += self.need_functions[station](time_difference)
        self.previous_times[station] = self.env.now


marathon_distance = 42.0

average_usage_times = {
    "meal": 5,
    "water": 5,
    "toilet": 40,
    "medical": 420,
}

skip_if_waiting_time_higher = {
    "meal": 120,
    "water": 120,
    "toilet": 180,
    "medical": 600,
}

water_drinking_time = lambda: max(np.random.normal(average_usage_times["water"], 2), 1)
meal_time = lambda: max(np.random.normal(average_usage_times["meal"], 2), 1)
toilet_time = lambda: max(np.random.normal(average_usage_times["toilet"], 10), 5)
medical_time = lambda: max(np.random.normal(average_usage_times["medical"], 100), 30)

usage_times = {
    "water": water_drinking_time,
    "meal": meal_time,
    "toilet": toilet_time,
    "medical": medical_time,
}

need_functions = {
    "meal": lambda x: x / 3600.0,  # once per hour
    "water": lambda x: x / 3600.0 * 3.0,  # three times per hour
    "toilet": lambda x: x / 3600.0 / 2.0,  # once per 2 hours
    "medical": lambda x: x
    / 3600.0
    / 1000,  # over the course of the race, 1 in 5 runners needs medical attention
}

need_satisfaction = {
    "meal": lambda x: x - 1.0,
    "water": lambda x: x - 1.0,
    "toilet": lambda x: x - 1.0,
    "medical": lambda x: x - 1.0,
}

# Resources

# – Km5: WC, Water, Isotonic drink and Medical sevices
# – Km10: WC, Water, Isotonic drink and Medical sevices
# – Km 15: WC, Water, Isotonic drink, Medical sevices and Finsher Gel
# – Km 20: WC, Water, Isotonic drink, Medical sevices, Finsher Gel and Banana
# – Km 25: WC, Water, Isotonic drink, Medical sevices, Finsher Gel and Banana
# – Km 27,5: WC, Water, and Isotonic drink
# – Km 30: WC, Water, Isotonic drink, Medical sevices, Finsher Gel and Banana
# – Km 32,5: WC, Water, and Isotonic drink
# – Km 35: WC, Water, Isotonic drink, Medical sevices, Finsher Gel and Banana
# – Km 37,5: WC, Water, Isotonic drink, and Finsher Gel
# – Km 40: WC, Water, Isotonic drink, Medical sevices and Banana


def assign_resources(
    env, meal_capacity=10, water_capacity=10, toilet_capacity=10, medical_capacity=10
):
    return [
        {  # km 5
            "water": MonitoredResource(env, water_capacity),
            "toilet": MonitoredResource(env, toilet_capacity),
            "medical": MonitoredResource(env, medical_capacity),
        },
        {  # km 10
            "water": MonitoredResource(env, water_capacity),
            "toilet": MonitoredResource(env, toilet_capacity),
            "medical": MonitoredResource(env, medical_capacity),
        },
        {  # km 15
            "meal": MonitoredResource(env, meal_capacity),
            "water": MonitoredResource(env, water_capacity),
            "toilet": MonitoredResource(env, toilet_capacity),
            "medical": MonitoredResource(env, medical_capacity),
        },
        {  # km 20
            "meal": MonitoredResource(env, meal_capacity),
            "water": MonitoredResource(env, water_capacity),
            "toilet": MonitoredResource(env, toilet_capacity),
            "medical": MonitoredResource(env, medical_capacity),
        },
        {  # km 25
            "meal": MonitoredResource(env, meal_capacity),
            "water": MonitoredResource(env, water_capacity),
            "toilet": MonitoredResource(env, toilet_capacity),
            "medical": MonitoredResource(env, medical_capacity),
        },
        {  # km 27.5
            "water": MonitoredResource(env, water_capacity),
            "toilet": MonitoredResource(env, toilet_capacity),
        },
        {  # km 30
            "meal": MonitoredResource(env, meal_capacity),
            "water": MonitoredResource(env, water_capacity),
            "toilet": MonitoredResource(env, toilet_capacity),
            "medical": MonitoredResource(env, medical_capacity),
        },
        {  # km 32.5
            "water": MonitoredResource(env, water_capacity),
            "toilet": MonitoredResource(env, toilet_capacity),
        },
        {  # km 35
            "meal": MonitoredResource(env, meal_capacity),
            "water": MonitoredResource(env, water_capacity),
            "toilet": MonitoredResource(env, toilet_capacity),
            "medical": MonitoredResource(env, medical_capacity),
        },
        {  # km 37.5
            "meal": MonitoredResource(env, meal_capacity),
            "water": MonitoredResource(env, water_capacity),
            "toilet": MonitoredResource(env, toilet_capacity),
        },
        {  # km 40
            "meal": MonitoredResource(env, meal_capacity),
            "water": MonitoredResource(env, water_capacity),
            "toilet": MonitoredResource(env, toilet_capacity),
            "medical": MonitoredResource(env, medical_capacity),
        },
        {},  # Finishing line
    ]


distances = [5.0, 5.0, 5.0, 5.0, 5.0, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.0]


def generate_running_function(mean_total_running_time, running_time_stddev):
    expected_finishing_time = np.random.normal(
        mean_total_running_time, running_time_stddev
    )
    return lambda distance: expected_finishing_time * distance / marathon_distance


def run_simulation(
    mean_total_running_time,
    usage_times,
    need_functions,
    meal_capacity=10,
    water_capacity=10,
    toilet_capacity=10,
    medical_capacity=10,
):
    env = simpy.Environment()
    resources = assign_resources(
        env,
        meal_capacity=meal_capacity,
        water_capacity=water_capacity,
        toilet_capacity=toilet_capacity,
        medical_capacity=medical_capacity,
    )

    running_time_stddev = 1200

    # create runners
    runners = []
    for i in range(0, 8303):
        runners.append(
            Runner(
                env,
                name="runner %i" % i,
                running_time=generate_running_function(
                    mean_total_running_time, running_time_stddev
                ),
                distances=distances,
                queues=resources,
                usage_times=usage_times,
                need_functions=need_functions,
                need_satisfaction=need_satisfaction,
                average_waiting_times=average_usage_times,
                skip_if_waiting_time_higher=skip_if_waiting_time_higher,
            )
        )

    # running simulation
    env.run()
    return (runners, resources)


runners, resources = run_simulation(
    11933.0,
    usage_times,
    need_functions,
    meal_capacity=40,
    water_capacity=80,
    toilet_capacity=45,
    medical_capacity=5,
)
