import pandas as pd
import scipy

import numpy as np
import simpy
import random
import math

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
        for i in range(0, len(self.distances)):
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


marathon_distance = 42.195

average_usage_times = {
    "meal": 5,
    "water": 5,
    "toilet": 40,
    "medical": 420,
}

skip_if_waiting_time_higher = {
    "meal": math.inf,
    "water": math.inf,
    "toilet": math.inf,
    "medical": math.inf,
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
    env, meal_capacities, water_capacities, toilet_capacities, medical_capacities
):
    return [
        {  # km 5
            "water": MonitoredResource(env, water_capacities[0]),
            "toilet": MonitoredResource(env, toilet_capacities[0]),
            "medical": MonitoredResource(env, medical_capacities[0]),
        },
        {  # km 10
            "water": MonitoredResource(env, water_capacities[1]),
            "toilet": MonitoredResource(env, toilet_capacities[1]),
            "medical": MonitoredResource(env, medical_capacities[1]),
        },
        {  # km 15
            "meal": MonitoredResource(env, meal_capacities[2]),
            "water": MonitoredResource(env, water_capacities[2]),
            "toilet": MonitoredResource(env, toilet_capacities[2]),
            "medical": MonitoredResource(env, medical_capacities[2]),
        },
        {  # km 20
            "meal": MonitoredResource(env, meal_capacities[3]),
            "water": MonitoredResource(env, water_capacities[3]),
            "toilet": MonitoredResource(env, toilet_capacities[3]),
            "medical": MonitoredResource(env, medical_capacities[3]),
        },
        {  # km 25
            "meal": MonitoredResource(env, meal_capacities[4]),
            "water": MonitoredResource(env, water_capacities[4]),
            "toilet": MonitoredResource(env, toilet_capacities[4]),
            "medical": MonitoredResource(env, medical_capacities[4]),
        },
        {  # km 27.5
            "water": MonitoredResource(env, water_capacities[5]),
            "toilet": MonitoredResource(env, toilet_capacities[5]),
        },
        {  # km 30
            "meal": MonitoredResource(env, meal_capacities[6]),
            "water": MonitoredResource(env, water_capacities[6]),
            "toilet": MonitoredResource(env, toilet_capacities[6]),
            "medical": MonitoredResource(env, medical_capacities[6]),
        },
        {  # km 32.5
            "water": MonitoredResource(env, water_capacities[7]),
            "toilet": MonitoredResource(env, toilet_capacities[7]),
        },
        {  # km 35
            "meal": MonitoredResource(env, meal_capacities[8]),
            "water": MonitoredResource(env, water_capacities[8]),
            "toilet": MonitoredResource(env, toilet_capacities[8]),
            "medical": MonitoredResource(env, medical_capacities[8]),
        },
        {  # km 37.5
            "meal": MonitoredResource(env, meal_capacities[9]),
            "water": MonitoredResource(env, water_capacities[9]),
            "toilet": MonitoredResource(env, toilet_capacities[9]),
        },
        {  # km 40
            "meal": MonitoredResource(env, meal_capacities[10]),
            "water": MonitoredResource(env, water_capacities[10]),
            "toilet": MonitoredResource(env, toilet_capacities[10]),
            "medical": MonitoredResource(env, medical_capacities[10]),
        },
        {},  # Finishing line
    ]


distances = [5.0, 5.0, 5.0, 5.0, 5.0, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.195]
num_stations = 11


def generate_running_function(mean_total_running_time, running_time_stddev):
    expected_finishing_time = np.random.normal(
        mean_total_running_time, running_time_stddev
    )
    return lambda distance: expected_finishing_time * distance / marathon_distance


def run_simulation_with_capacities(
    mean_total_running_time,
    usage_times,
    need_functions,
    meal_capacities,
    water_capacities,
    toilet_capacities,
    medical_capacities,
    num_runners = 8303
):
    env = simpy.Environment()
    resources = assign_resources(
        env,
        meal_capacities=meal_capacities,
        water_capacities=water_capacities,
        toilet_capacities=toilet_capacities,
        medical_capacities=medical_capacities,
    )

    running_time_stddev = 1200

    # create runners
    runners = []
    for i in range(0, num_runners):
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


def run_simulation(
    mean_total_running_time,
    usage_times,
    need_functions,
    meal_capacity=10,
    water_capacity=10,
    toilet_capacity=10,
    medical_capacity=10,
):
    meal_capacities = [meal_capacity] * num_stations
    water_capacities = [water_capacity] * num_stations
    toilet_capacities = [toilet_capacity] * num_stations
    medical_capacities = [medical_capacity] * num_stations
    return run_simulation_with_capacities(
        mean_total_running_time,
        usage_times,
        need_functions,
        meal_capacities = meal_capacities,
        water_capacities = water_capacities,
        toilet_capacities = toilet_capacities,
        medical_capacities = medical_capacities,
    )


runners, resources = run_simulation(
    11933.0,
    usage_times,
    need_functions,
    meal_capacity=40,
    water_capacity=80,
    toilet_capacity=45,
    medical_capacity=5,
)
resource_names = ['meal', 'medical', 'toilet', 'water']

"""## Binary Search"""

import copy
def queue_size_binary_search(min_allowed_max_waiting_time = 7, max_allowed_waiting_time = 15):
  base_mean_speed = 11933.0
  max_num_resources= 180
  obtained_bounds= []
  obtained_meal_capacities = []
  obtained_water_capacities = []
  obtained_toilet_capacities = []
  obtained_medical_capacities = []
  for i in range(0, num_stations):
    upper_bounds = {k: max_num_resources for k in resource_names}
    lower_bounds = {k: 1 for k in resource_names}

    has_incorrect_capacity = True
    next_tried_capacity = {}
    print("obtaining bounds for station {}". format(i))
    present_stations = []
    max_waiting_times = {}
    while has_incorrect_capacity:
      next_tried_capacity = {k: (upper_bounds[k] + lower_bounds[k]) // 2 for k in resource_names}
      meal_capacities = obtained_meal_capacities + [next_tried_capacity["meal"]] + [max_num_resources] * (num_stations - len(obtained_meal_capacities))
      water_capacities = obtained_water_capacities + [next_tried_capacity["water"]] + [max_num_resources] * (num_stations - len(obtained_water_capacities))
      toilet_capacities = obtained_toilet_capacities + [next_tried_capacity["toilet"]] + [max_num_resources] * (num_stations - len(obtained_toilet_capacities))
      medical_capacities = obtained_medical_capacities + [next_tried_capacity["medical"]] + [max_num_resources] * (num_stations - len(obtained_medical_capacities))
      ru, re = run_simulation_with_capacities(
          base_mean_speed,
          usage_times,
          need_functions,
          meal_capacities = meal_capacities,
          water_capacities = water_capacities,
          toilet_capacities = toilet_capacities,
          medical_capacities = medical_capacities,
          num_runners = 8303
      )
      max_waiting_times = {k: np.max(re[i][k].waiting_times + [0])  for k in re[i]}
      has_incorrect_capacity = False
      present_stations = list(max_waiting_times.keys())
      for resource_name in max_waiting_times:
        if upper_bounds[resource_name] - lower_bounds[resource_name] <= 0:
          next_tried_capacity[resource_name]= max(upper_bounds[resource_name], lower_bounds[resource_name])
          continue
        if max_waiting_times[resource_name] > max_allowed_waiting_time:
          lower_bounds[resource_name] = next_tried_capacity[resource_name] + 1
          has_incorrect_capacity = True
        elif max_waiting_times[resource_name] < min_allowed_max_waiting_time:
          upper_bounds[resource_name] = next_tried_capacity[resource_name]
          has_incorrect_capacity = True

    print("Max waiting times {}".format(max_waiting_times))
    obtained_bounds.append({'upper': {k: upper_bounds[k] for k in present_stations}, 'lower': {k: lower_bounds[k] for k in present_stations}})
    obtained_meal_capacities.append(next_tried_capacity['meal'] if 'meal' in present_stations else None)
    obtained_water_capacities.append(next_tried_capacity['water'] if 'water' in present_stations else None)
    obtained_toilet_capacities.append(next_tried_capacity['toilet'] if 'toilet' in present_stations else None)
    obtained_medical_capacities.append(next_tried_capacity['medical'] if 'medical' in present_stations else None)

  return {
    'obtained_bounds':obtained_bounds,
    'obtained_meal_capacities': obtained_meal_capacities,
    'obtained_water_capacities': obtained_water_capacities,
    'obtained_toilet_capacities': obtained_toilet_capacities,
    'obtained_medical_capacities': obtained_medical_capacities
  }
right_sized = queue_size_binary_search()
right_sized

right_sized

right_sized_cached = {'obtained_bounds': [{'upper': {'water': 105, 'toilet': 167, 'medical': 4},
   'lower': {'water': 103, 'toilet': 167, 'medical': 4}},
  {'upper': {'water': 53, 'toilet': 133, 'medical': 7},
   'lower': {'water': 53, 'toilet': 133, 'medical': 7}},
  {'upper': {'meal': 37, 'water': 36, 'toilet': 79, 'medical': 5},
   'lower': {'meal': 35, 'water': 35, 'toilet': 79, 'medical': 5}},
  {'upper': {'meal': 18, 'water': 27, 'toilet': 39, 'medical': 6},
   'lower': {'meal': 18, 'water': 27, 'toilet': 38, 'medical': 6}},
  {'upper': {'meal': 11, 'water': 22, 'toilet': 18, 'medical': 6},
   'lower': {'meal': 10, 'water': 22, 'toilet': 16, 'medical': 6}},
  {'upper': {'water': 21, 'toilet': 26}, 'lower': {'water': 19, 'toilet': 26}},
  {'upper': {'meal': 12, 'water': 18, 'toilet': 34, 'medical': 5},
   'lower': {'meal': 7, 'water': 18, 'toilet': 34, 'medical': 5}},
  {'upper': {'water': 17, 'toilet': 34}, 'lower': {'water': 16, 'toilet': 30}},
  {'upper': {'meal': 9, 'water': 13, 'toilet': 32, 'medical': 9},
   'lower': {'meal': 7, 'water': 13, 'toilet': 30, 'medical': 9}},
  {'upper': {'meal': 5, 'water': 11, 'toilet': 28},
   'lower': {'meal': 4, 'water': 10, 'toilet': 27}},
  {'upper': {'meal': 5, 'water': 11, 'toilet': 18, 'medical': 8},
   'lower': {'meal': 5, 'water': 11, 'toilet': 18, 'medical': 8}}],
 'obtained_meal_capacities': [None, None, 36, 18, 10, None, 9, None, 8, 4, 5],
 'obtained_water_capacities': [104, 53, 35, 27, 22, 20, 18, 16, 13, 10, 11],
 'obtained_toilet_capacities': [167, 133, 79, 38, 17, 26, 34, 32, 31, 27, 18],
 'obtained_medical_capacities': [4, 7, 5, 6, 6, None, 5, None, 9, None, 8]}

sum(right_sized_cached['obtained_toilet_capacities'])

"""## Latex"""
only_capacities = {k:v for k,v in right_sized_cached.items() if k != 'obtained_bounds'}
print(pd.DataFrame(only_capacities).to_latex())

"""## Repeated Runs"""
def obtain_repeated_results(num_runs = 3):
  results = []
  base_mean_speed = 11933.0

  for i in range(0, num_runs):
    print('.', end='')
    ru,re = run_simulation_with_capacities(
      base_mean_speed,
      usage_times,
      need_functions,
      meal_capacities=right_sized_cached['obtained_meal_capacities'],
      water_capacities=right_sized_cached['obtained_water_capacities'],
      toilet_capacities=right_sized_cached['obtained_toilet_capacities'],
      medical_capacities=right_sized_cached['obtained_medical_capacities']
    )
    results.append({'resources': re, 'runners': ru})
  return results
repeated_executions = obtain_repeated_results(50)

"""## Linear Model"""
linear_factors = {
      'humidity': {
          'medium': 0.08 * 60,
          'high': 0.56 * 60
      },
      'temperature': {
          'medium': 0.04 * 60,
          'high': 1.87 * 60
      },
      'wind': {
          'medium': 1.51 * 60,
          'high': 1.35 * 60
      }
  }

conditions_2017_marathon = {
    'humidity': 'medium',
    'temperature': 'medium',
    'wind': 'medium'
}

def run_linear_simulation(h, t, w):
  base_mean_speed = 11933.0 - sum([linear_factors[k][v] for k,v in conditions_2017_marathon.items()])
  mean_speed = base_mean_speed + linear_factors['humidity'][h]+ linear_factors['temperature'][t]+ linear_factors['wind'][w]

  meal_capacity = 40
  water_capacity = 80
  toilet_capacity = 45
  medical_capacity = 5


  ru, re = run_simulation_with_capacities(
      mean_speed,
      usage_times,
      need_functions,
      meal_capacities=right_sized_cached['obtained_meal_capacities'],
      water_capacities=right_sized_cached['obtained_water_capacities'],
      toilet_capacities=right_sized_cached['obtained_toilet_capacities'],
      medical_capacities=right_sized_cached['obtained_medical_capacities']
    )
  return{
      'humidity': h,
      'temperature': t,
      'wind': w,
      'runners': ru,
      'resources': re
  }

def obtain_linear_results():
  results = []

  for h in linear_factors['humidity']:
    for t in linear_factors['temperature']:
      for w in linear_factors['wind']:
        results.append(run_linear_simulation(h,t,w))

  return results

linear_results = obtain_linear_results()

"""# Statistical Helpers"""
import pandas as pd
import scipy

import types

no_waiting = types.SimpleNamespace()
no_waiting.waiting_times = [0]

def waiting_times(resources, station):
  return [ q.get(station, no_waiting).waiting_times for q in resources  ]

def runners_needs(runners, resource_name):
  return [
    [runner.current_needs[resource_name][i] for runner in runners ]
    for i in range(0, len(runners[0].current_needs[resource_name]))
  ]

def median_need_by_station(runners, resource_names):
  return {resource_name: [np.median(station_needs) for station_needs in runners_needs(runners, resource_name)] for resource_name in resource_names}

def runners_skipped_resource(runners, resource_name):
  skipped = [ [1 if need != None else 0 for need in runner.skipped_needs[resource_name] ] for runner in runners]
  return np.sum(skipped, axis=0)

def total_skipped_by_station(runners, resource_names):
  return {resource_name: runners_skipped_resource(runners, resource_name) for resource_name in resource_names}

def num_users(resources, resource_name):
  return [len(resource[resource_name].waiting_times) if resource_name in resource else 0 for resource in resources]

def compute_resource_statistics(resources, resource_name):
  w = waiting_times(resources, resource_name)
  non_empty_w = filter(lambda q: len(q) > 0,w)
  flattened_w = [t for q in w for t in q ]
  return {
    'resource': resource_name,
    'max queue': (np.max(flattened_w)),
    'max median queue': (np.max([np.median(q) for q in non_empty_w])),
    'average overall': (np.mean(flattened_w))
  }

"""# Analyze Repeated Executions"""
def obtain_repeated_data_frame(repeated_results, resource_name):
  stats = [compute_resource_statistics(r['resources'], resource_name) for r in repeated_results]
  return pd.DataFrame(stats)

repeated_data_frames = {k: obtain_repeated_data_frame(repeated_executions, k) for k in resource_names}

repeated_data_frames['water'].describe()

repeated_data_frames['meal'].describe()

repeated_data_frames['medical'].describe()

repeated_data_frames['toilet'].describe()

for resource_name in resource_names:
  shapiro_test_result = scipy.stats.shapiro(obtain_repeated_data_frame(repeated_executions, resource_name)['max queue'])
  print(resource_name)
  print(shapiro_test_result)

standard_deviations = {k:repeated_data_frames[k]['max queue'].std()  for k in repeated_data_frames}
standard_deviations

all_summaries = []
for k in repeated_data_frames:
  summary = repeated_data_frames[k].describe().reset_index()
  summary.insert(0,'resource', k)
  all_summaries.append(summary)
all_summaries = pd.concat(all_summaries)
all_summaries = all_summaries.rename(columns={'index':'statistic'})
all_summaries

"""## Latex

"""

def print_latex_statistic(stat_name):
  df = all_summaries[all_summaries['statistic'] == stat_name]
  print(df.loc[:, df.columns != 'statistic'].to_latex(index=False, float_format="%.2f", escape=True))

print_latex_statistic('50%')

print_latex_statistic('max')

print_latex_statistic('std')

print_latex_statistic('mean')

for k in repeated_data_frames:
  print("\\begin{table}")
  print(repeated_data_frames[k].describe().to_latex())
  print("\\caption{Statistical summary for %s resources in repeated runs}"%k)
  print("\\label{tab:statistics-%s}" % k)
  print("\\end{table}\n\n")

"""# Analyze Linear Model Results"""
linear_factors = ['humidity', 'wind', 'temperature']
def compute_linear_model_resource_statistics():
  columns = linear_factors + ['resource', 'max queue', 'max median queue', 'average overall']
  columns = {c: [] for c in columns }
  for r in linear_results:
    for resource_name in resource_names:
      for f in linear_factors:
        columns[f].append(r[f])
      stats = compute_resource_statistics(r['resources'], resource_name)
      for k in stats:
        columns[k].append(stats[k])
  return columns

def compute_linear_model_runner_statistics():
  columns = linear_factors + ['mean finish time', 'mean expected finish time']
  columns = {c: [] for c in columns }
  for r in linear_results:
    for f in linear_factors:
      columns[f].append(r[f])
    columns['mean finish time'].append(np.mean([runner.finishing_time for runner in r['runners']]))
    expected_finishing_times = [runner.running_time(marathon_distance) for runner in r['runners']]
    columns['mean expected finish time'].append(np.mean(expected_finishing_times))
  return columns
lin_resource_stats = pd.DataFrame(compute_linear_model_resource_statistics())
lin_runner_stats = pd.DataFrame(compute_linear_model_runner_statistics())

lin_runner_stats

print(lin_runner_stats.to_latex(index=False, float_format="%.0f"))

print(lin_resource_stats.sort_values(['resource', 'humidity', 'wind', 'temperature'], ascending = [True, False, False, False]).to_latex())

lin_resource_stats[lin_resource_stats['resource'] == 'water']

lin_resource_stats[lin_resource_stats['resource'] == 'meal']

lin_resource_stats[lin_resource_stats['resource'] == 'toilet']

lin_resource_stats[lin_resource_stats['resource'] == 'medical']

"""# Plot Data

## Obtain Example Run
"""

baseline_water = math.ceil(sum(right_sized_cached['obtained_water_capacities']) / 11)
baseline_toilet = math.ceil(sum(right_sized_cached['obtained_toilet_capacities']) / 11)
baseline_meal = math.ceil(sum([ i for i in right_sized_cached['obtained_meal_capacities'] if i != None]) / 7)
baseline_medical = math.ceil(sum([ i for i in right_sized_cached['obtained_medical_capacities'] if i != None]) / 8)

print("baseline params per station:")
print("water: {}".format(baseline_water))
print("toilet: {}".format(baseline_toilet))
print("meal: {}".format(baseline_meal))
print("medical: {}".format(baseline_medical))

runners, resources = run_simulation(
    11933.0,
    usage_times,
    need_functions,
    meal_capacity=baseline_meal,
    water_capacity=baseline_water,
    toilet_capacity=baseline_toilet,
    medical_capacity=baseline_medical,
)

"""## Compute Data"""

num_users_by_station = pd.DataFrame({k: num_users(resources, k) for k in resource_names})

sum(num_users(resources, 'medical'))

median_needs = pd.DataFrame(median_need_by_station(runners, resource_names))
total_skipped = pd.DataFrame(total_skipped_by_station(runners, resource_names))

finishing_times= [runner.finishing_time for runner in runners]
expected_finishing_times = [runner.running_time(marathon_distance) for runner in runners]

"""## Actual plots"""

import matplotlib.pyplot as plt

from google.colab import files
def download_plot(name):
  plt.savefig(name)
  files.download(name)

plt.figure()
num_users_by_station.plot()
download_plot('users-by-station.pdf')

plt.hist(finishing_times)

plt.hist(expected_finishing_times)

print("min actual: {}  min expected: {}".format( min(finishing_times), min(expected_finishing_times)))
print("max actual: {}  max expected: {}".format( max(finishing_times), max(expected_finishing_times)))

median_needs.plot()
download_plot("median-needs.pdf")

import matplotlib as mpl

mpl.pyplot.hist(resources[0]['water'].waiting_times)

import statistics

fig, ax = plt.subplots()
median_needs.plot(ax= ax)

list(map(len, waiting_times(resources, 'medical')))

stations = list(usage_times.keys())
median_wait = {
    'meal': [statistics.median(t) for t in waiting_times(resources, 'meal')],
    'toilet': [statistics.median(t) for t in waiting_times(resources, 'toilet')],
    'water': [statistics.median(t) for t in waiting_times(resources, 'water')],
    'medical': [statistics.median(t) if len(t) > 0 else 0 for t in waiting_times(resources, 'medical')],
}

x = np.arange(len(median_wait['water']))  # the label locations
width = 1.0/ (len(stations) + 1.0)  # the width of the bars
multiplier = len(median_wait.keys())

fig, ax = plt.subplots(layout='constrained')

for attribute, measurement in median_wait.items():
    offset = width * multiplier
    rects = ax.bar(x  + offset, measurement, width, label=attribute)
    ax.bar_label(rects, fmt="%i", padding=3)
    multiplier += 1

# Add some text for labels, title and custom x-axis tick labels, etc.
ax.set_ylabel('Median waiting time')
ax.set_title('Median waiting time per station')
ax.set_xticks(x + width, x)
ax.legend(loc='upper left', ncols=4)
ax.margins(0.05, 0.2)


download_plot("median-wait-per-station.pdf")

stations = list(usage_times.keys())
median_wait = {
    'meal': [np.max(t) for t in waiting_times(resources, 'meal')],
    'toilet': [np.max(t) for t in waiting_times(resources, 'toilet')],
    'water': [np.max(t) for t in waiting_times(resources, 'water')],
    'medical': [np.max(t) if len(t) > 0 else 0 for t in waiting_times(resources, 'medical')],
}

x = np.arange(len(median_wait['water']))  # the label locations
width = 1.0/ (len(stations) + 1.0)  # the width of the bars
multiplier = len(median_wait.keys())

fig, ax = plt.subplots(layout='constrained')

for attribute, measurement in median_wait.items():
    offset = width * multiplier
    rects = ax.bar(x  + offset, measurement, width, label=attribute)
    ax.bar_label(rects, fmt="%i", padding=3)
    multiplier += 1

# Add some text for labels, title and custom x-axis tick labels, etc.
ax.set_ylabel('Maximum waiting time')
ax.set_title('Maximum waiting time per station')
ax.set_xticks(x + width, x)
ax.legend(loc='upper left', ncols=4)
ax.margins(0.05, 0.2)

download_plot("maximum-waiting-times.pdf")

median_wait

plt.plot(runners[10].stop_times)

