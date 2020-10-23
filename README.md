# TfWM Travel Times Archiver

The `TFWM Travel Times Archiver` is a long running process that will continously
poll and detect changes returned by the `services_timetables.json` TransportAPI
endpoint.

The endpoint includes a current `progress_between_stops` field, from which the
actual travel time between each pair of stops along a bus journey can be
inferred. The travel times are then stored in a database, tagged with additional
metadata allowing for short-term and long-term querying of statistical
aggregates of historical travel times between stops.

## Installation

Get [`stack`](https://docs.haskellstack.org/en/stable/README/), clone this
repository, and run:

```
cd tfwm-travel-times-archiver
stack setup
stack build
```

To install globally:

```
stack install
```

## Running

If installed locally:

```
stack exec tfwm-travel-times-archiver -- --help
```

If installed globally:

```
tfwm-travel-times-archiver --help
```

### Command line options

```
TfWM Travel Times Archiver

Usage: tfwm-travel-times-archiver --config STRING [--database STRING] [--port INT]

Available options:
  -h,--help                Show this help text
  --config STRING          Configuration file path
  --database STRING        LevelDB path, default: ./travel_times
  --port INT               Port, default: 80
```

## Configuration file format

```
{
  "url":     <url>,     // optional, TransportAPI query URL, defaults to "http://nxwm.transportapi.com"
  "source":  <source>,  // optional, SIRI VM data source, defaults to "nxwm_siri_vm"
  "app_id":  <app_id>,  // TransportAPI app id
  "app_key": <app_key>, // TransportAPI app key
  "services": [         // List of operators and services to query
    [ <operator1>, [ <service1>, <service2> ] ],
    [ <operator2>, [ <service1>, <service2> ] ],
    ...
  ]
}
```

The provided `config.json` file can be used as a starting point.

## API

The TfWM Travel Times Archiver will start up a server which accepts requests of
the following form:

```
http://.../travel-times/<time from>/<time to>/<time of day from>/<time of day to>/<weekdays>
```

* `time from`, `time to` - Overall time interval in the ISO 8601 format
* `time of day from`, `time of day to` - Time of day subintervals in the `HH:MM` format
* `weekdays` - Days of week in the `DDDDDDD` format, where `D` may be either `t` (query) or `f` (ignore)

For example, to query all travel times between `14:00` and `19:00` o'clock each Thursday and Friday between the dates `2019-12-03T08:00:00+00:00` and `2019-12-10T22:00:00+00:00`, issue the following request:

```
http://.../travel-times/2019-12-03T08:00:00+00:00/2019-12-10T22:00:00+00:00/14:00/19:00/fffttff
```

The response will then be in the following format:

```
{
  "segments": [
    {
      "from": "639000611",         // Atcocode
      "to": "639000621",           // Atcocode
      "median_travel_times": [
        {
          "observed_travel_time_secs": 65, // In seconds
          "time_range": [                  // Time interval
            "09:30:00",
            "09:45:00"
          ],
          "scheduled_travel_time_secs": 60 // In seconds
        },
        ...
      ]
    },
    ...
  ]
}
```

`observed_travel_time_secs` and `scheduled_travel_time_secs` are the *median*
observed (actual) and scheduled travel times across all recorded journeys for
the segment `from` - `to` during the `time_range` interval.
