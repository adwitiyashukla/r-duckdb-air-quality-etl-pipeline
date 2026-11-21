# r-duckdb-air-quality-etl-pipeline

[![ci](https://github.com/adwitiyashukla/r-duckdb-air-quality-etl-pipeline/actions/workflows/ci.yml/badge.svg)](https://github.com/adwitiyashukla/r-duckdb-air-quality-etl-pipeline/actions/workflows/ci.yml)

A data engineering project in R. It takes the raw hourly readings from every EPA air monitor in the US for 2016 to 2025, rebuilds the daily Air Quality Index (AQI) for each monitor, county and metro area, and then checks every row against the AQI files EPA publishes.

Live report: https://huggingface.co/spaces/adwitiyashukla/r-duckdb-air-quality-etl-pipeline

I built it to see if I could get to EPA's exact numbers starting only from the raw data. EPA documents the AQI formula, but many small rules only show up when your numbers do not match theirs. The pipeline keeps a reason for every row that still differs.

## Results

Full run on my laptop: HP OmniBook 5, Intel Core 5 120U, 8 GB RAM, Windows 11, R 4.6.2.

| | |
|---|---|
| EPA files | 172 zip files, about 3 GB |
| Hourly readings | 312,537,675 |
| Monitors | 7,577 at 2,714 sites |
| First run, zips already on disk | 64 minutes |
| Second run, nothing changed | under 20 seconds, 509 of 510 targets skipped |
| Parquet layers and warehouse on disk | 1.6 GB |

Most of the first run is reading the zips into Parquet (43 minutes). The rebuild itself takes about 4 minutes and the data contracts 12.

How close the rebuild gets to EPA's own files:

| Level | EPA rows | Same as EPA | Match |
|---|---|---|---|
| Ozone 8 hour averages | 65,862,136 | 65,862,136 | 100.000% |
| Daily summary per monitor | 19,544,921 | 19,514,102 | 99.842% |
| County daily AQI | 3,256,837 | 3,256,295 | 99.983% |
| Metro area (CBSA) daily AQI | 1,707,458 | 1,707,029 | 99.975% |

For counties and metro areas, "same" means the same AQI value. 3,256,180 county-days also have the same defining pollutant, defining site and number of sites.

All 30,819 daily rows that differ are PM2.5 or ozone. 17,558 of them are PM2.5 rows where EPA changed the hourly data after it built its daily file, so the hourly row has a newer last change date. Another 13,142 are Teledyne T640 monitors that EPA moved to the corrected method codes 736 and 738, where EPA's daily file does not agree with its own hourly file. 91 of the rest come from one Alaska ozone monitor where EPA's daily file counts every 8 hour window twice.

## Rules I had to work out

- Ozone 8 hour averages start at each hour, need at least 6 of the 8 hours, and are truncated to 3 decimals instead of rounded.
- The daily ozone value uses all 24 windows when at least 18 are valid. Otherwise it uses the 17 windows that start between 7 am and 11 pm, and it needs 13 of those.
- CO 8 hour averages end at each hour and are rounded half away from zero, so -0.05 ppm becomes -0.1 and not 0.0. I found this by testing a few rounding rules against EPA's 2024 file. It moved 2024 CO days from 99.35% to 99.98% matching.
- A PM2.5 day with fewer than 18 hours still counts if its average is above 35 ug/m3. PM10 does the same above 150, but it divides the sum by 24 instead of by the hours it has.
- NO2 only counts for a county on days with 18 hours or a max above 100 ppb. SO2 never sets the county AQI.
- When monitors tie for the county max, EPA picks the defining pollutant and the defining site separately. The pollutant is the first one alphabetically and the site is the highest site ID, so the two can come from different monitors.
- 8 hour windows stay inside the calendar year. January 1 does not use the last hours of December 31.
- AirData applies the 2024 PM2.5 breakpoints to every past year, so 2016 is graded with the new scale too.

## What the data shows

- The worst day in the ten years was June 29, 2023. Smoke from the Canadian wildfires put 141 counties above AQI 150.
- The 2024 PM2.5 breakpoints put between 8.3% and 9.9% of county-days each year into a worse category than the 2012 breakpoints would.
- The highest county AQI is 8,368, in Mono County, California on June 12, 2022, from PM10 dust. The AQI formula keeps going past 500, and EPA's file has the same number.

## How it works

```
EPA AirData zips
  -> landing    zips on disk, downloaded again only when size or Last-Modified changes
  -> bronze     one typed Parquet file per zip, bad rows split out, lineage in the Parquet metadata
  -> silver     8 hour averages, daily summaries and daily AQI per monitor
  -> gold       county and metro area AQI, plus the four level comparison with EPA
  -> warehouse  one DuckDB file for the API, the dashboard and the report
```

The pipeline is written with [targets](https://docs.ropensci.org/targets/) and does its heavy work in DuckDB. Each EPA file is its own target, so when EPA republishes one file only that file and the steps that use it run again. Each zip is streamed straight into a gzip file that DuckDB reads, so the plain CSV, about 2 GB for one hourly file, never lands on disk.

Data contracts written with pointblank check nulls, duplicates, value ranges, units and GMT offsets in bronze, the county and metro tables in gold, and the match rate against EPA for each year. A failed check stops the run before the warehouse gets built. In the full run 670 checks ran and none failed. 23 of them warned about hourly values outside the expected range, mostly PM10 readings below -20 or above 10,000 ug/m3. EPA keeps those readings in its own numbers, so the pipeline keeps them too and only stops when more than 0.1% of a file is out of range. The other 29 warnings are years where a level matched less than 100% but stayed above its gate.

All settings live in `config.yml` and are checked when the pipeline starts.

## Running it

You need R 4.6. From the repo folder:

```
Rscript -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
Rscript -e "renv::restore()"
Rscript -e "targets::tar_make()"
```

The first run downloads about 3 GB from EPA. For a quick try, set the environment variable `R_CONFIG_ACTIVE=live` to run only 2024, or change `years` in `config.yml`.

After the run:

```
Rscript -e "targets::tar_source(); run_dashboard()"
Rscript -e "targets::tar_source(); run_api()"
```

The dashboard runs on port 8080. The API runs on port 8000 and has Swagger docs at `/__docs__/`.

| Endpoint | Returns |
|---|---|
| `GET /health` | build info and county match counts |
| `GET /counties?state=06` | counties in a state |
| `GET /aqi/county?state=06&county=037&from=2024-01-01&to=2024-12-31` | daily AQI for one county next to EPA's |
| `GET /aqi/cbsa?code=31080&from=2024-01-01&to=2024-12-31` | the same for a metro area |
| `GET /aqi/worst?date=2023-06-29&limit=10` | worst counties on one day |
| `GET /reconciliation?level=county` | match counts by year |
| `GET /mismatches?level=county&year=2024` | rows that differ and the reason |

## Tests

```
Rscript -e "testthat::test_dir('tests/testthat')"
```

The tests write small fake EPA zip files with hand picked days, like a county where two sites tie, a PM2.5 day with only 17 hours and a CO window that averages to exactly -0.05. They run the real code on them and compare the DuckDB results with a plain R version of the same rules. They also run the whole pipeline twice to check that the second run skips everything, republish one file to check that only its branch runs again, and break a contract to check that no warehouse gets built. lintr runs as one of the tests.

CI runs the tests on Ubuntu and Windows, then runs the pipeline on the real 2024 EPA files.

## Layout

```
R/            fetch, bronze, rebuild, area, reconcile, contracts, warehouse, api, dashboard, report
tests/        testthat tests and the fake EPA files they build
ref/          AQI breakpoint tables
_targets.R    pipeline entry point
config.yml    settings
renv.lock     package versions
```
