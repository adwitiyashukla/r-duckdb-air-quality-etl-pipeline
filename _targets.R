targets::tar_source("R")

settings <- read_config()
options(aq.runtime = runtime_settings(settings))

years <- settings$years
paths <- settings$paths
bounds <- settings$bounds
quality <- settings$quality

targets::tar_option_set(memory = "transient", garbage_collection = TRUE, workspace_on_error = TRUE)

pipeline_targets(settings)
