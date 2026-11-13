# pre-processing pilot data ring test

library(tidyverse)
library(readxl)

EUSoilLabs_LRT_data_anonymous_20261104 <- read_excel(
  "data-raw/EUSoilLabs_LRT_data_anonymous_20261104.xlsx"
)

# 17 records with missing values in `data.status`
# these records also have missing values in the measurements => remove
EUSoilLabs_LRT_data_anonymous_20261104 |> 
  filter(is.na(data.status)) |> 
  View()


data_ringtest <- EUSoilLabs_LRT_data_anonymous_20261104 |>
  # remove records without relevant ring-test data
  filter(!is.na(data.status)) |> 
  # tidy variable names
  rename(
    lab_code = lab.code,
    meas_unit = meas.unit,
    meas_1 = meas.1,
    meas_2 = meas.2,
    meas_3 = meas.3,
    meas_4 = meas.4,
    meas_5 = meas.5,
    meas_mean = meas.mean,
    meas_uncertain = meas.uncertain,
    data_status = data.status,
    standard_total_value = standard_total.value,
    standard_total_uncertain = standard_total.uncertain,
    standard_extract_value = standard_extract.value,
    standard_extract_uncertain = standard_extract.uncertain,
    limit_determ = limit.determ
  ) |> 
  # factor variables
  mutate(
    part = factor(part, c("a", "b"), c("a - anorganic", "b - organic")),
    data_status = factor(
      data_status, 1:4, c("at least 3 measured values submitted",
                          "less than 3 measured values submitted",
                          "mean +/- SD submitted",
                          "value and unknown uncertainty submitted"),
      ordered = TRUE
    )
  ) |> 
  # number of reporting labs per subst
  mutate(.by = subst, nr_labs = n_distinct(lab_code)) |> 
  # arrange variables logically
  select(
    part, subst, nr_labs, lab_code, data_status, 
    standard_total_value, standard_total_uncertain, 
    standard_extract_value, standard_extract_uncertain, limit_determ,
    meas_unit, contains("meas_")
  ) |> 
  # nest data per soil descriptor 
  nest(.by = c(part, subst, nr_labs)) |> 
  arrange(part, desc(nr_labs))



# save pre-processed dataset
save(data_ringtest, file = here::here("data/ringtest-pilot.rda"))




