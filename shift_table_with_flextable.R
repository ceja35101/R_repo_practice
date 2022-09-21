library(flextable)
library(tidyverse)
library(safetyData)
use_df_printer()

set_flextable_defaults(
  theme_fun = theme_booktabs,
  big.mark = " ", 
  font.color = "#666666",
  border.color = "#666666",
  padding = 3,
)
################
adlb <- safetyData::sdtm_lb %>% as_tibble() %>% 
  filter(LBTEST %in% c("Albumin", "Alkaline Phosphatase"),
         str_detect(VISIT,"(WEEK|SCREENING)"))
adlb
###############

SHIFT_TABLE <- shift_table(
  x = adlb, cn_visit = "VISIT",
  cn_grade = "LBNRIND",
  cn_usubjid = "USUBJID",
  cn_lab_cat = "LBTEST",
  cn_is_baseline = "LBBLFL",
  baseline_identifier = "Y",
  grade_levels = c("LOW", "NORMAL", "HIGH"))
SHIFT_TABLE

##################
SHIFT_TABLE_VISIT <- attr(SHIFT_TABLE, "VISIT_N")

visit_as_factor <- attr(SHIFT_TABLE, "FUN_VISIT")
range_as_factor <- attr(SHIFT_TABLE, "FUN_GRADE")

# post treatments ----
SHIFT_TABLE <- SHIFT_TABLE %>% 
  mutate(VISIT    = visit_as_factor(VISIT),
         BASELINE = range_as_factor(BASELINE),
         LBNRIND  = range_as_factor(LBNRIND))

SHIFT_TABLE_VISIT <- SHIFT_TABLE_VISIT %>% 
  mutate(VISIT = visit_as_factor(VISIT))

SHIFT_TABLE
#################
my_format <- function(z) {
  formatC(z * 100, digits = 1, format = "f",
          flag = "0", width = 4)
}

tab <- tabulator(
  x = SHIFT_TABLE,
  hidden_data = SHIFT_TABLE_VISIT,
  row_compose = list(
    VISIT = as_paragraph(VISIT, "\n(N=", N_VISIT, ")")
  ),
  rows = c("LBTEST", "VISIT", "BASELINE"),
  columns = c("LBNRIND"),
  `n` = as_paragraph(N),
  `%` = as_paragraph(as_chunk(PCT, formatter = my_format))
)
tab

#################
ft <- as_flextable(
  x = tab, separate_with = "VISIT",
  label_rows = c(LBTEST = "Lab Test", VISIT = "Visit",
                 BASELINE = "Reference Range Indicator")) |>
  width(j = 3, width = 0.9)

ft
