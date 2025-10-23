library(tidyverse)
library(gtsummary)
trial %>% 
  tbl_summary() %>% 
  gtsummary::as_gt() %>% 
  gt::gtsave('table.docx')
