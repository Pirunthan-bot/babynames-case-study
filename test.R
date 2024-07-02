library(tidyverse)
library(ggplot2)

midwest_selected <- midwest %>%
    select(county,state,poptotal) %>%
        group_by(state,county) %>%
            slice_max(poptotal, n=1)

