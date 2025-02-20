# load packages
library(tidyverse)
library(here)
library(ggpubr)
devtools::load_all()

# set theme for plotting
theme_set(theme_pubr(base_family = "Helvetica"))

# load some data
qrf_data = read_rds(here("analysis/data/derived_data/qrf_data.rds"))
dens_offset = read_rds(here("analysis/data/derived_data/dens_offset.rds"))
sel_hab_mets = read_rds(here("analysis/data/derived_data/sel_hab_mets.rds"))
qrf_mod = read_rds(here("analysis/data/derived_data/qrf_mod.rds"))
hab_dict = read_rds(here("analysis/data/derived_data/hab_dict.rds"))

# change some of the names of metrics for plotting purposes
sel_hab_mets <-
  sel_hab_mets %>%
  left_join(hab_dict %>%
              select(Metric = ShortName,
                     Name) %>%
              distinct()) %>%
  mutate(Name = recode(Name,
                       'Wetted Width To Depth Ratio Avg' = 'Width:Depth Ratio',
                       'Wetted Depth SD' = 'Depth Complexity',
                       'Wetted Channel Braidedness' = 'Braidedness',
                       'Fish Cover: Total' = 'Fish Cover',
                       'Riparian Cover: Some Canopy' = 'Riparian Canopy',
                       'Wetted Width Integrated' = 'Wetted Width',
                       'Substrate < 6mm' = 'Fines',
                       # 'Substrate: D16',
                       'Channel Unit Frequency' = 'Channel Unit\nFrequency',
                       'Avg. August Temperature' = 'Avg. August Temp.',
                       'Large Wood Volume: Wetted Slow Water' = 'Large Wood\nFreq. in Pools'))


# which habitat metrics should be plotted?
plot_metrics <-
  c("avg_aug_temp",
    "FishCovTotal",
    "CU_Freq",
    "RipCovCanSome")

# plot partial dependence plots
plot_partial_dependence(rf_mod = qrf_mod,
                        data = qrf_data,
                        plot_covars = plot_metrics,
                        data_dict = hab_dict %>%
                          select(-Name) %>%
                          inner_join(sel_hab_mets %>%
                                       select(ShortName = Metric,
                                              Name)) |>
                          filter(ShortName %in% plot_metrics),
                        type = 'quantile',
                        pred_quantile = 0.9,
                        log_transform = T,
                        log_offset = dens_offset,
                        scales = 'free') +
  labs(y = 'Capacity Prediction (per m)') +
  scale_color_grey(start = 0.5, end = 0.5) +
  theme(legend.position = 'none',
        strip.text = element_text(size = 9),
        axis.text = element_text(size = 8))

ggsave(filename = "O:/Documents/ResearchPapers/2015_HabitatCapacityReview/Update2024/qrf_pdp.png",
       width = 7,
       height = 7)
