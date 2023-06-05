##### Figures and tables -----

# Generate simulated dataset if it does not exist in the environment -----
if(!"simulation" %in% ls()){
  source("code/sim_aggregation.R")
}


# Updating dataset -----
## Rename certain methods and probabilities -----
simulation[, treat_control_param := gsub("monotone", "uniform", treat_control_param)]
simulation[, treat_control_param := gsub("linear increase", "linear", treat_control_param)]
simulation[, treat_control_param := gsub("increase of intermediate category", "intermediate", treat_control_param)]

simulation[, method := gsub("Chi.*_corrected", "Chi Squared (with correction)", method)]
simulation[, method := gsub("Chi.*2", "Chi Squared", method)]
simulation[, method := gsub("Independence_Cochran_Mantel_Haenszel", "Cochran Armitage (coin package)", method)]
simulation[, method := gsub("Independence_Mann_Whitney_exact", "Mann Whitney (coin package)", method)]
simulation[, method := gsub("Dich_Logistic", "Logistic Regression", method)]
simulation[, method := gsub("Cochran_Armitage", "Cochran Armitage", method)]
simulation[, method := gsub("Mann_Whitney", "Mann Whitney", method)]
simulation[, method := gsub("Prop_Odds", "Proportional Odds", method)]
simulation[, method := gsub("Stuarts_C", "Stuarts tau c", method)]


## Subsetting datasets according to type-I error, power and probabilities from previous studies -----
same <-
  levels(as.factor(simulation$treat_control_param))[sapply(strsplit(levels(
    as.factor(simulation$treat_control_param)
  ), " - "),
  function(i)
    i[[1]] == i[[2]])]

simulation_type_I <- simulation[!treat_control_param %like% "stroke" &
                                  treat_control_param %in% same]

simulation_power <- simulation[!treat_control_param %like% "stroke" &
                                 !treat_control_param %in% same]


## Make a colour palette -----
mk.palette <- data.table()
mk.palette$methods <-
  c(
    "Chi Squared (with correction)",
    "Mann Whitney (coin package)",
    "Cochran Armitage (coin package)",
    "Chi Squared",
    "Mann Whitney",
    "Fisher",
    "Stuarts tau c",
    "Cochran Armitage",
    "Logistic Regression",
    "Proportional Odds"
  )
mk.palette$colours <-
  c(
    "#88BDE6",
    "#FBB258",
    "#BFA554",
    "#265DAB",
    "#DF5C24",
    "#059748",
    "#E5126F",
    "#9D722A",
    "#7B3A96",
    "#C7B42D"
  )


# Figure 2 -----
plot_rejection_rate(unique(simulation[!method %in% c("Chi Squared (with correction)",
                                                     "Cochran Armitage (coin package)",
                                                     "Mann Whitney (coin package)") & 
                                        treat_control_param %in% same & 
                                        !treat_control_param %like% "stroke"],
                           by = c("method", "K", "treat_control_param", "n_obs")),
                    yintercept = 0.05)

# Figure 3 -----
plot_rejection_rate(unique(simulation_type_I[method %in% c("Fisher", "Chi Squared")],
                           by = c("method", "K", "treat_control_param", "n_obs")),
                    smooth = FALSE,
                    yintercept = 0.05)

# Figure 4 -----
plot_rejection_rate(unique(
  simulation_power[!method %in% c("Chi Squared (with correction)",
                                  "Cochran Armitage (coin package)",
                                  "Mann Whitney (coin package)") &
                    treat_control_param %in% c("geometric - intermediate",
                                               "geometric - uniform",
                                               "linear - intermediate")],
  by = c("method", "K", "treat_control_param", "n_obs")),
yintercept = 0.8)

# Figure 5 -----
plot_rejection_rate(unique(
  simulation_power[!method %in% c("Chi Squared (with correction)",
                                  "Cochran Armitage (coin package)",
                                  "Mann Whitney (coin package)") &
                    treat_control_param %in% c("geometric - linear",
                                               "intermediate - uniform",
                                               "linear - uniform")],
  by = c("method", "K", "treat_control_param", "n_obs")),
yintercept = 0.8) 

# Figure 6 -----
plot_rejection_rate(unique(
  simulation_power[!method %in% c("Chi Squared (with correction)",
                                  "Cochran Armitage (coin package)",
                                  "Mann Whitney (coin package)",
                                  "Logistic Regression",
                                  "Fisher",
                                  "Chi Squared") &
                    treat_control_param %in% c("intermediate - uniform",
                                               "uniform - intermediate")],
  by = c("method", "K", "treat_control_param", "n_obs")),
smooth = FALSE)

# Figure 7 -----
ggplot(unique(
  simulation[treat_control_param %like% "stroke - stroke" &
               !method %in% c("Chi Squared (with correction)",
                              "Mann Whitney (coin package)",
                              "Cochran Armitage (coin package)")],
  by = c("method", "K", "treat_control_param", "n_obs")
),
aes(x = n_obs,
    y = rejection_rate)) +
  geom_line(aes(colour = method), alpha = 0.3) +
  xlab("Sample Size") +
  ylab("Rejection Rate") +
  facet_wrap( ~ K, scales = "free") +
  geom_smooth(linewidth = 0.6) +
  scale_color_manual(breaks = mk.palette$methods,
                     values = mk.palette$colours,
                     name = "Methods") +
  theme_tufte() +
  theme(axis.line = element_line(colour = "grey70"))

# Figure 8 -----
ggplot(unique(
  simulation[treat_control_param %like% "stroke" &
               K %in% 7 &
               !method %in% c("Chi Squared (with correction)",
                              "Mann Whitney (coin package)",
                              "Cochran Armitage (coin package)")],
  by = c("method", "K", "treat_control_param", "n_obs")
),
aes(x = n_obs,
    y = rejection_rate)) +
  geom_line(aes(colour = method), alpha = 0.3) +
  xlab("Sample Size") +
  ylab("Rejection Rate") +
  facet_wrap(~ treat_control_param) +
  geom_smooth(size = 0.6) +
  scale_color_manual(breaks = mk.palette$methods,
                     values = mk.palette$colours,
                     name = "Methods") +
  theme_tufte() +
  theme(axis.line = element_line(colour = "grey70"))

# Table 2 -----
difference_chi <- foreach(
  k = as.list(levels(as.factor(simulation$K))),
  .combine = rbind
) %:%
  foreach(
    n = as.list(levels(as.factor(simulation$n_obs))),
    .combine = rbind
  ) %:%
  foreach(param = as.list(levels(as.factor(
    simulation$treat_control_param))),
    .combine = rbind) %dopar% {
      data.table::data.table(K = k,
                             n_obs = n, 
                             treat_control_param = param, 
                             diff = 
                               abs(diff(unique(
                                 simulation[method %in% c("Chi Squared",
                                                          "Chi Squared (with correction)")],
                                 by = c("method", "K", "treat_control_param", "n_obs")
                               )[K %in% k &
                                   treat_control_param %in% param &
                                   n_obs %in% n,
                                 rejection_rate])))
    }

difference_chi[!treat_control_param %like% "stroke" & diff != 0]


# Appendix -----
# A Study Design and Methods -----
mRS_3 <- list(treatment = c(0.76, 0.14, 0.1),
              control = c(0.75, 0.15, 0.1))

mRS_5 <- list(treatment = c(1447, 32, 15, 33, 13)/1540,
              control = c(1425, 48, 18, 32, 7)/1530)

mRS_7 <- list(treatment = c(0.15, 0.21, 0.18, 0.16, 0.13, 0.07, 0.1),
              control = c(0.07, 0.1, 0.12, 0.15, 0.24, 0.12, 0.19))

mRS_9 <- list(treatment = c(1447, 32, 5, 10, 12, 10, 6, 5, 13)/1540, 
              control = c(1425, 48, 4, 14, 11, 12, 8, 1, 7)/1530)


# B Results -----
## Figure 9 -----
plot_rejection_rate(unique(simulation_type_I[method %in% c("Proportional Odds", "Logistic Regression")],
                           by = c("method", "K", "treat_control_param", "n_obs")),
                    smooth = FALSE,
                    yintercept = 0.05)

## Figure 10 -----
plot_rejection_rate(unique(simulation_type_I[method %in% c("Proportional Odds", "Mann Whitney")],
                           by = c("method", "K", "treat_control_param", "n_obs")),
                    smooth = FALSE,
                    yintercept = 0.05)

## Figure 11 -----
plot_rejection_rate(unique(
  simulation_power[!method %in% c("Chi Squared (with correction)",
                                  "Cochran Armitage (coin package)",
                                  "Mann Whitney (coin package)") &
                    !treat_control_param %in% c("geometric - intermediate",
                                                "geometric - uniform",
                                                "linear - intermediate",
                                                "geometric - linear",
                                                "intermediate - uniform",
                                                "linear - uniform")],
  by = c("method", "K", "treat_control_param", "n_obs")
))

## Figure 12 -----
plot_rejection_rate(unique(simulation_type_I[method %in% c("Mann Whitney",
                                                           "Mann Whitney (coin package)")], 
                           by = c("method", "K", "treat_control_param", "n_obs")),
                    smooth = FALSE,
                    yintercept = 0.05)

## Figure 13 -----
plot_rejection_rate(unique(simulation_power[method %in% c("Mann Whitney",
                                                          "Mann Whitney (coin package)")], 
                           by = c("method", "K", "treat_control_param", "n_obs")), 
                    smooth = FALSE)

## Figure 14 -----
plot_rejection_rate(unique(simulation[treat_control_param %like% "stroke" &
                                        method %in% c("Mann Whitney",
                                                      "Mann Whitney (coin package)")], 
                           by = c("method", "K", "treat_control_param", "n_obs")), 
                    smooth = FALSE)

## Figure 15 -----
plot_rejection_rate(unique(simulation_type_I[method %in% c("Cochran Armitage",
                                                           "Cochran Armitage (coin package)")], 
                           by = c("method", "K", "treat_control_param", "n_obs")), 
                    smooth = FALSE, 
                    yintercept = 0.05)

## Figure 16 -----
plot_rejection_rate(unique(simulation_power[method %in% c("Cochran Armitage",
                                                          "Cochran Armitage (coin package)")], 
                           by = c("method", "K", "treat_control_param", "n_obs")), 
                    smooth = FALSE)

## Figure 17 -----
plot_rejection_rate(unique(simulation[treat_control_param %like% "stroke" &
                                        method %in% c("Cochran Armitage",
                                                      "Cochran Armitage (coin package)")], 
                           by = c("method", "K", "treat_control_param", "n_obs")), 
                    smooth = FALSE)

## Figure 18 -----
plot_rejection_rate(unique(
  simulation[treat_control_param %like% "stroke" &
               !treat_control_param %in% "stroke - stroke" &
               !K %in% 7 &
               !method %in% c("Chi Squared (with correction)",
                              "Mann Whitney (coin package)",
                              "Cochran Armitage (coin package)")],
  by = c("method", "K", "treat_control_param", "n_obs")
))



## Differences between inbuilt and coin functions
### Mean differences between the inbuilt and coin function of the Mann-Whitney U test -----
difference_mann_whitney <- foreach(
  k = as.list(levels(as.factor(simulation$K))),
  .combine = rbind
) %:%
  foreach(
    n = as.list(levels(as.factor(simulation$n_obs))),
    .combine = rbind
  ) %:%
  foreach(param = as.list(levels(as.factor(
    simulation$treat_control_param))),
    .combine = rbind) %dopar% {
      data.table::data.table(K = k,
                             n_obs = n, 
                             treat_control_param = param, 
                             diff = 
                               abs(diff(unique(
                                 simulation[method %in% c("Mann Whitney", "Mann Whitney (coin package)")],
                                 by = c("method", "K", "treat_control_param", "n_obs")
                               )[K %in% k &
                                   treat_control_param %in% param &
                                   n_obs %in% n,
                                 rejection_rate])))
    }

mean(difference_mann_whitney$diff)
# 0.001483932
mean(difference_mann_whitney[treat_control_param %in% same & 
                               !treat_control_param %like% "stroke", diff])
# 0.001435019
mean(difference_mann_whitney[!treat_control_param %in% same & 
                               !treat_control_param %like% "stroke", diff])
# 0.001631944
mean(difference_mann_whitney[treat_control_param %like% "stroke", diff])
# 0.001308321



### Mean differences between the inbuilt and coin function of the Cochran-Armitage test -----
difference_cochran <- foreach(
  k = as.list(levels(as.factor(simulation$K))),
  .combine = rbind
) %:%
  foreach(
    n = as.list(levels(as.factor(simulation$n_obs))),
    .combine = rbind
  ) %:%
  foreach(param = as.list(levels(as.factor(
    simulation$treat_control_param))),
    .combine = rbind) %dopar% {
      data.table::data.table(K = k,
                             n_obs = n, 
                             treat_control_param = param, 
                             diff = 
                               abs(diff(unique(
                                 simulation[method %in% c("Cochran Armitage", 
                                                          "Cochran Armitage (coin package)")],
                                 by = c("method", "K", "treat_control_param", "n_obs")
                               )[K %in% k &
                                   treat_control_param %in% param &
                                   n_obs %in% n,
                                 rejection_rate])))
    }


mean(difference_cochran$diff)
# 0.002549786
mean(difference_cochran[treat_control_param %in% same & 
                          !treat_control_param %like% "stroke", diff])
# 0.001527335
mean(difference_cochran[!treat_control_param %in% same & 
                          !treat_control_param %like% "stroke", diff])
# 0.003594444
mean(difference_cochran[treat_control_param %like% "stroke", diff])
# 0.00161133





