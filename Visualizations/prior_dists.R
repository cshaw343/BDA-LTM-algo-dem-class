#---- Package Loading ----
if (!require("pacman"))
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("cowplot")

#---- Cog Test Priors ----
cog_test_sensitivity <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
  stat_function(fun = dbeta, n = 1000,
                args = list(shape1 = 4.5, shape2 = 6.5),
                color = "#1f6997", size = 3) +
  ylab("Cog Test") + scale_y_continuous(breaks = NULL) +
  xlab("Sensitivity") +
  theme_minimal() +
  theme(text = element_text(size = 30))

cog_test_sensitivity

cog_test_specificity <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
  stat_function(fun = dbeta, n = 1000,
                args = list(shape1 = 1, shape2 = 7.8),
                color = "#1f6997", size = 3) +
  ylab("Cog Test") + scale_y_continuous(breaks = NULL) +
  xlab("1 - Specificity") +
  theme_minimal() +
  theme(text = element_text(size = 30))

cog_test_specificity

#---- IADL Priors ----
IADL_sensitivity <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
  stat_function(fun = dbeta, n = 1000,
                args = list(shape1 = 4.4, shape2 = 0.3),
                color = "#1f6997", size = 3) +
  ylab("IADL") + scale_y_continuous(breaks = NULL) +
  xlab("Sensitivity") +
  theme_minimal() +
  theme(text = element_text(size = 30))

IADL_sensitivity

IADL_specificity <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
  stat_function(fun = dbeta, n = 1000,
                args = list(shape1 = 2.7, shape2 = 6.6),
                color = "#1f6997", size = 3) +
  ylab("IADL") + scale_y_continuous(breaks = NULL) +
  xlab("1 - Specificity") +
  theme_minimal() +
  theme(text = element_text(size = 30))

IADL_specificity

