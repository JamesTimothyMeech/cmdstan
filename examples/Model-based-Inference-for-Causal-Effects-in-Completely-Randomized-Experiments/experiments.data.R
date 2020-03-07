
N <- 500       # number of observations
alpha <- 1.0   # intercept in the Y model
tau <- 0.25    # treatment effect

# The assignment mechanism
N_t <- 200                                  # number of treated units
W <- sample(rep(c(0, 1), c(N - N_t, N_t)))  # binary treatment variable
ii_t <- which(W == 1); ii_c <- which(W == 0) # index arrays for treatment variable

# The science
mu_c <- alpha + 0*tau; sd_c <- 1   # mean and SD for the control
mu_t <- alpha + 1*tau; sd_t <- 1   # mean and SD for the treated

rho <- 0.0                                   # correlation between the errors
cov_mat <- rbind(c(sd_c^2, rho*sd_c*sd_t),
                 c(rho*sd_c*sd_t, sd_t^2))   # variance-covariance matrix

science <- mvrnorm(n = N, mu = c(mu_c, mu_t), Sigma = cov_mat, empirical = TRUE)
Y0 <- science[, 1]        # potential outcome if W = 1
Y1 <- science[, 2]        # potential outcome if W = 0
tau_unit <- Y1 - Y0       # unit-level treatment effect

# The realization of potential outcomes by the assignment mechanism
Y_obs <- Y0 * (1 - W) + Y1 * W
Y_mis <- Y0 * W + Y1 * (1 - W)

# Collect data into a list format suitable for Stan
stan_data <- list(N = N, y = Y_obs, w = W, rho = 0.0)

# Compile and run the stan model
fit_simdat <- stan(file = "experiemnts.stan",
                   data = stan_data,
                   iter = 1000, chains = 4)
