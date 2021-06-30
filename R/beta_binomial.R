#Beta-binomial code from https://cran.r-project.org/web/packages/brms/vignettes/brms_customfamilies.html
beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int", vars = "vint1[n]"
)

stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"

stanvars_beta_binomial2 <- stanvar(scode = stan_funs, block = "functions")


#modified to not require rstan

beta_binomial_lpmf <- function(n, N, alpha, beta) {
  lchoose(N, n) + lbeta(n + alpha, N - n + beta) - lbeta(alpha, beta)
}

beta_binomial_rng <- function(N, alpha, beta) {
  max_length <- max(length(N), length(alpha), length(beta))
  allowed_lengths <- c(1, max_length)
  if(!(length(N) %in% allowed_lengths) || !(length(alpha) %in% allowed_lengths) ||
     !(length(beta) %in% allowed_lengths)) {
    stop("Bad lengths")
  }

  probs <- rbeta(max_length, alpha, beta)
  rbinom(max_length, N, probs)
}

log_lik_beta_binomial2 <- function(i, prep) {
  mu <- brms:::get_dpar(prep, "mu", i = i)
  phi <- brms:::get_dpar(prep, "phi", i = i)
  trials <- prep$data$vint1[i]
  y <- prep$data$Y[i]

  alpha <- mu * phi
  beta <- (1 - mu) * phi
  beta_binomial_lpmf(y, trials, alpha, beta)
}

posterior_predict_beta_binomial2 <- function(i, prep, ...) {
  mu <- brms:::get_dpar(prep, "mu", i = i)
  phi <- brms:::get_dpar(prep, "phi", i = i)
  trials <- prep$data$vint1[i]

  alpha <- mu * phi
  beta <- (1 - mu) * phi

  beta_binomial_rng(trials, alpha, beta)
}

posterior_epred_beta_binomial2 <- function(prep) {
  mu <- prep$dpars$mu
  trials <- prep$data$vint1
  trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  mu * trials
}
