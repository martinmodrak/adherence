data {
  int<lower=0> N;
  array[N] int<lower=0,upper=1> adherent;
  int<lower=2> N_measurements;
  array[N] int<lower=1, upper=N_measurements> measurement;
  int<lower=2> N_classes;
  array[N] int<lower=1, upper=N_classes> classes;

  int<lower=2> N_NYHA;
  array[N_measurements] int<lower=0, upper=N_NYHA> NYHA;
  array[N_measurements] int<lower=0,upper=1> VO2_max_missing;
  vector<lower=0>[N_measurements] VO2_max;
  array[N_measurements]  int<lower=0,upper=1> NT_proBNP_missing;
  vector<lower=0>[N_measurements] NT_proBNP;
  array[N_measurements] int<lower=0, upper=1> cohort;

  int<lower=2> N_EF;
  array[N_measurements] int<lower=0,upper=1> EF_missing;
  array[N_measurements] int<lower=0, upper=N_EF> EF;

}

parameters {
  real adherent_intercept;
  vector[N_classes] class_intercept_raw;
  real<lower=0> class_intercept_sd;

  real badhealth_slope;

  ordered[N_NYHA - 1] nyha_thres;

  real VO2_max_intercept;
  real VO2_max_slope;
  real<lower=0> VO2_max_sd;

  real NT_proBNP_intercept;
  real NT_proBNP_slope;
  real<lower=0> NT_proBNP_sd;

  real cohort_diff;

  ordered[N_EF - 1] EF_thres;

  vector[N_measurements] badhealth;

}


transformed parameters {
  vector[N_classes] class_intercept = class_intercept_raw * class_intercept_sd;
}

model {
  adherent_intercept ~ normal(0, 3);
  class_intercept_raw ~ std_normal();
  class_intercept_sd ~ normal(0, 2);
  badhealth ~ normal(0, 1);
  nyha_thres ~ normal(0, 2);
  VO2_max_intercept ~ normal(log(10), 1);
  VO2_max_slope ~ normal(0, 1);
  VO2_max_sd ~ normal(0, 1);
  NT_proBNP_intercept ~ normal(6, 1);
  NT_proBNP_slope ~ normal(0, 2);
  NT_proBNP_sd ~ normal(0, 1);
  EF_thres ~ normal(0, 2);

  adherent ~ bernoulli_logit(adherent_intercept + class_intercept[classes] + badhealth_slope * badhealth[measurement] + cohort_diff * to_vector(cohort[measurement]));
  NYHA ~ ordered_logistic(badhealth, nyha_thres);
  for(s in 1:N_measurements) {
    if(!VO2_max_missing[s]) {
      VO2_max[s] ~ lognormal(VO2_max_intercept + VO2_max_slope * badhealth[s], VO2_max_sd);
    }
    if(!EF_missing[s]) {
      EF[s] ~ ordered_logistic(badhealth[s], EF_thres);
    }
    if(!NT_proBNP_missing[s]) {
      NT_proBNP[s] ~ lognormal(NT_proBNP_intercept + NT_proBNP_slope * badhealth[s], NT_proBNP_sd);
    }
  }
}

