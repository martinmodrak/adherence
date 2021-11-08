data {
  int<lower=0> N;
  int<lower=0,upper=1> adherent[N];
  int<lower=2> N_subjects;
  int<lower=1, upper=N_subjects> subject[N];

  int<lower=2> N_NYHA;
  int<lower=0, upper=N_NYHA> NYHA[N_subjects];
  int<lower=0,upper=1> VO2_max_missing[N_subjects];
  vector<lower=0>[N_subjects] VO2_max;
  vector<lower=0>[N_subjects] NT_proBNP;

  int<lower=2> N_EF;
  int<lower=0,upper=1> EF_missing[N_subjects];
  int<lower=0, upper=N_EF> EF[N_subjects];

}

transformed data {
  int N_NYHA_thres_low = (N_NYHA - 2) %/% 2;
  int N_NYHA_thres_high = N_NYHA - 2 - N_NYHA_thres_low;
}

parameters {
  real adherent_intercept;
  real badhealth_slope;
  vector[N_subjects] badhealth;

  //Separate into low and high and force 0 as one of the thresholds
  positive_ordered[N_NYHA_thres_low] nyha_thres_low;
  positive_ordered[N_NYHA_thres_high] nyha_thres_high;

  real VO2_max_intercept;
  real VO2_max_slope;
  real<lower=0> VO2_max_sd;

  real NT_proBNP_intercept;
  real NT_proBNP_slope;
  real<lower=0> NT_proBNP_sd;

  ordered[N_EF - 1] EF_thres;
}

transformed parameters {
  ordered[N_NYHA - 1] nyha_thres =
    append_row(
      append_row(- reverse(nyha_thres_low), to_vector([0])),
    nyha_thres_high);
}

model {
  adherent_intercept ~ normal(0, 3);
  badhealth ~ normal(0, 1);
  nyha_thres_low ~ normal(0, 2);
  nyha_thres_high ~ normal(0, 2);
  VO2_max_intercept ~ normal(log(10), 1);
  VO2_max_slope ~ normal(0, 1);
  VO2_max_sd ~ normal(0, 1);
  NT_proBNP_intercept ~ normal(6, 1);
  NT_proBNP_slope ~ normal(0, 2);
  NT_proBNP_sd ~ normal(0, 1);
  EF_thres ~ normal(0, 2);

  adherent ~ bernoulli_logit(adherent_intercept + badhealth_slope * badhealth[subject]);
  NYHA ~ ordered_logistic(badhealth, nyha_thres);
  NT_proBNP ~ lognormal(NT_proBNP_intercept + NT_proBNP_slope * badhealth, NT_proBNP_sd);
  for(s in 1:N_subjects) {
    if(!VO2_max_missing[s]) {
      VO2_max[s] ~ lognormal(VO2_max_intercept + VO2_max_slope * badhealth[s], VO2_max_sd);
    }
    if(!EF_missing[s]) {
      EF[s] ~ ordered_logistic(badhealth[s], EF_thres);
    }
  }
}

