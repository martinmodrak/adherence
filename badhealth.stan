data {
  int<lower=0> N;
  int<lower=0,upper=1> adherent[N];
  int<lower=2> N_subjects;
  int<lower=1, upper=N_subjects> subject[N];
  int<lower=2> N_classes;
  int<lower=1, upper=N_classes> classes[N];

  int<lower=2> N_NYHA;
  int<lower=0, upper=N_NYHA> NYHA[N_subjects];
  int<lower=0,upper=1> VO2_max_missing[N_subjects];
  vector<lower=0>[N_subjects] VO2_max;
  vector<lower=0>[N_subjects] NT_proBNP;

  int<lower=2> N_EF;
  int<lower=0,upper=1> EF_missing[N_subjects];
  int<lower=0, upper=N_EF> EF[N_subjects];

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

  ordered[N_EF - 1] EF_thres;

  vector[N_subjects] badhealth;

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

  adherent ~ bernoulli_logit(adherent_intercept + class_intercept[classes] + badhealth_slope * badhealth[subject]);
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

