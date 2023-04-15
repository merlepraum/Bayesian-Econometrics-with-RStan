
stan = "

//Stan code for assignment EBS2043
//Author: Merle Maria Praum
//Student number: i6249108


data {
 int < lower = 1 > N; // Sample size
 vector[N] year; // time
 vector[N] inv; //investments
 vector[N] gov; //governmental expenditure
 vector[N] log_gdp; // real gdp
}

parameters {
 real a; // Intercept
 real b; // trend coef
 real c; // investment coef
 real d; // government spending coef
 real < lower = 0 > sigma; // Error SD
}


model {
  a ~ normal(-0.019, 40);
  b ~ normal(0.108, 0.0001);
  c ~ normal(-0.022, 0.0004);
  d ~ normal(-0.075, 0.25);
 
  sigma ~ cauchy(0,3);
  for (i in 1:N)
   log_gdp[i] ~ normal(a + b*year[i] + c*inv[i] + d*gov[i],sigma);
}

/*
generated quantities {      // Not being used
}
*/
"
