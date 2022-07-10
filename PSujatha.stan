functions{
  real PSD_log(vector x, real t){
    vector[num_elements(x)] prob;
    real lprob;
    for (i in 1:num_elements(x)){
    prob[i] = (t^3 * (x[i]^2 + x[i]*(t + 4) + (t^2 + 3*t + 4))
      )/(((t + 1)^(x[i]+3))*(t^2 + t + 2));
    }
    lprob = sum(log(prob));
    return lprob;
  }
}

// The input data is a vector 'Y' of length 'N'.
data {
  int <lower=0> N;
  vector[N] Y;
}

// The parameters accepted by the model.
parameters {
  real <lower = 0> t;
}

// The Poisson-Sujatha distribution (PSD) model to be estimated.
model {
  //LIKELIHOOD
  Y ~ PSD(t);
}
