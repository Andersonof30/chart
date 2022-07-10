//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

functions{
  real ASN_log(vector x, real alp, real bet){
    vector[num_elements(x)] prob;
    real lprob;
    for (i in 1:num_elements(x)){
      prob[i] = alp*bet*(x[i]^(alp - 1)) *((1 - x[i]^alp)^(bet - 1));
    }
    lprob = sum(log(prob));
    return lprob;
  }
}

// The input data is a vector 'y' of length 'N'.
data {
  int <lower=0> N;
  vector[N] Y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real <lower= 0 > alp;
  real <lower= 0 > bet;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  alp ~ normal(0.5,2.5);
  bet ~ normal(0.5,2.5);
  Y ~ ASN(alp, bet);
}