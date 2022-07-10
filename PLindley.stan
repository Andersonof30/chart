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
  real ASN_log(vector x, real mu){
    vector[num_elements(x)] prob;
    real lprob;
    real t;
    
    t = (-1/(2*mu))*((mu-1)-sqrt(((mu-1)^2)+8*mu));
    
    for (i in 1:num_elements(x)){
      prob[i] = ((t + x[i] + 2)*(t + 1)^-(x[i]))/(
        (t + 1)^3*t^(-2));
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
  real <lower = 0> mu;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  //LIKELIHOOD
  Y ~ ASN(mu);
}
