//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at
functions{
  real ASN_log(vector x, real mu){
    vector[num_elements(x)] prob;
    real lprob;
    real smu; 
    real h;
    smu = (12*mu*sqrt(3*(4*mu^4 + 71*mu^2 + 8))+ 180*mu^2+8)^(1.0/3);
    h = (1.0/(3*mu))*(1+smu/2-2*(3*mu^2 - 1)/smu);
    for (i in 1:num_elements(x)){

      prob[i] = ((h^2 + h + x[i] + 1)*(h + 1)^(-1*x[i]))/((h + 1)^2*(h^2 + 1)*h^(-2));
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
