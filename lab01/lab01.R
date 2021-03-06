n = 20
s = 5
f = n-s
a = 2
b = 2
p = s/n
q = f/n
bernoulli_sample = rbinom(20,1,p)
actual_sample_mean = p
actual_sample_sd = sqrt(p*q)


posterior_function = function(N){
  set.seed(12345)
      result = rbeta(N,a+s,b + f)
    mean_sd = list("mean" = mean(result), "sd" = sqrt(var(result)))
return(mean_sd)
}

plot_mean_sd = function(){
  posterior_call = seq(from = 1000, to = 10000, by = 1000)
  posterior_result = matrix(0,nrow = 10, ncol = 2)
  for (i in 1:length(posterior_call)){
    result = posterior_function(posterior_call[i])
    posterior_result[i,1] = result$mean
    posterior_result[i,2] = result$sd
  }
  cat(posterior_result)
  
  plot(1:10, posterior_result[,1], type = 'l', 
       main = 'mean and standard deviation')
  cat('Actual mean is: ',actual_sample_mean, ' Atcual SD is: ', actual_sample_sd)
  plot(1:10, posterior_result[,2], col = 'red', type = 'l')
}
plot_mean_sd()


  #Second question:
  Data<-c(44,25,45,52,30,63,19,50,34,67)	
  mu = 3.7
  n = 10000
  t2<-mean((log(Data)-mu)^2)
  X = rchisq(n,n)
  sigma_square = n*t2/X
  hist(sigma_square)
  #calculating 10000 from the posterior distribution of sigma squared
  library(LaplacesDemon)
  Post_draws<-rinvchisq(10000,10000)
  hist(Post_draws)

  #They both follow the same distributi on
  #2.b
  phi = pnorm(sqrt(sigma_square)/sqrt(2), mean = 0, sd = 1)
  gini = 2*phi-1
  hist(gini)
#From the histogram we can conclude that the posterior distribution for the gini coefficients is normal

  #2.3
  #Calculating 90% equal tail credible interval
  p.interval(gini,prob = 0.9, plot = TRUE, HPD = FALSE)
  
  #Calculating HPD interval
  p.interval(gini,prob = 0.9, plot = TRUE, HPD = TRUE)
  #The HPD interval takes the interval considering the highest density, wheras the equal tail inverval considers tails equally (0.05 left and right)
  
  
#Question 3
  library(LaplacesDemon)
  options(digits = 21)
  radians_data = c(-2.44,2.14,2.54,1.83,2.02,2.33,-2.79,2.23,2.07,2.02)
  angles = c(40,303,326,285,296,314,20,308,299,296)
  k_exp = exp(-radians_data)
  k_grid = expand.grid(x = angles, y = k_exp)
  mu = 2.39
  x = 3
  lambda = 1
  k_prior = exp(-lambda*x) # prior
  grid_k_values = k_grid[,2]
  calculate_posterior = function(){
    posterior_values = c()
    for(i in 1:length(k_grid)){
      bessel_values = besselI(grid_k_values[i],0)
      prob = exp(grid_k_values[i]*cos(radians_data-mu))/(2*pi*bessel_values)
      likehood = prod(prob)
      posterior_values = c(posterior_values,k_prior*likehood)
    }
    return(posterior_values)
  }
  grid_angels = k_grid[,1]
  #hist(posterior_values)
  posterior = calculate_posterior()
  plot(density(posterior, bw = 2), main = "Posterior Distribution of k for the Wind Direction")
  
 #3.2
  calculate_mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  calculate_mode(posterior)
  max(posterior)
  