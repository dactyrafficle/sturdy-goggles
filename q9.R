

rm(list = ls())   # clears all variables from environment
dev.off()         # remove graphics during interactive session
# CTRL + L clears the console

# obviously this codes junk. its the worst of both world. neither performant, nor elegant. but it works, which is a quality of its own

get_a_set_of_observations_a2_q9 = function(n_obs_, rho_, sigma_) {
  
  beta_0_ = 0
  beta_1_ = 1
  beta_2_ = 1
  
  # rho_ matters when we generate x1 x2
  x = generate_x1_x2_correlated(n_obs_, rho_)
  x1_ = x[,c(1)]
  x2_ = x[,c(2)]
  
  # sigma_ comes into play
  u_ = rnorm(n=n_obs_, mean=0, sd=sigma_)
  y_ = beta_0_ + beta_1_ * x1_ + beta_2_ * x2_ + u_
  
  return(data.frame(
    x1 = c(x1_),
    x2 = c(x2_),
    y = c(y_)
   )
  )
  
  
};


# hist(x=df_$x1,density=15,axes=FALSE,col="skyblue2")
#  curve(.1*dnorm(x,-.6,.15))
# model_ = lm(df_$y ~ df_$x1)
# summary_ = summary(model_)
# summary_


run_p1 = function(df_) {
  
  # no intercept model
  model_ = lm(df_$y ~ df_$x1 + df_$x2 - 1)
  summary_ = summary(model_)
  
  return(data.frame(
    b1_hat = summary_$coefficients[1],
    b2_hat = summary_$coefficients[2],
    t1 = summary_$coefficients[5],
    t2 = summary_$coefficients[6],
    r_sq = summary_$r.squared
  ))
  
}

run_p2 = function(df_) {
  
  # misspec but with an intercept
  model_ = lm(df_$y ~ df_$x1)
  summary_ = summary(model_)
  
  return(data.frame(
    b1_hat = summary_$coefficients[2],
    b2_hat = 0,
    t1 = summary_$coefficients[6],
    t2 = 0,
    r_sq = summary_$r.squared
  ))
  
}


set.seed(12)
rho_arr = c(0.2, 0.5, 0.8)  # rho goes from 0.2 to 0.5 to 0.8
sigma_arr = c(1, sqrt(5), sqrt(10)) # sigma goes from 1 to sqrt(5) to sqrt(10)



# i need to create a list
# there are 27 models total 3x3x3 rho-sigma-procedure
# each gets 1000 sets
# so there are 27000 rows
# each row needs 8 columns : rho-sigma-procedure + [b1, b2, t1, t2, r_sq]
# overall, its a matrix with 27000x8

n_obs = 50
n_sets = 1000
count = 0
n_rows = 27 * n_sets
results = matrix(nrow = n_rows, ncol = 8)


for (rho in rho_arr) {
  
  for (sigma in sigma_arr) {
    
    # generate a bunch of sample using rho and sigma
    
    for (i in 1:n_sets) {
      
      df_ = get_a_set_of_observations_a2_q9(n_obs_ = n_obs, rho_ = rho, sigma_ = sigma);
      
      
      # procedure_id = 1 : rho, sigma, procedure
      count = count + 1
      results[count, 1] = rho
      results[count, 2] = sigma
      results[count, 3] = 1
      
      model_1 = run_p1(df_)
      results[count, 4] = model_1$b1_hat
      results[count, 5] = model_1$b2_hat
      results[count, 6] = model_1$t1
      results[count, 7] = model_1$t2
      results[count, 8] = model_1$r_sq


      
      # procedure_id = 2 : rho, sigma, procedure
      count = count + 1
      results[count, 1] = rho
      results[count, 2] = sigma
      results[count, 3] = 2
      
      model_2 = run_p2(df_)
      results[count, 4] = model_2$b1_hat
      results[count, 5] = model_2$b2_hat
      results[count, 6] = model_2$t1
      results[count, 7] = model_2$t2
      results[count, 8] = model_2$r_sq
      
      # procedure_id = 3 : rho, sigma, procedure
      count = count + 1
      results[count, 1] = rho
      results[count, 2] = sigma
      results[count, 3] = 3
      
      if (abs(model_1$t2) > 2) {
        results[count, 4] = model_1$b1_hat
        results[count, 5] = model_1$b2_hat
        results[count, 6] = model_1$t1
        results[count, 7] = model_1$t2
        results[count, 8] = model_1$r_sq
      }
      
      # dont like else statements
      if (abs(model_1$t2) <= 2) {
        results[count, 4] = model_2$b1_hat
        results[count, 5] = model_2$b2_hat
        results[count, 6] = model_2$t1
        results[count, 7] = model_2$t2
        results[count, 8] = model_2$r_sq
      }
      
    }
  
    
  }
  
}

# results # contains all the results, which is 1000 sets from 27 models
colnames(results) = c("rho", "sigma", "procedure_id", "b1_hat", "b2_hat", "t1", "t2", "r_sq")


draw_histogram = function(rho_, sigma_, procedure_id_, variable_name_, bins_){
  
  # using 2 global variables : results, and n_obs
  
  # filter for the variable we want
  a = results[results[, "rho"] == rho_,]
  a = a[a[, "sigma"] == sigma_,]
  a = a[a[, "procedure_id"] == procedure_id_,]
  x_ = a[,variable_name_]
  
  color_str = "seagreen"
  if (variable_name_ == "b1_hat") {
    color_str = "skyblue2"
  }
  if (variable_name_ == "b2_hat") {
    color_str = "royalblue"
  }
  if (variable_name_ == "t1") {
    color_str = "lightcoral"
  }
  if (variable_name_ == "t2") {
      color_str = "indianred3"
  }
  if (variable_name_ == "r_sq") {
    color_str = "royalblue4"
  }
  

  var_ = sigma_^2
  file_name = paste("econ_852_a2_q9_hist_","rho=", rho_, "_var=", var_,"_pid=",procedure_id_,"_variable=",variable_name_, sep="")
  
  # print(file_name)
  png(filename = paste("C:\\Xampp\\htdocs\\_queens\\F22_econ_852\\a2\\",file_name,".png",sep=""))
  hist(x=x_,
       col=color_str,
       border="gray87",
       breaks=c(bins_),
       freq=FALSE, # this means we get densities
       main=paste(variable_name_, "given pid = ", procedure_id_,"; rho =", rho_,", sigma =", floor(sigma_*100)/100 ,"&",n_obs,"obs", sep=" "),
       xlab=variable_name_)
  dev.off()
  
}


# print all the histograms

print_all_the_histrograms = function(variable_name_) {
  
  # uses one global. results
  
  x_ = results[,variable_name_]
  
  bin_width = 1;  
  if (variable_name_ == "b1_hat" || variable_name_ == "b2_hat") {
    bin_width = 0.125  
  }
  if (variable_name_ == "t1" || variable_name_ == "t2") {
    bin_width = 0.25  
  }
  lower_bound = floor(min(x_)*(1/bin_width))*bin_width
  upper_bound = ceiling(max(x_)*(1/bin_width))*bin_width
  
  
  if (variable_name_ == "r_sq") {
    bin_width = 0.05  
    lower_bound = 0
    upper_bound = 1
  }
  

  bins = c(seq(from = lower_bound, to = upper_bound, by = bin_width))
  
  hist_id = 0
  for (i in 1:3){
    for (j in 1:3){
      for (k in 1:3){
        
        hist_id = hist_id + 1
        print(hist_id)
        
        draw_histogram(
          rho_ = rho_arr[i],
          sigma_ = sigma_arr[j],
          procedure_id_ = k,
          variable_name_ = variable_name_,
          bins_ = bins
        )
        
      }
    }
  }
  
}



print_all_the_histrograms("b1_hat")
print_all_the_histrograms("b2_hat")
print_all_the_histrograms("t1")
print_all_the_histrograms("t2")
print_all_the_histrograms("r_sq")




?write.table # docs
write.table(results,
            file="filename.csv",
            sep=",",
            row.names=FALSE
)
