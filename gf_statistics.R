#constants
e <- 2.71828182845904
pi <- 3.14159265358979

gf_count_NA <- function(x) {
  f <- function(y) {sum(is.na(y))}
  if (typeof(x) == "list") {
    value = data.frame(lapply(x,f))
  } else {
  value = f(x)
  }
}

iseven <- function(x) {
  if(x %% 2 == 0) {
    TRUE
  } else {
    FALSE
  }
}
  
gf_range <- function(x) {
  x <- na.omit(x)
  value = max(x) - min(x)
}

gf_mean <- function(x) {
  x <- na.omit(x)
  sum(x) / length(x)
}

gf_median <- function(x) {
  x <- na.omit(x)
  sorted_arr <- sort(x)
  length_X <- length(x)
  if (iseven(length_X)) {
    middle_lower_val <- sorted_arr[length_X / 2]
    middle_upper_val <- sorted_arr[(length_X / 2) + 1]
    gf_mean(c(middle_lower_val,middle_upper_val))
  } else {
    sorted_arr[(length_X+1)/2]
  }
}

gf_first_quartile <- function(x) {
  x <- na.omit(x)
  sorted_arr <- sort(x)
  length_X <- length(x)
  val <- (length_X+1)*1/4
  if (val%%1==0) {
    value = sorted_arr[val]
  } else if(val%%1==0.5) {
    value = gf_mean(c(sorted_arr[val%/%1],sorted_arr[(val%/%1)+1]))
  }  else if(val%%1<0.5) {
    value = sorted_arr[val%/%1]
  }  else if(val%%1>0.5) {
    value = sorted_arr[(val%/%1) + 1]
  }
}

gf_third_quartile <- function(x) {
  x <- na.omit(x)
  sorted_arr <- sort(x)
  length_X <- length(x)
  val <- (length_X+1)*3/4
  if (val%%1==0) {
    value = sorted_arr[val]
  } else if(val%%1==0.5) {
    value = gf_mean(c(sorted_arr[val%/%1],sorted_arr[(val%/%1)+1]))
  }  else if(val%%1<0.5) {
    value = sorted_arr[val%/%1]
  }  else if(val%%1>0.5) {
    value = sorted_arr[(val%/%1) + 1]
  }
}

gf_inter_quartile_range <- function(x) {
  value <-  gf_third_quartile(x) - gf_first_quartile(x) 
}

gf_mode_data_list <- function(x) {
  x <- na.omit(x)
  unique_values <- unique(x)
  count_values <- vector(mode = "integer",length = length(unique_values))
  for (i in x) {
    j <- match(i,unique_values)
    count_values[j] <- count_values[j] + 1 
  }
  df <- data.frame(val = unique_values, count = count_values)
  df <- df[order(df$count,decreasing = TRUE),]
  row.names(df) <- NULL
  value = df
}

gf_mode <- function(x){
  x <- na.omit(x)
  df <- gf_mode_data_list(x)
  for (i in 1:(nrow(df)-1)) {
    if (df$count[i] > df$count[i+1]) {
      break
    }
  }
  if (i == (nrow(df)-1) && df$count[i] == df$count[i+1]) {
    i<-i+1
  }
  value = df[1:i,1:2]
}

gf_variance <- function(x) {
  x <- na.omit(x)
  mean_vector <- rep(gf_mean(x), length(x))
  variance_vector = (x-mean_vector)^2
  value = sum(variance_vector) / (length(x) - 1)
}

gf_standard_deviation <- function(x) {
  x <- na.omit(x)
  value = gf_variance(x)^.5
}

gf_coefficient_of_variation <- function(x) {
  x <- na.omit(x)
  value = gf_standard_deviation(x) / gf_mean(x) * 100
}

gf_standard_error <- function(x) {
  x <- na.omit(x)
  value = gf_standard_deviation(x) / length(x)^.5
}

gf_z_scores <- function(x) {
  x <- na.omit(x)
  mean_vector <- rep(gf_mean(x), length(x))
  standard_deviation_vector <- rep(gf_standard_deviation(x), length(x))
  value = (x-mean_vector)/standard_deviation_vector
}

gf_sample_covariance <- function(x,y) {
  x1<-x[complete.cases(x,y)]
  y1<-y[complete.cases(x,y)]
  mean_vector_x <- rep(gf_mean(x1), length(x1))
  mean_vector_y <- rep(gf_mean(y1), length(y1))
  value = sum((x1 - mean_vector_x)*(y1 - mean_vector_y)) / (length(x1) - 1)
}

gf_correlation_coefficient <- function(x,y) {
  x1<-x[complete.cases(x,y)]
  y1<-y[complete.cases(x,y)]
  value = gf_sample_covariance(x1,y1) / (gf_standard_deviation(x1) * gf_standard_deviation(y1))
}

gf_frequency_distribution <- function(x) {
  dl <- gf_mode_data_list(x)
  dl <- dl[order(dl$val),]
}

gf_frequency_distribution_by_classify <- function(x) {
  x <- na.omit(x)
  mi <- round(floor(min(x)),-1)
  ma <- round(floor(max(x)+1),-1)
  value = data.frame(table(cut(x,seq(mi,ma,length.out=11))))
}

gf_plot_box <- function(x){
  x <- na.omit(x)
  boxplot(x,horizontal = TRUE,main = "Box Plot")
  q1 <- gf_first_quartile(x)
  q2 <- gf_median(x)
  q3 <- gf_third_quartile(x)
  iqr <- gf_inter_quartile_range(x)
  max_iqr <- q3 + iqr * 1.5
  min_iqr <- q1 - iqr * 1.5
  abline(v=c(q1,q3,q2,min(x),max(x)),col=c("red","red","blue","green","green"),lty=2, lwd=1)
}

gf_plot_normal_probability <- function(x){
  x <- na.omit(x)
  Rank <- seq(1, length(x), by=1)
  Z_Score <- sort(gf_z_scores(x))
  plot(Z_Score,Rank,main = "Normal Probabability Plot")
}

gf_plot_histogram <- function(x) {
  x <- na.omit(x)
  dl <- gf_mode_data_list(x)
  dl <- dl[order(dl$val),]
  barplot(height=dl$count,names.arg=dl$val,main = "Histogram Plot", xlab = "Value", ylab = "Count")
}

gf_plot_histogram_by_classify <- function(x) {
  x <- na.omit(x)
  dl <- gf_frequency_distribution_by_classify(x)
  barplot(height=dl$Freq,names.arg=dl$Var1,main = "Histogram Plot", ylab = "Count")
}

gf_plot_pareto <- function(x) {
  x <- na.omit(x)
  dl <- gf_mode_data_list(x)
  barplot(height=dl$count,names.arg=dl$val,main = "Pareto Plot")
  cumulative_probability <- cumsum(dl$count) / rep(sum(dl$count,length(dl)))
  lines(x=row.names(dl),y=cumulative_probability*max(dl$count),type = "b")
  axis(side = 4, at = c(0,cumulative_probability*max(dl$count)), labels = c(0,round(cumulative_probability,1)))
}

gf_plot_pie <- function(x) {
  x <- na.omit(x)
  dl <- gf_mode_data_list(x)
  dl <- dl[order(dl$val),]
  pie(dl$count,labels=dl$val,radius = 1,main = "Pie Plot")
}

gf_plot_poisson_distribution <- function(x) {
  x <- na.omit(x)
  k <- seq(0,max(x),1)
  lambda <- gf_mean(x)
  poisson_pdf <- function(x) {
    value = ((lambda^x)*e^(-lambda))/factorial(x)
  }
  poisson_probabilities <- lapply(k, poisson_pdf)  
  plot(x = k,y = poisson_probabilities,type = "b", main = "Poisson Distribution Plot")
}

gf_plot_normal_distribution <- function(x) {
  x <- na.omit(x)
  sigma_x <- gf_standard_deviation(x)
  mean_x <- gf_mean(x)
  normal_distribution_pdf <- function(x) {
    value =1/(sigma_x*(2*pi)^.5)*e^(-.5*((x-mean_x)/sigma_x)^2) 
  }
  sample_space <- seq(from = mean_x - 5 * sigma_x, to = mean_x + 5 * sigma_x,by = 0.01)
  normal_distribution <- lapply(sample_space, normal_distribution_pdf)
  plot(sample_space,normal_distribution,type = "l", main = "Normal Distribution")
}

gf_plot_log_normal_distribution <- function(x) {
  x <- na.omit(x)
  sigma_x <- gf_standard_deviation(x)
  mean_x <- gf_mean(x)
  log_normal_distribution_pdf <- function(x) {
    value =1/(x*sigma_x*(2*pi)^.5)*e^(-.5*((log(x,base = e)-mean_x)/sigma_x)^2) 
  } 
  sample_space <- seq(from = 0, to = mean_x + 10 * sigma_x,by = 0.01)
  log_normal_distribution <- lapply(sample_space, log_normal_distribution_pdf)
  plot(sample_space,log_normal_distribution,type = "l", main = "Log Normal Distribution")
}

gf_outliers_remove_iqr <- function(x) {
  x <- na.omit(x)
  iqr <- gf_inter_quartile_range(x)
  first_q <- gf_first_quartile(x)
  third_q <- gf_third_quartile(x)
  lower_boundary <- first_q - 1.5 * first_q
  upper_boundary <- third_q + 1.5 * third_q
  f <- function(v) {
    if (v < lower_boundary) {
      value = FALSE
    } else if (v > upper_boundary) {
      value = FALSE
    } else {
      value = TRUE
    }
  }
  value = x[sapply(x,f)]
}

gf_outliers_remove_std <- function(x) {
  x <- na.omit(x)
  std <- gf_standard_deviation(x)
  lower_boundary <- gf_mean(x) -  3 * std
  upper_boundary <- gf_mean(x) +  3 * std
  f <- function(v) {
    if (v < lower_boundary) {
      value = FALSE
    } else if (v > upper_boundary) {
      value = FALSE
    } else {
      value = TRUE
    }
  }
  value = x[sapply(x,f)]
}
