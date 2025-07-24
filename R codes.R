# install the required packages crom CRAN at first
# install.packages("tidyverse")
# install.packages("kableExtra")
# install.packages("latex2exp")

set.seed(123)
library(tidyverse)
library(kableExtra)
library(latex2exp)


# the function used to simulate

simulate <- function(x, n, distribution, ...){
  u <- if (distribution == "exponential") {
    rexp(n, ...)
  } else if (distribution == "weibull") {
    rweibull(n, ...)
  } else if (distribution == "normal") {
    rnorm(n, ...)
  } else {
    stop("Unsupported distribution")
  }
  x1 = runif(n, min = 45, max = 60)
  
  x2 = runif(n, min = 20, max = 30)
  
  
  y = 15+ 0.6 * x1 + 2 * x2 + u
  
  fit <- lm(y~x1+x2) |> summary()
  result <- data.frame(
    distribution = distribution,
    sample_size = n,
    beta1 = fit$coefficients[2],
    beta2 = fit$coefficients[3])
  return(result)
}



## simulating using pmap
## for each sample size there will be 100 iterations
## so for three distributions, there will be 900 iterations in total
## used separate parameters for each distribution to simulate u.
## I have used pmap_df() to do so.
set.seed(123)
sim_result <- pmap_df(.l = list(x = 1:900,
                                n = rep(c(rep(15,100),
                                          rep(100, 100), 
                                          rep(400, 100)),3),
                                distribution = c(rep("exponential", 300),
                                                 rep("weibull", 300),
                                                 rep("normal", 300)),
                                c(rate = rep(2, 300),
                                  shape = rep(2,300),
                                  mean = rep(0,300))),
                      
                      .f= simulate)
## see the result
sim_result %>% head()
## but pmap_df and all the other *_df are superseded. though they will work fine.
## If you want you can use purr::list_rbind() with just pmap().
set.seed(123)
sim_result2 <- pmap(.l = list(x = 1:900,
                              n = rep(c(rep(15,100),
                                        rep(100, 100), 
                                        rep(400, 100)),3),
                              distribution = c(rep("exponential", 300),
                                               rep("weibull", 300),
                                               rep("normal", 300)),
                              c(rate = rep(2, 300),
                                shape = rep(2,300),
                                mean = rep(0,300))),
                    
                    .f= simulate) %>%  list_rbind()

## see the result. they are the same
sim_result2 %>% head()

simulation <- sim_result %>% group_by(sample_size, distribution) %>% 
  summarise(across(beta1:beta2, 
                   list(mean = ~round(mean(.x),3),
                        sd = ~round(sd(.x), 3)), 
                   .names = "{.col}_{.fn}")) %>%
  mutate(sample_size = as.numeric(sample_size)) %>%
  arrange(sample_size) %>% 
  mutate(distribution = str_to_title(distribution)) %>% 
  rename_with(str_to_title)
simulation |> kable(align = "c")







ggplot(sim_result)+
  geom_histogram(aes(beta1),
                 bins = 8, 
                 fill = "steelblue", 
                 color = "black")+
  labs(title = expression(paste("Distribution of ",hat(beta)[1])))+ # used expression
  facet_wrap(~distribution+sample_size, 
             scales = "free",
             labeller = label_both)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 20, 
                                  color = "black"))






library(latex2exp)
ggplot(sim_result)+
  geom_histogram(aes(beta2),
                 bins = 8, fill = "steelblue",
                 color = "black")+
  facet_wrap(~distribution+sample_size, 
             scales = "free",
             labeller = label_both)+
  
  labs(title = TeX("Distribution of $\\hat{\\beta}_1$"))+ # used latex2exp
  
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 20, 
                                  color = "black"))