# load libraries
library(dplyr)
library(progress)
library(ggplot2)
library(gridExtra)
library(furrr)

# ---------------------
# Functions
# ---------------------
rmse <- function(x) {sqrt(mean(x^2))}

plugin <- function(x,y) {
  rmse(rowMeans(x) - rowMeans(y))
}

permute <- function(z,nx,ny) {
  ind <- sample(1:(nx+ny),nx)
  xtilde <- z[,ind]
  ytilde <- z[,-ind]
  plugin(xtilde,ytilde)
}

permutation <- function(x,y) {
  z <- cbind(x,y)
  plugin(x,y) - mean(replicate(100,permute(z,ncol(x),ncol(y))))
}

jackknife <- function(x,y) {
  cov <- c(rep(1,ncol(x)),rep(0, ncol(y)))
  z <- t(cbind(x,y))
  xbar <- rowMeans(x)
  ybar <- rowMeans(y)
  jhat <- mean(sapply(1:nrow(z),jk1, z=z,cov=cov,xbar=xbar,ybar=ybar,nx=ncol(x),ny=ncol(y)))
  thetahat <- plugin(x,y)
  return(nrow(z)*thetahat - (nrow(z)-1)*jhat)
}

jk1 <- function(z,cov,i,xbar,ybar,nx,ny) {
  if(cov[i]==0) {
    #y case
    ytilde <- (ybar*ny - z[i,])/(ny-1)
    return(rmse(xbar - ytilde))
  } else {
    #x case
    xtilde <- (xbar*nx - z[i,])/(nx-1)
    return(rmse(xtilde - ybar))
  }
  #rmse(colMeans(zni[covni==1,]) - colMeans(zni[covni==0,]))
}

boot <- function(x,y,strat=TRUE) {
  if(strat) {
    indx <- sample(1:ncol(x), ncol(x), replace=TRUE)
    indy <- sample(1:ncol(y), ncol(y), replace=TRUE)
    return(rmse(rowMeans(x[,indx]) - rowMeans(y[,indy])))
  } else {
    cov <- c(rep(1,ncol(x)),rep(0, ncol(y)))
    z <- cbind(x,y)
    covb <- c(1,1)
    while(length(unique(covb))==1) {
      ind <- sample(1:ncol(z),ncol(z),replace=TRUE)
      covb <- cov[ind]
    }
    zb <- z[,ind]
    return(rmse(rowMeans(zb[,covb==1, drop=FALSE]) - rowMeans(zb[,covb==0,drop=FALSE])))
  }
}

bootstrap <- function(x,y,strat=TRUE) {
  2*plugin(x,y) - mean(replicate(500, boot(x,y,strat=strat)))
}

# sampling function
samp_function <- function(mux, muy, samp_size, class_ratio,D) {

  # set sample size
  nx <- ceiling(class_ratio*samp_size/(1+class_ratio))
  ny <- samp_size-nx

  # sample
  x <- replicate(nx,rnorm(D,mean=mux))
  y <- replicate(ny,rnorm(D,mean=muy))
  out <- data.frame(
    samp_size = samp_size,
    class_ratio = class_ratio,
    #method = c("none", "Jackknife", "permutation", "bootstrap", "bootstrap (stratified)"),
    method = c("none", "Jackknife", "permutation", "bootstrap"),
    value = c(
      plugin(x,y),
      jackknife(x,y),
      permutation(x,y),
      #bootstrap(x,y,strat=FALSE),
      bootstrap(x,y)
    )
  )
  return(out)
}

# ---------------------
# Simulations
# ---------------------

sim_function <- function(theta, D, samp_size, class_ratio) {

  mux <- rep(0,D)
  muy <- rep(theta,D)

  # set sample size
  nx <- ceiling(class_ratio*samp_size/(1+class_ratio))
  ny <- samp_size-nx

  # sample
  x <- replicate(nx,rnorm(D,mean=mux))
  y <- replicate(ny,rnorm(D,mean=muy))
  out <- data.frame(
    D = D,
    theta = theta,
    samp_size = samp_size,
    class_ratio = class_ratio,
    method = c("none", "Jackknife", "permutation", "bootstrap"),
    value = c(
      plugin(x,y),
      jackknife(x,y),
      permutation(x,y),
      bootstrap(x,y)
    )
  )
  return(out)
}

params <- expand.grid(
  samp_size=c(150, 250, 500, 1000, 2500),
  class_ratio=c(0.01, 0.1, 0.5, 1),
  theta=c(0, 1),
  D = c(10,100)
)

plan(multisession, workers = 12)
sim_out_df <- 1:100 %>%
  future_map_dfr(
    ~params %>%
      pmap(
        sim_function
      ),
    .progress=TRUE,
    .options =furrr_options(seed = 2023L)
  )

# ---------------------
# Plot
# ---------------------
plot_df <- sim_out_df %>%
  mutate(method = factor(method, levels = c("none", "bootstrap", "permutation", "Jackknife"))) %>%
  group_by(D, theta, samp_size, class_ratio, method) %>%
  summarize(avg = mean(value), p025 = quantile(value, 0.025), p975 = quantile(value, 0.975), .groups = "drop_last")

# new facet label names
ratio.labs <- c("class ratio\n1:100", "class ratio\n1:10", "class ratio\n1:2", "class ratio\n1:1")
names(ratio.labs) <- c("0.01", "0.1", "0.5", "1")

# plot w. theta = 0
plot1 <- ggplot(subset(plot_df, theta == 0 & D == 100), aes(x = factor(samp_size), y = avg, group =1)) +
  geom_point() +
  geom_errorbar(aes(ymin = p025 , ymax = p975), width = 0.2) +
  geom_hline(aes(yintercept = 0), color = 'red', linetype = 'dashed') +
  facet_grid(method~class_ratio, labeller = labeller(class_ratio = ratio.labs)) +
  labs(title = paste0("\u03b8"," = 0\n"), y = expression(hat(theta)), x = "Sample Size", caption = paste0("Note: red dashed line =  ", "\u03b8.")) +
  theme(
    strip.text.x = element_text(size=16, face = 'bold'),
    strip.text.y = element_text(size=16, face = 'bold'),
    axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size=16, margin = margin(t = 0, r = 15, b = 0, l = 15), face = 'bold'),
    axis.title.x = element_text(size=16, margin = margin(t = 15, r = 0, b = 15, l = 0), face = 'bold'),
    plot.caption = element_text(hjust = 0, size = 16),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18))

# plot w. theta = 1
plot2 <- ggplot(subset(plot_df, theta == 1 & D == 100), aes(x = factor(samp_size), y = avg, group =1)) +
  geom_point() +
  geom_errorbar(aes(ymin = p025 , ymax = p975), width = 0.2) +
  geom_hline(aes(yintercept = 1), color = 'red', linetype = 'dashed') +
  facet_grid(method~class_ratio, labeller = labeller(class_ratio = ratio.labs)) +
  labs(title = paste0("\u03b8"," = 1"), y = expression(hat(theta)), x = "Sample Size") +
  theme(
    strip.text.x = element_text(size=16, face = 'bold'),
    strip.text.y = element_text(size=16, face = 'bold'),
    axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size=16, margin = margin(t = 0, r = 15, b = 0, l = 15), face = 'bold'),
    axis.title.x = element_text(size=16, margin = margin(t = 15, r = 0, b = 15, l = 0), face = 'bold'),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18))

grid.arrange(plot1, plot2, ncol=2)
