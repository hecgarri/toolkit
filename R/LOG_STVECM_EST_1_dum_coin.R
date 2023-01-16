
LSTVECM <- function(y_dat, k_y_, vnames_y_, ys_addtr_, vnames_ad_, vname_th_, p_max_, c_rank_, coin_vec_, trend_, restrc_, restrt_, l_diff_, dmax__,delay_){
    k_y <- k_y_
    ys_addtr <- ys_addtr_
    trend <- trend_
    p_max <- p_max_
    dmax_ <- dmax__
    l_diff <- l_diff_
    delay <- delay_
    c_rank <- c_rank_
    coin_vec <- coin_vec_
    restrc <- restrc_
    restrt <- restrt_
    vnames_y <- vnames_y_
    vnames_ad <- vnames_ad_
    vname_th <- vname_th_

    if (ys_addtr <- 0){
            k_addtr <-0
    } else{
            k_addtr <-ncol(ys_addtr)
    }
    nobs <- nrow(y_dat)

## k_y is number of variables in system
## k_addtr is number of exogenous candidate transition variables 

k_tr <- k_y

#  k_y is number of variables in system
#  k_addtr is number of exogenous candidate transition variables 

#  T_est is number of observations in the estimation sample.

diff_ <- 1
p_dmax <- max(c(p_max,dmax_))
max_diff <- maxc(c(diff_,l_diff))
T_est <- nobs-p_dmax-max_diff

# Construct data-matrix #

y <- matrix()
y <- y_dat

ys <- y[1:nrow(y_dat), 1:k_y]
deter <- cbind(rep(1, T_est), seq(1, T_est))

# Crear matriz de datos
ysd <- ys[2:(T_est+p_dmax+1), 1:k_y] - ys[1:(T_est+p_dmax), 1:k_y]
dat <- ysd[(p_dmax+1):(T_est+p_dmax), ]
beta <- t(coin_vec)

# Crear elemento determinista
if (restrc && !restrt) {
  det <- deter[, 1]
} else if (restrt && restrc) {
  det <- deter[, 1:2]
} else if (restrt && !restrc) {
  det <- deter[, 2]
} else if (!restrt && !restrc) {
  det <- data.frame()
}

}