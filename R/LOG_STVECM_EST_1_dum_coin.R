
LSTVECM <- function(y_dat, k_y_, vnames_y_, ys_addtr_, vnames_ad_, vname_th_, p_max_, c_rank_, coin_vec_, trend_, restrc_, restrt_, l_diff_, dmax__,delay_){
    k_y <- k_y_
    ys_addtr <- ys_addtr_
    trend <- trend_
    p_max <- p_max_
    dmax_ <- dmax__
    l_diff <- l_diff_
    delay <- delay_
    c_rank<- c_rank_
    coin_vec<- coin_vec_
    restrc<- restrc_
    restrt<- restrt_
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


}