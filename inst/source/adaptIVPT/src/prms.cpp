#include <iostream>
#include <cmath>
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <algorithm>
#include <iterator>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "misc.h"
#ifdef _OPENMP
  #include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]] 
// [[Rcpp::depends(RcppArmadillo,RcppProgress)]]


/********************************
n is # of donors
r is # of skin replicates
Test:      n by r matrix
Reference: n by r matrix
********************************/
// [[Rcpp::export]]
arma::vec msabe(const arma::mat &Test,
           const arma::mat &Reference,
           const double &m,
           const double &sigma_W0,
           const double &sig_level) {
    int n = Test.n_rows;
    int r = Test.n_cols;
    const double theta = std::pow(std::log(m) / sigma_W0, 2.0);
    const double t_cutoff = R::qt(1.0 - sig_level, static_cast<double>(n-1), 1, 0);
    const double chi2_cutoff = R::qnchisq(1.0 - sig_level, static_cast<double>(n*(r-1)), 0.0, 1, 0);
    arma::vec Rj_bar = arma::mean(Reference, 1);
    double S2_WR = arma::accu(arma::square(Reference.each_col() - Rj_bar)) / static_cast<double>(n*(r-1));
    arma::vec Ij = arma::mean(Test - Reference, 1);
    double Ibar = arma::mean(Ij);
    double S2_I = arma::var(Ij);

    double out = 0.0;
    double upperbound = 0.0;
    double lowerbound = 0.0;
    if (std::sqrt(S2_WR) >= 0.294) {
        double X = Ibar * Ibar - S2_I / static_cast<double>(n);
        double Y = -theta * S2_WR;
        double X_beta = std::pow(std::abs(Ibar) + t_cutoff * std::sqrt(S2_I / static_cast<double>(n)), 2.0);
        double Y_beta = -theta * static_cast<double>((r-1)*n) * S2_WR / chi2_cutoff;
        double X_diff = X_beta - X;
        double Y_diff = Y_beta - Y;
        double V = X_diff * std::abs(X_diff) + Y_diff * std::abs(Y_diff);
        double SABE_upperbound = X + Y + sgn(V) * std::sqrt(std::abs(V));
        upperbound = SABE_upperbound;
        if (SABE_upperbound <= 0.0 && std::abs(Ibar) < std::log(m)) {
            out = 1.0;
        }
    } else {
        double TOST_upperbound = Ibar + t_cutoff * std::sqrt(S2_I / static_cast<double>(n));
        double TOST_lowerbound = Ibar - t_cutoff * std::sqrt(S2_I / static_cast<double>(n));
        upperbound = TOST_upperbound;
        lowerbound = TOST_lowerbound;
        if (TOST_lowerbound >= -std::log(m) && TOST_upperbound <= std::log(m)) {
            out = 1.0;
        }
    }
    return {out, Ibar, S2_WR, S2_I, lowerbound, upperbound};
}

// [[Rcpp::export]]
double prms(const int &n,
            const int &r,
            const double &sigma2_WT,
            const double &sigma2_WR,
            const double &logGMR,
            const double &m,
            const double &sigma_W0,
            const double &sig_level, // significance level (alpha)
            const int &nsim,
            const int &ncores) {
    const double theta = std::pow(std::log(m) / sigma_W0, 2.0);
    const double t_cutoff = R::qt(1.0 - sig_level, static_cast<double>(n-1), 1, 0);
    const double chi2_cutoff = R::qnchisq(1.0 - sig_level, static_cast<double>(n*(r-1)), 0.0, 1, 0);
    arma::vec out(nsim, arma::fill::zeros);
    arma::vec S2_WRs(nsim, arma::fill::zeros);
    arma::vec S2_Is(nsim, arma::fill::zeros);
    for (int isim = 0; isim < nsim; ++isim) {
        S2_WRs(isim) = ::Rf_rnchisq(static_cast<double>(n*(r-1)), 0.0) * sigma2_WR / static_cast<double>(n*(r-1));
        S2_Is(isim) = ::Rf_rnchisq(static_cast<double>(n-1), 0.0) * (sigma2_WR + sigma2_WT) / static_cast<double>(r*(n-1));
    }
    arma::vec Ibars(nsim, arma::fill::randn);
    Ibars *= std::sqrt((sigma2_WT + sigma2_WR) / static_cast<double>(n*r));
    Ibars += logGMR;


    #ifdef _OPENMP
    #pragma omp parallel for schedule(static) num_threads(ncores)
    #endif
    for (int isim = 0; isim < nsim; ++isim) {
        double S2_I = S2_Is(isim);
        double S2_WR = S2_WRs(isim);
        double Ibar = Ibars(isim);
        double test_function = sim1_mixedscaling(n, r, m, theta, t_cutoff, chi2_cutoff, S2_I, S2_WR, Ibar);
        out(isim) = test_function;
    }
    return arma::mean(out);
}

// [[Rcpp::export]]
int reestimate_samplesize(const int &n_init,
           const int &r,
           const double &lGMR,
           const double &S2_WR,
           const double &alpha_level,
           const double &m,
           const double &sigma_W0,
           const double &target_power,
           const int &nmax,
           const int &nsim,
           const int &ncores) {
    int n = n_init + 1;
    // Double n until the passing rate is >= target_power
    double out_old = 0.0;
    while (n < nmax) {
        out_old = prms(n, r, S2_WR, S2_WR, lGMR, m, sigma_W0, alpha_level, nsim, ncores);
        if (out_old >= target_power) {
            break;
        } else {
            n *= 2;
        }
    }
    if (n >= nmax) {
        return nmax;
    }

    // Binary search
    int n_lower = n / 2;
    int n_upper = n;
    double out = out_old;
    while (n_upper - n_lower > 1) {
        n = (n_upper + n_lower) / 2;
        out = prms(n, r, S2_WR, S2_WR, lGMR, m, sigma_W0, alpha_level, nsim, ncores);
        if (out < target_power) {
            n_lower = n;
        } else {
            n_upper = n;
        }
    }
    return n_upper;
}
