#include <iostream>
#include <cmath>
#include "misc.h"


double sim1_mixedscaling(const int &n,
                          const int &r,
                          const double &m,
                          const double &theta,
                          const double &t_cutoff, // = t_{(1-\alpha, n-1)}
                          const double &chi2_cutoff,
                          const double &S2_I,
                          const double &S2_WR,
                          const double Ibar) { // = chi2_{(1-\alpha, n*(r-1))}
    double out = 0.0;
    if (std::sqrt(S2_WR) >= 0.294) {
        double X = Ibar * Ibar - S2_I / static_cast<double>(n);
        double Y = -theta * S2_WR;
        double X_beta = std::pow(std::abs(Ibar) + t_cutoff * std::sqrt(S2_I / static_cast<double>(n)), 2.0);
        double Y_beta = -theta * static_cast<double>((r-1)*n) * S2_WR / chi2_cutoff;
        double X_diff = X_beta - X;
        double Y_diff = Y_beta - Y;
        double V = X_diff * std::abs(X_diff) + Y_diff * std::abs(Y_diff);
        double SABE_upperbound = X + Y + sgn(V) * std::sqrt(std::abs(V));
        if (SABE_upperbound <= 0.0 && std::abs(Ibar) < std::log(m)) {
            out = 1.0;
        }
    } else {
        double TOST_upperbound = Ibar + t_cutoff * std::sqrt(S2_I / static_cast<double>(n));
        double TOST_lowerbound = Ibar - t_cutoff * std::sqrt(S2_I / static_cast<double>(n));
        if (TOST_lowerbound >= -std::log(m) && TOST_upperbound <= std::log(m)) {
            out = 1.0;
        }
    }
    return out;
}
