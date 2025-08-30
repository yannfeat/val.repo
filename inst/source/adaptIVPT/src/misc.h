#ifndef IVPT_MISC_H
#define IVPT_MISC_H

/************************
Sign function
(templated and type-safe)
************************/
template <typename T> T sgn(T val) {
    return (T(0) < val) - (val < T(0));
}

double sim1_mixedscaling(const int &n,
                          const int &r,
                          const double &m,
                          const double &theta,
                          const double &t_cutoff, // = t_{(1-\alpha, n-1)}
                          const double &chi2_cutoff,
                          const double &S2_I,
                          const double &S2_WR,
                          const double Ibar);

#endif
