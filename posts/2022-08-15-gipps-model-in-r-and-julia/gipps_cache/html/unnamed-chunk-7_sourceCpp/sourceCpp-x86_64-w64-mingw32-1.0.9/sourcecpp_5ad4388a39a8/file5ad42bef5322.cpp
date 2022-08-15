#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
DataFrame for_loop_gipps(double resolution,
                         int n,
                         int time_length,
                         double tau,
                         double an,
                         double bn_const,
                         double Vn,
                         double bcap,
                         double ln1,

                         NumericVector Time,
                         NumericVector vn_ff,
                         NumericVector vn_cf,
                         NumericVector vn,
                         NumericVector vn1,
                         NumericVector sn,
                         NumericVector xn,
                         NumericVector xn1,
                         NumericVector deltav,
                         NumericVector bn
) {


  for(int t = 1; t < (time_length-1); t++) {


    // ## free flow
    vn_ff[t] = vn[t-1] + (2.5 * an * tau * (1 - (vn[t-1])/Vn)) * ((0.025 + pow((vn[t-1]/Vn), 0.5)));

    // if (Rcpp::NumericVector::is_na(vn1[t-1])) {
    //
    //   vn1[t-1] = 0;
    //
    // } else {
    //
    //   vn1[t-1] = vn1[t-1];
    //
    //   }

    // ## car following
    vn_cf[t] = (bn_const * tau) + sqrt(
      (pow(bn_const,2) * pow(tau,2)) - (bn_const * (2 * (xn1[t-1] - ln1 - xn[t-1]) - (vn[t-1] * tau) - (pow((vn1[t-1]),2)/bcap)))
    );


    // ## gipps speed


    if (Rcpp::NumericVector::is_na(vn1[t-1])){

      vn[t] = vn_ff[t];

    } else {

      if (vn_ff[t] < vn_cf[t]){

        vn[t] = vn_ff[t];

      } else {

        vn[t] = vn_cf[t];

        }

    }

    // ### if the speed is negative, make it zero


    if (vn[t] < 0) {

      vn[t] = 0;

    } else {

      vn[t] = vn[t];

    }


    // ## acceleration
    bn[t-1] = (vn[t] - vn[t-1])/(resolution);



    // ## position
    xn[t] = xn[t-1] + (vn[t-1] * resolution) + (0.5 * bn[t-1] * pow(resolution, 2));


    // # spacing
    sn[t] = xn1[t] - xn[t];


    // # speed difference
    deltav[t] = vn[t] - vn1[t];


  }

  DataFrame df = DataFrame::create(Named("fvn") = n,
                                   Named("Time") = Time,
                                   Named("xn1") = xn1,
                                   Named("vn1") = vn1,
                                   Named("ln1") = ln1,
                                   Named("bn") = bn,
                                   Named("xn") = xn,
                                   Named("vn") = vn,
                                   Named("sn") = sn,
                                   Named("deltav") = deltav,
                                   Named("vn_ff") = vn_ff,
                                   Named("vn_cf") = vn_cf);

  return df;


}


#include <Rcpp.h>
#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// for_loop_gipps
DataFrame for_loop_gipps(double resolution, int n, int time_length, double tau, double an, double bn_const, double Vn, double bcap, double ln1, NumericVector Time, NumericVector vn_ff, NumericVector vn_cf, NumericVector vn, NumericVector vn1, NumericVector sn, NumericVector xn, NumericVector xn1, NumericVector deltav, NumericVector bn);
RcppExport SEXP sourceCpp_1_for_loop_gipps(SEXP resolutionSEXP, SEXP nSEXP, SEXP time_lengthSEXP, SEXP tauSEXP, SEXP anSEXP, SEXP bn_constSEXP, SEXP VnSEXP, SEXP bcapSEXP, SEXP ln1SEXP, SEXP TimeSEXP, SEXP vn_ffSEXP, SEXP vn_cfSEXP, SEXP vnSEXP, SEXP vn1SEXP, SEXP snSEXP, SEXP xnSEXP, SEXP xn1SEXP, SEXP deltavSEXP, SEXP bnSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type resolution(resolutionSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type time_length(time_lengthSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< double >::type an(anSEXP);
    Rcpp::traits::input_parameter< double >::type bn_const(bn_constSEXP);
    Rcpp::traits::input_parameter< double >::type Vn(VnSEXP);
    Rcpp::traits::input_parameter< double >::type bcap(bcapSEXP);
    Rcpp::traits::input_parameter< double >::type ln1(ln1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Time(TimeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vn_ff(vn_ffSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vn_cf(vn_cfSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vn(vnSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vn1(vn1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sn(snSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xn(xnSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xn1(xn1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type deltav(deltavSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bn(bnSEXP);
    rcpp_result_gen = Rcpp::wrap(for_loop_gipps(resolution, n, time_length, tau, an, bn_const, Vn, bcap, ln1, Time, vn_ff, vn_cf, vn, vn1, sn, xn, xn1, deltav, bn));
    return rcpp_result_gen;
END_RCPP
}
