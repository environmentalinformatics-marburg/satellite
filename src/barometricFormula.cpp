#include <Rcpp.h>
using namespace Rcpp;

//// difference -----
//// calculate absolute difference between dem and geopotential heights
// [[Rcpp::export]]
NumericVector difference(double x, NumericVector y) {
  
  int len = y.size();
  NumericVector out(len);
  for (int i = 0; i < len; i++) {
    out[i] = x - y[i];
  }
  
  return abs(out);
}

//// is.na() -----
//// (taken from http://gallery.rcpp.org/articles/working-with-missing-values/)
// [[Rcpp::export]]
LogicalVector isNA(NumericVector x) {
  int n = x.size();
  LogicalVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = NumericVector::is_na(x[i]);
  }
  return out;
}

//// na.omit() -----
//// (taken from http://stackoverflow.com/questions/19156353/remove-na-values-efficiently)
// [[Rcpp::export]]
NumericVector naOmit(NumericVector x) {
  int len = x.size();
  std::vector<double> out(len);
  
  int n = 0;
  for (int i = 0; i < len; i++) {
    if (x[i] == x[i]) {
      out[n] = x[i];
      n++;
    }
  }
  
  out.resize(n);
  return Rcpp::wrap(out);
}

//// which.min() -----
// [[Rcpp::export]]
int whichMin(NumericVector x) {
  
  int len = x.size();
  
  NumericVector y(len);
  y = naOmit(x);
  
  bool out = false;
  int i = 0;
  while ((!out) | (i == 0)) {
    out = (min(y) == x[i]);
    i++;
  }
  
  return i - 1;
}

//// barometric formula -----
//// (taken from https://en.wikipedia.org/wiki/Barometric_formula)
// [[Rcpp::export]]
double barometricFormula(double z, NumericVector gp, NumericVector ta, 
                         IntegerVector p) {
  
  // identify minimum difference between dem and geopotential heights
  NumericVector differences = difference(z, gp);
  int id = whichMin(differences);

  // if subsequent geopotential is NA, automatically use 1st algorithm 
  bool isna = (gp[id + 1] != gp[id + 1]);
  bool smallerThan = false;

  if (isna) {
    smallerThan = true;
  } else {
    smallerThan = differences[id - 1] < differences[id + 1];
  }
  
  // base elevation, pressure, and temperature
  double h0, h1, t, p0;
  if (smallerThan) {
    h0 = gp[id];
    h1 = z;
    t = ta[id];
    p0 = p[id];
  } else {
    h0 = gp[id + 1];
    h1 = z;
    t = ta[id + 1];
    p0 = p[id + 1];
  }

  // calculate pressure at z-level
  double dh, g, M, R;

  dh = h1 - h0;
  g = 9.80665;
  M = 0.0289644; // taken from braeunling (2015) flugzeugtriebwerke.
  R = 8.3144598; // taken from http://physics.nist.gov/cgi-bin/cuu/Value?r.

  double out;
  out = p0 * exp((-1) * (g * M * dh) / (R * (t + 273.15)));
  
  return out;
}

//// run barometric formula on matrix
// [[Rcpp::export]]
NumericVector run_barometricFormula(NumericMatrix a, NumericMatrix b, 
                                    NumericVector dem, IntegerVector p) {
  
  int nRows = a.nrow(), nCols = a.ncol();
  NumericVector out(nRows);
  
  NumericVector gp(nCols), ta(nCols);

  for (int i = 0; i < nRows; i++) {

    gp = a(i, _);
    ta = b(i, _);

    if (all(is_na(gp)) | all(is_na(ta))) {
      out[i] = -999;
    } else {
      out[i] = barometricFormula(dem[i], gp, ta, p);
    }
  }
  
  return out;
}