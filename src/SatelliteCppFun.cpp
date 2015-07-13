#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix insertMinReqRem(IntegerVector anHs, IntegerVector anHt) {

   // setup output matrix
   int nRows = anHs.size(), nCols = anHt.size();
   NumericMatrix mnOut(nRows, nCols);
   
   // setup temporary vectors
   IntegerVector anValRows(nRows);
   int nSumRows;
   int nReq;
   
   IntegerVector anValCols(nCols);
   int nSumCols;
   int nRem;
   
   IntegerVector anReqRem(2);
   
   // loop over ht$counts
   for (int i = 0; i < nCols; i++) {
     for (int j = 0; j < nRows; j++) {
       
       // pixelsreq
       for (int k = 0; k <= j; k++) {
         anValRows[k] = mnOut(k, i);
       }
       nSumRows = sum(anValRows);
       nReq = anHt[i] - nSumRows;
       anReqRem[0] = nReq;
       
       // reset temporary vector
       for (int k = 0; k < nRows; k++) {
         anValRows[k] = 0;
       }
       
       // pixelrem
       for (int l = 0; l <= i; l++) {
         anValCols[l] = mnOut(j, l);
       }
       nSumCols = sum(anValCols);
       nRem = anHs[j] - nSumCols;
       anReqRem[1] = nRem;

       // reset temporary vector
       for (int l = 0; l < nCols; l++) {
         anValCols[l] = 0;
       }
       
       // insert minimum value into output matrix
       mnOut(j, i) = min(anReqRem); 
     }
   }
   
   return mnOut;
}

////////////////////////////////////////////////////////////////////////////////
// functions required for computation of scattering model in calcPathRadDOS ////
////////////////////////////////////////////////////////////////////////////////

struct add_multiple {
  int incr;
  int count;
  add_multiple(int incr)
    : incr(incr), count(0)
    {}
  inline int operator()(int d) {
    return d + incr * count++;
  }
};

// [[Rcpp::export]]
Rcpp::NumericVector seqC(double from_, double to_, double by_ = 1.0) {
  int adjust = std::pow(10, std::ceil(std::log10(10 / by_)) - 1);
  int from = adjust * from_;
  int to = adjust * to_;
  int by = adjust * by_;
  
  std::size_t n = ((to - from) / by) + 1;
  Rcpp::IntegerVector res = Rcpp::rep(from, n);
  add_multiple ftor(by);
  
  std::transform(res.begin(), res.end(), res.begin(), ftor);
  return Rcpp::NumericVector(res) / adjust;
}

// [[Rcpp::export]]
NumericVector ScatteringModel(NumericMatrix mnBandWls, double dScatCoef) {
  
  int nRows = mnBandWls.nrow();
  NumericVector anOut(nRows);
  
  double x;
  double y;
  double by = 0.001;
  List lsActBand(nRows);
  
  NumericVector anTmp;
  
  for (int i = 0; i < nRows; i++) {
    x = mnBandWls(i, 0);
    y = mnBandWls(i, 1);
    
    lsActBand[i] = seqC(x, y, by);
    
    anTmp = lsActBand[i];
    
    for (int j = 0; j < anTmp.size(); j++) {
      anTmp[j] = pow(anTmp[j], dScatCoef); 
    }
    
    anOut[i] = mean(anTmp);
  }
  
  return anOut;
}
