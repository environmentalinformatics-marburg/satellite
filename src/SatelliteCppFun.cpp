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
