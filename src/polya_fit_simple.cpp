/**********************************************************************
		        Joint Sentiment-Topic (JST) Model
***********************************************************************

(C) Copyright 2013, Chenghua Lin and Yulan He

Written by: Chenghua Lin, University of Aberdeen, chenghua.lin@abdn.ac.uk.
Part of code is from http://gibbslda.sourceforge.net/.

This file is part of JST implementation.

JST is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.

JST is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA

***********************************************************************/

#include "polya_fit_simple.h"
#include "math_func.h"
#include <Rcpp.h>
#include <math.h>
#include <iostream>
#include <string>
#include <algorithm>
#include <vector>

using namespace std;


int polya_fit_simple(int ** data, double * alpha, int _K, int _nSample) {
	int K = _K;                 // hyperparameter dimension
	int nSample = _nSample;     // total number of samples, i.e.documents
	int polya_iter = 100000;    // maximum number of fixed point iterations
	int ifault1, ifault2;

	double sum_alpha_old;
	double sum_g = 0; // sum_g = sum_digama(data[i][k] + old_alpha[k]),
	double sum_h = 0; // sum_h + sum_digama(data[i] + sum_alpha_old) , where data[i] = sum_data[i][k] for all k,
	bool sat_state = false;
	int i, k, j;

	// std::vector rather than raw new[] so the buffers are released on every
	// return path (the previous implementation leaked on each call).
	std::vector<double> old_alpha(K, 0.0);
	// the sum of the counts of each data sample P = {P_1, P_2,...,P_k}
	std::vector<double> data_row_sum(nSample, 0.0);

	// data_row_sum
	for (i = 0; i < nSample; i++) {
		for (k = 0; k < K; k++) {
			data_row_sum[i] += *(*(data+k)+i) ;
		}
	}

	// simple fix point iteration
	for (i = 0; i < polya_iter; i++) {  // reset sum_alpha_old
		sum_alpha_old = 0;
		// update old_alpha after each iteration
		for (j = 0; j < K; j++) {
			old_alpha[j] = *(alpha+j);
		}

		 // calculate sum_alpha_old
		 for (j = 0; j < K; j++) {
			 sum_alpha_old += old_alpha[j];
		 }

		 // sum_h, and digama(sum_alpha_old), do not depend on k: both are fully
		 // determined by data_row_sum and sum_alpha_old, which are fixed for the
		 // whole fixed-point iteration. Computing them once instead of once per
		 // dimension divides the inner work by K.
		 sum_h = 0;
		 for (j = 0; j < nSample; j++) {
			 sum_h += digama(data_row_sum[j] + sum_alpha_old, &ifault1);
		 }
		 const double denom = sum_h - nSample * digama(sum_alpha_old, &ifault2);

		 // A zero or non-finite denominator would turn every alpha into inf/nan
		 // and silently poison the model. Leave alpha at its last valid value.
		 if (!R_FINITE(denom) || denom == 0.0) {
			 break;
		 }

		 for (k = 0; k < K; k++) {
			 // digama(old_alpha[k]) is needed by the update below anyway, and it is
			 // also the value of every term whose count is zero. Since the counts are
			 // sparse -- most documents contain no word from a given dimension --
			 // reusing it skips the majority of the digama() evaluations here.
			 const double dig_old_alpha = digama(old_alpha[k], &ifault1);

			 sum_g = 0;

			 // calculate sum_g[k]
			 for (j = 0; j < nSample; j++) {
				 const int count = *(*(data+k)+j);
				 sum_g += (count == 0) ?
					 dig_old_alpha :
					 digama(count + old_alpha[k], &ifault1);
			 }

			 // update alpha (new)
			 const double candidate = old_alpha[k] * (sum_g - nSample * dig_old_alpha) / denom;

			 // a Dirichlet parameter must stay finite and strictly positive; keep the
			 // previous value rather than propagating a degenerate one
			 *(alpha+k) = (R_FINITE(candidate) && candidate > 0.0) ? candidate : old_alpha[k];
		 }

		 // terminate iteration ONLY if each dimension of {alpha_1, alpha_2, ... alpha_k} satisfy the termination criteria,
		 for (j = 0; j < K; j++) {
			 if (fabs( *(alpha+j) - old_alpha[j]) > 0.000001) break;
			 if ( j == K-1) {
				 sat_state = true;
			 }
		 }

		// check whether to terminate the whole iteration
		if(sat_state) {
			break;
		}

	}
  return 0;
}
