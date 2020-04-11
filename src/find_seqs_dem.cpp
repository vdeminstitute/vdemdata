/*
 * Rcpp function for identifying sequences of potential democratization episodes.
 */

#include <Rcpp.h>
#include <queue>

using namespace Rcpp;
using namespace std;

//' Identify sequences of potential democratization episodes
//'
//' This is a subfunction (c++) of vdemdata::get_dem
//' see the documentation of vdemdata::get_dem (?get_dem) for details on parameters, etc.
//'
//[[Rcpp::export]]
NumericVector find_seqs_dem(NumericVector v,
                        NumericVector r,
                        double start_incl = 0, // NOTE: all parameters are set to 0 here (the function needs some value), the settings of the parameters in the overarching R script are overwriting this and are therefore decisive!
                        double year_turn = 0, // NOTE also: the parameter "cum_incl" is only set and implemented in the R script and checks if potential episodes are "manifest" (cf. Wilson et al. 2020)
                        double cum_turn = 0,
                        int tolerance = 0) {

//creating a vector out of length v and taking the first difference (any anual change) of v and r as d and r2
  NumericVector out = NumericVector(v.size(), NumericVector::get_na()),
    d = diff(v), r2 = diff(r);

//setting up the objects that keep track of the country-specific episode number (count), the number of years of stasis there has been (tolerance_count) and the cumulative change during the time of stasis
// (NOTE: this is not cum_incl, since it looks at stasis years only - we call it "change" and set it to 0, since there should be - per definitionem - no change during stasis) as well as the length of the country-unit time series (d_len).
  queue<int> q;
  int count = 1, tolerance_count = 0;
  double change = 0.000;
  size_t d_len = d.size();

//start of a loop that looks for the beginning of an episode (d[i] >= start_incl). Records the index of the d value that is greater than start_incl in q. resets the tolerance count and total diff to zero since this if() statement only occurs at the beginning of an episode.
  for (int i = 0; i < d_len; i++) {
    if (d[i] >= start_incl) {
      q.push(i);
      tolerance_count = 0;
      change = 0;
    }

    // We haven't found the start of a sequence (q.empty() == T), keep going
    if (q.empty())
      continue;

    //increase the tolerance_count and the change count if there is stasis
    if (year_turn <= d[i] && d[i] < start_incl) {
      tolerance_count++;
      change += d[i];
    }

    // How do we end a seq? Either:
    //  - End of vector
    //  - Reach tolerance w/o another inc
    //  - Hit a NA
    //  - Decrease < cum_turn
    //  - Revert to v2x_regime == 0 (return to closed autocracy)
    if (i == d_len - 1 || tolerance_count == tolerance ||
        NumericVector::is_na(d[i]) || d[i] < year_turn|| change < cum_turn || (r2[i] < 0 && r[i+1] == 0)) {
      int head = q.front(),
        tail;

      // Include stasis period
      if (tolerance_count > 0)
        // If we're at the end of seq and it's not a stop value, we need to offset by 1 because of indexing values. The end index value of the end of the episode is tail.
        tail = (i == d_len - 1 && d[i] >= year_turn) ? i + 1 : i;
      else
        tail = (q.size() > 1) ? q.back() + 1 : head + 1;

      NumericVector sub = NumericVector(tail - head + 1);
      sub.fill(count);

      out[seq(head, tail)] = sub;

    //when the episode is over, add 1 to the country-specific episode number (count) and reset tolerance_count, change and q.
      count++;
      tolerance_count = 0;
      change = 0;

      queue<int> empty;
      swap(q, empty);
    }

  }

  return out;
}
