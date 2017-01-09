#include <Rcpp.h>
using namespace Rcpp;
//#include <iostream>
#include <fstream>

// [[Rcpp::export]]
void export_ecg_impl(NumericVector x, std::string& path)
{
  // Create output stream
  std::ofstream file(path.c_str());

  // Open file
  if (file.is_open())
  {
    // Walk through 'x' and write to file
    for (int i = 0; i < x.size(); i++)
    {
      file << x[i] << "\n";
    }

    // Close file
    file.close();
  }
}
