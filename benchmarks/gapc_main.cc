// the following code is used in out_main.cc for measuring CPU time of gapc programs
// the function getCPUTime() is defined here: 
// http://nadeausoftware.com/articles/2012/03/c_c_tip_how_measure_cpu_time_benchmarking

// the program is run with:
// ./out "ggcguaggcgccgugcuuuugcuccccgcgcgcuguuuuucucgcugacuuucagcgggcggaaaagccucggccugccgccuuccaccguucauucuag"   for 100 input length
// ./out `printf "ggcguaggcgccgugcuuuugcuccccgcgcgcuguuuuucucgcugacuuucagcgggcggaaaagccucggccugccgccuuccaccguucauucuag"%.0s {1..2}`   for 200 input length
// ./out `printf "ggcguaggcgccgugcuuuugcuccccgcgcgcuguuuuucucgcugacuuucagcgggcggaaaagccucggccugccgccuuccaccguucauucuag"%.0s {1..3}`   for 300 input length etc.
// the inputs are equal to those in Benchmark.hs

...
#include "getCPUTime.c"
...
int main(int argc, char **argv)
{
...
  std::cout.precision(15);

  double elapsed_secs_total = 0;
  for (unsigned int i = 1; i <= 100; i++) {
    obj.init(opts);
    double begin = getCPUTime( );
    obj.cyk();
    gapc::return_type res = obj.run();
    double end = getCPUTime( );
    double elapsed_secs = end - begin;
    elapsed_secs_total += elapsed_secs;
    std::cout << "cpu time: " << std::fixed << elapsed_secs << std::endl;
  }
  std::cout << "total cpu time (100x): " << std::fixed << elapsed_secs_total << std::endl;
  std::cout << "mean cpu time: " << std::fixed << (elapsed_secs_total / 100.0) << std::endl;
...
}