## Timing the loops to pull data

# Example from the documentation
See help(Sys.sleep).

For example, from ?Sys.sleep

testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
testit(3.7)

Yielding

> testit(3.7)
user  system elapsed 
0.000   0.000   3.704 