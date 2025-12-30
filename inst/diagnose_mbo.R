# Quick test: save as test_mbo_simple.R
Sys.setenv(R_ENABLE_JIT="0", OMP_NUM_THREADS="1")
library(mlrMBO)
obj = makeSingleObjectiveFunction(fn=function(x) sum(x^2), par.set=makeParamSet(makeNumericParam("x", -5, 5)))
result = mbo(obj, control=setMBOControlTermination(makeMBOControl(), iters=2))
print(result$y)