x <- read.lp("groupselect.lp")
x
solve(x)
get.objective(x)
get.variables(x)
get.constraints(x)
get.sensitivity.objex(x)
get.sensitivity.rhs(x)
