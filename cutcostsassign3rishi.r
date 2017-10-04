x <- read.lp("cutcostassign3rishi.lp")
x
solve(x)
get.objective(x)
get.variables(x)
get.constraints(x)
get.sensitivity.objex(x)
get.sensitivity.rhs(x)

#This is an optimal solution . Not sure if we need have any slog times . But will be working on it further more.