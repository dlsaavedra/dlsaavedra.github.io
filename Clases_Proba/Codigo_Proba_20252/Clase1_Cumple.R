n = 50

num_no_repitan = choose(365,n) * factorial(n);num_no_repitan
denom_no_repitan = 365**n;denom_no_repitan

prob_no_repitan = num_no_repitan/denom_no_repitan;prob_no_repitan

prob_repitan = 1 - prob_no_repitan;prob_repitan

print(paste0("Probabilidad que en una clase de ",n," alumnos almenos un par coincidan en el cumpleaños es de ",round(prob_repitan*100, digits=2), "%"))
      
