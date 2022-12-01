ll = [[parse(Int, x) for x in split(s)] for s in split(read("input", String), "\n\n")]
println(maximum(sum(l) for l in ll))
println(sum(sort([sum(l) for l in ll])[end-2:end]))
