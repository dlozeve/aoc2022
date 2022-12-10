import sys, itertools as it
f = lambda s: (1, 0) if s.startswith("noop") else (2, int(s.split()[1]))
lengths, vals = map(list, zip(*[f(x) for x in open(sys.argv[1]).readlines()]))
pos = list(it.chain.from_iterable(it.repeat(x, n) for n, x in zip([1]+lengths+[1], [1]+list(it.accumulate(vals, initial=1)))))
print(sum(i * pos[i] for i in range(20, 6*40, 40)))
print("\n".join("".join(" █"[-1 <= pos[40*j+i+1] - i <= 1] for i in range(40)) for j in range(6)))
