f = lambda n, t: next(i+n for i in range(len(t)-n) if len(set(t[i:i+n])) == n)
inp = open("input").read()
print(f(4, inp))
print(f(14, inp))
