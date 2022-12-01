l = [sum(map(int, x.splitlines())) for x in open("input").read().split("\n\n")]
print(max(l))
print(sum(sorted(l)[-3:]))
