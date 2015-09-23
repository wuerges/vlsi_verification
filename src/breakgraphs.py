import sys
import subprocess

i = sys.stdin.read()

ds = i.split("digraph")

print(ds)
ps = []


for i, d in enumerate(ds[1:]):
    with open("gout-%d.dot" % i, "wb") as f:
        f.write(("digraph " + d).encode("utf-8"))


