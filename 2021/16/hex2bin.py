xs = input().strip()
xs = "".join(f"{int(x, 16):04b}" for x in xs)

print(xs, end="")
