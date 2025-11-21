
fun f<a>(x :: a) -> a block:
  if false: f(1) else: 2 end
  x
end
print(f(42))
print(f("a\n"))
