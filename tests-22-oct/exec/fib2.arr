
fun fib(a :: Number, b :: Number, n :: Number) -> Number:
  if n == 0:
    a
  else:
    fib(b, a + b, n - 1)
  end
end

print(fib(0, 1, 10))
print("\n")

