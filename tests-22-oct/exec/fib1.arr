
fun fib(n :: Number) -> Number:
  if n <= 1:
    n
  else:
    fib(n - 2) + fib(n - 1)
  end
end

print(fib(10))
print("\n")

