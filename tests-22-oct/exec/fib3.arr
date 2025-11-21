
fun fib(n :: Number) -> Number:
  fun loop(a :: Number, b :: Number, k :: Number) -> Number:
    if k == 0:
      a
    else:
      loop(b, a + b, k - 1)
    end
  end
  loop(0, 1, n)
end

print(fib(10))
print("\n")

