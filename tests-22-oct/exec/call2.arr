
fun foo(f :: (Number -> Nothing), n :: Number) -> Nothing:
  if n == 0 block:
    nothing
  else:
    f(n)
    foo(f, n - 1)
  end
end

fun println(x :: Number) -> Nothing block:
  print(x)
  print("\n")
  nothing
end

foo(println, 10)
