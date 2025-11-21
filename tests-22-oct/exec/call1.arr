
fun foo(f :: (Number -> Nothing)) -> Nothing block:
  f(42)
  f(89)
end

fun println(x :: Number) -> Nothing block:
  print(x)
  print("\n")
  nothing
end

foo(println)
