
fun iter(f :: (Any -> Nothing), l :: List<Any>) -> Nothing:
  cases (List<Any>) l:
  | empty      => nothing
  | link(x, s) => block: f(x)
                         iter(f, s) end
  end
end

l123 :: List<Number> = link(1, link(2, link(3, empty)))

fun foo(x :: Any) -> Nothing block:
  print(x)
  print("\n")
  nothing
end

iter(foo, l123)

