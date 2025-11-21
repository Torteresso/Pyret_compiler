
fun iter<a>(f :: (a -> Nothing), l :: List<a>) -> Nothing:
  cases (List<a>) l:
  | empty      => nothing
  | link(x, s) => block: f(x)
                         iter(f, s) end
  end
end

l1234 :: List<Number> = link(1, link(2, link(3, link(4, empty))))
l56 :: List<Number> = link(5, link(6, empty))
ll123456 :: List<List<Number>> = link(l1234, link(l56, empty))

fun foo(x :: Any) -> Nothing block:
  print(x)
  print("\n")
  nothing
end

fun print_list(x :: List<Number>) -> Nothing block:
  iter(foo, x)
  print("---\n")
  nothing
end

iter(print_list, ll123456)


