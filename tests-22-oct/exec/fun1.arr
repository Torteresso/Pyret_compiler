
fun foo(f :: (Number -> Number)) -> Number: f(42) end
fun bar(f :: (Any -> Any)) -> Any: f("\n") end

foo(print)
bar(print)
print(foo(print))
print(bar(print))


