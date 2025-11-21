
fun foo(f :: (Number -> String)) -> String: f(3) end
fun f2(n :: Any) -> String: "abc" end
print(foo(f2))
