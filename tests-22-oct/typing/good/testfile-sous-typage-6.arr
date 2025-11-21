
fun foo(f :: (Number -> String)) -> String: f(3) end
fun f1(n :: Number) -> String: "abc" end
print(foo(f1))
