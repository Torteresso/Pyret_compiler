fun f<a>(x :: a) -> a: x end

fun g(x :: Any) -> Any: x end

print(f("hello\n"))
print(g("hello\n"))
print(f(42))
print(g(42))
print(f(link("a", link("\n", link("b", empty)))))
print(g(link("a", link("\n", link("b", empty)))))
print("\n")
