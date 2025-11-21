fun len(l :: List<Any>) -> Number:
  cases(List<Any>) l:
    | empty => 0
    | link(f, r) => 1 + len(r)
  end
end

l1234 :: List<Number> = link(1, link(2, link(3, link(4, empty))))

print(len(l1234))
print("\n")
