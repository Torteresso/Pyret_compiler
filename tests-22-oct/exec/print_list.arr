fun print_list(l :: List<Any>) -> Nothing:
  cases(List<Any>) l:
  | empty => block: print("\n")
                    nothing end
  | link(x, r) => block: print(x)
                         print(",")
                         print_list(r) end
  end
end

l123 :: List<Number> = link(1, link(2, link(3, empty)))

print_list(l123)
