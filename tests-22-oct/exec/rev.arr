fun print_list(l :: List<Any>) -> Nothing:
  cases(List<Any>) l:
  | empty => block: print("\n")
                    nothing end
  | link(x, r) => block: print(x)
                         print(",")
                         print_list(r) end
  end
end

fun rev_append<a>(l :: List<a>, s :: List<a>) -> List<a> :
  cases(List<a>) l:
  | empty => s
  | link(x, r) => rev_append(r, link(x, s))
  end
end

fun rev<a>(l :: List<a>) -> List<a> : rev_append(l, empty) end

l123 :: List<Number> = link(1, link(2, link(3, empty)))
print_list(rev(l123))

