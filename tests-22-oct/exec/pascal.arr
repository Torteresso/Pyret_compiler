fun print_list(n :: Number, l :: List<Number>) -> Nothing:
  if n <= 0: block: print("\n")
                    nothing end
  else: cases(List<Number>) l:
  | empty => raise("print_list")
  | link(x, r) => block: print(if x == 0: "." else: "*" end)
                         print_list(n - 1, r) end
  end
  end
end

fun make(n :: Number, v :: Number) -> List<Number> :
  if n == 0: empty else: link(v, make(n - 1, v)) end
end

fun update_row(prev :: Number, r :: List<Number>) -> List<Number> :
  cases(List<Number>) r:
  | empty => empty
  | link(x, xs) => link(num-modulo(x + prev, 7), update_row(x, xs))
  end
end

fun compute_row(r :: List<Number>) -> List<Number> :
  cases(List<Number>) r:
  | empty => raise("compute_row")
  | link(x, xs) => link(1, update_row(1, xs))
  end
end

fun loop(n :: Number, i :: Number, r :: List<Number>) -> Nothing :
  if i < n block:
    s = compute_row(r)
    print_list(i, s)
    loop(n, i + 1, s)
  else:
    nothing
  end
end

n = 42
loop(n, 0, make(n + 1, 0))
