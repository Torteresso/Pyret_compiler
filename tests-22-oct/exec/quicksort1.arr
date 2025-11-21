
fun print_list(l :: List<Any>) -> Nothing:
  cases(List<Any>) l:
  | empty => block: print("\n")
                    nothing end
  | link(x, r) => block: print(x)
                         print(",")
                         print_list(r) end
  end
end

fun append<a>(l1 :: List<a>, l2 :: List<a>) -> List<a>:
  cases(List<a>) l1:
  | empty => l2
  | link(x, r) => link(x, append(r, l2))
  end
end

fun qsort(l :: List<Number>) -> List<Number>:
  cases(List<Number>) l:
  | empty => empty
  | link(x, r) =>
    cases(List<Number>) r:
    | empty => link(x, empty)
    | link(_, _) =>
        fun split(lt :: List<Number>, ge :: List<Number>,
                  td :: List<Number>) -> List<Number>:
          cases(List<Number>) td:
          | empty => append(qsort(lt), link(x, qsort(ge)))
          | link(y, s) => if y < x: split(link(y, lt), ge, s)
                          else: split(lt, link(y, ge), s) end
          end
        end
        split(empty, empty, r)
    end
  end
end

l123 :: List<Number> = link(1, link(2, link(3, empty)))
print_list(qsort(l123))
print_list(qsort(link(1, link(3, link(2, empty)))))
print_list(qsort(link(1, link(1, link(1, link(2, empty))))))
print_list(qsort(link(2, link(1, link(1, link(1, empty))))))
print_list(qsort(link(2, link(1, link(4, link(8,
                 link(2, link(3, link(1, link(1, empty))))))))))
