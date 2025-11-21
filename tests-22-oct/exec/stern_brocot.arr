
fun str(n :: Number) -> String :
       if n == 0 : "0"
  else if n == 1 : "1"
  else if n == 2 : "2"
  else if n == 3 : "3"
  else if n == 4 : "4"
  else if n == 5 : "5"
  else if n == 6 : "6"
  else if n == 7 : "7"
  else if n == 8 : "8"
  else: "9"
  end
end

fun compute(l :: Number, a :: Number, b :: Number, c :: Number, d :: Number)
  -> String :
  if l == 0:
    str(a) + "/" + str(b) + " "
  else:
    compute(l - 1, a, b, a + c, b + d) + compute(l - 1, a + c, b + d, c, d)
  end
end

fun stern_brocot(l :: Number) -> String :
  compute(l, 0, 1, 1, 1)
end

print(stern_brocot(4))
print("\n")

