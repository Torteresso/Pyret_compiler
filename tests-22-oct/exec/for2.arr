
l = link(1, link(2, link(3, link(4, link(5, empty)))))

a = for fold(s :: Number from 0, x :: Number from l) -> Number:
  s + x
end

print(a)
print("\n")

