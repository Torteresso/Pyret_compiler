fun div_(x :: Number, y :: Number) -> Number:
  (x - num-modulo(x, y)) / y
end

fun add(x :: Number, y :: Number) -> Number : x + y end
fun sub(x :: Number, y :: Number) -> Number : x - y end
fun mul(x :: Number, y :: Number) -> Number :
  t = x * y
  div_(t + div_(8192, 2), 8192)
end
fun divi(x :: Number, y :: Number) -> Number :
  t = x * 8192
  div_(t + div_(y, 2), y)
end
fun of_int(x :: Number) -> Number : x * 8192 end

zero = of_int(0)
two = of_int(2)
four = of_int(4)

fun iter(n :: Number, a :: Number, b :: Number, xn :: Number, yn :: Number)
  -> Boolean :
  if n == 10 block:
    true
  else:
    xn2 = mul(xn, xn)
    yn2 = mul(yn, yn)
    if add(xn2, yn2) > four:
      false
    else:
      iter(n + 1, a, b, add(sub(xn2, yn2), a),
                        add(mul(two, mul(xn, yn)), b))
    end
  end
end

fun inside(x :: Number, y :: Number) -> Boolean :
  iter(0, x, y, zero, zero)
end

fun run(steps :: Number) -> Nothing :
  xmin = of_int(-2)
  xmax = of_int(1)
  deltax = divi(sub(xmax, xmin), (two * steps))
  ymin = of_int(-1)
  ymax = of_int(1)
  deltay = divi(sub(ymax, ymin), of_int(steps))
  fun fori(i :: Number) -> Nothing :
    if i >= steps block:
      nothing
    else:
      y = add(ymin, mul(of_int(i), deltay))
      fun forj(j :: Number) -> Nothing :
        if j >= (2 * steps) block:
          nothing
        else:
          x = add(xmin, mul(of_int(j), deltax))
          print(if inside(x, y): "0" else: "1" end)
          forj(j + 1)
        end
      end
      forj(0)
      print("\n")
      fori(i + 1)
    end
  end
  fori(0)
end

run(30)
