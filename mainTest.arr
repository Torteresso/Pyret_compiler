fun g(x :: Number, y :: Number) -> (Number -> Number):
  lam(z :: Number) -> Number:
    x + y + z
  end
end

g(1, 2)(3) 

