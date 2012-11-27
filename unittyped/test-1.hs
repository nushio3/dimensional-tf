import Prelude hiding ((+),(*),(-),(/))
import UnitTyped.SI (meter, second)
import UnitTyped.SI.Meta (kilo, centi)
import UnitTyped.SI.Derived.Time (hour,hertz)
import UnitTyped.NoPrelude ((+),(*),(-),(/))


main = do
  print $ 1 meter + 35 centi meter
  print $ 40 kilo meter / 1 hour + 10 hertz * 120 centi meter
