Import angles-related classes:

```tut:silent
import org.opentorah.calendar.angles.Angles
import Angles.{Position, Rotation}
```

Create some angles:

```tut:book
val fullCircle = Rotation(360)
val zero = Position(360)
```

Look at them - inside and out:

```tut:book
println(fullCircle.digits)
println(zero.digits)
```
