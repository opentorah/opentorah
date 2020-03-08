Import angles-related classes:

```scala
import org.opentorah.calendar.angles.Angles
import Angles.{Position, Rotation}
```

Create some angles:

```scala
val fullCircle = Rotation(360)
// fullCircle: org.opentorah.calendar.angles.Angles#Vector = 360°

val zero = Position(360)
// zero: org.opentorah.calendar.angles.Angles#Point = 0°
```

Look at them - inside and out:

```scala
println(fullCircle.digits)
// List(360)

println(zero.digits)
// List(0)
```
