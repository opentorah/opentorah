Import angles-related classes:

```scala
import org.podval.calendar.angles.Angles
import Angles.{Position, Rotation}
```

Create some angles:

```scala
val fullCircle = Rotation(360)
// fullCircle: org.podval.calendar.angles.Angles#Vector = 360°

val zero = Position(360)
// zero: org.podval.calendar.angles.Angles.Position = 0°
```

Look at them - inside and out:

```scala
println(fullCircle.digits)
// List(360)

println(zero.digits)
// List(0)
```
