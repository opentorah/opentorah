Import angles-related classes:

```scala
import org.opentorah.astronomy.Angles
import Angles.{Position, Rotation}
```

Create some angles:

```scala
val fullCircle = Rotation(360)
// fullCircle: RotationAngle = 360°
val zero = Position(360)
// zero: PositionAngle = 0°
```

Look at them - inside and out:

```scala
println(fullCircle.digits)
// List(360)
println(zero.digits)
// List(0)
```
