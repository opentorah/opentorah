Import angles-related classes:

```scala mdoc:silent
import org.opentorah.angles.Angles
import Angles.{Position, Rotation}
```

Create some angles:

```scala mdoc
val fullCircle = Rotation(360)
val zero = Position(360)
```

Look at them - inside and out:

```scala mdoc
println(fullCircle.digits)
println(zero.digits)
```
