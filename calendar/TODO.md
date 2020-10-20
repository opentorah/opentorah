Maybe:
- add Numbers.NumberType?


The Small Switch:
- drop the S parameter
- keep the (F-bound) N type parameter for Number
- turn type parameter N into a type member for the companions (with refined types)
- Number: extends Ordered[N]
  type Point <: PointNumber[Point]
  val Point: PointCompanion { type N = Point }
  type Vector <: VectorNumber[Vector]
  val Vector: VectorCompanion { type N = Vector }

- NumberCompanion:
  private type NN = N
  type CompanionType = NumberCompanion { type N = NN }
  def companion: CompanionType


The Big Switch:
- drop the S parameter
- turn the N parameter into type member
- override val numbers: <more precise>
- override type N <: <more precise>

- NumberCompanion:
  type N <: numbers.NumberType

- Number:
  type CompanionType = NumberCompanion { type N = Number.this.N }
  def companion: CompanionType
  

- Numbers:
  type NumberType <: Number
  type Point <: NumberType with PointNumber { type N = Point }
  val Point: PointCompanion { type N = Point }
  type Vector <: NumberType with VectorNumber { type N = Vector }
  val Vector: VectorCompanion { type N = Vector }

- VectorNumber:
  // TODO why do I need the cast here and only here? How to get rid of it?
  final def canonical: numbers.Vector = numbers.Vector.canonical(digits).asInstanceOf[numbers.Vector] 

 
Angle:   type N <: Number with Angle
Angles: 
  override type Point <: MomentBase { type N = Point }
  override val Point: MomentCompanion { type N = Point }

Time:    type N <: Number with Time
Times:
  override type Point <: TimePointBase { type N = Point }
  override type Vector <: TimeVectorBase { type N = Vector }
  final lazy val week: Vector = Vector().days(7) - no prefix

MomentCompanion:
  type N <: numbers.Moment
MomentBase:
  type N <: numbers.Moment
  