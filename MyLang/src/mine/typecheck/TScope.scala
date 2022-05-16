package mine
package typecheck
import Parser._
import collection.mutable
import collection.mutable.Buffer

class Env(optimise: Boolean, debug: Boolean) {
  def genTypeName() = {
    '$' +: util.Random.alphanumeric.take(4).mkString
  }

  class UnificationException(t1: Type, t2: Type) extends Exception {
    override def getMessage(): String = s"Attempt to unify $t1 (${t1.pos}) & $t2 (${t2.pos})"
  }

  sealed trait AssignedType {
    def registerTypes(): Unit = {}
    def name: String
    def repr: Type
    def unifyWith(to: ExprType): Unit
    def exprType: ExprType
    override def toString(): String = s"$name :: $repr"
  }
  object AssignedType {
    private def rt(t: FluxType): AssignedType = new RenamedType(t)
    def fun(scope: Scope, args: Seq[AssignedType], returns: AssignedType): Parameterised = {
      // What name to assign?
      val argsType = rt(new StructType(args.zipWithIndex.map(x => s"_${x._2}" -> x._1), genTypeName()))
      new Parameterised(genTypeName(), "Fun", Seq(argsType, returns))
    }
    def anyFloat: AssignedType = {
      rt(new StrongUnion(Seq(BaseType(F32), BaseType(F64)), genTypeName()))
    }
    def anyInt: AssignedType = {
      val ts = Seq(S8, S16, S32, S64, U8, U16, U32, U64)
      rt(new StrongUnion(ts.map(BaseType.apply), genTypeName()))
    }
    def anyString: AssignedType = {
      val cString = rt(new Parameterised(genTypeName(), "Ptr", Seq(BaseType(U8))))
      val aString = rt(new StructType(Seq("Len" -> AssignedType.anyInt, "Ptr" -> BaseType(U8)), genTypeName()))
      rt(new StrongUnion(Seq(cString, aString), genTypeName()))
    }
    def apply(scope: Scope): AssignedType = apply(scope, WildCard, "")
    def apply(scope: Scope, t: Type): AssignedType = apply(scope, t, "")
    def apply(scope: Scope, t: Type, namePrefix: String): AssignedType = t match {
      case EnforcedUnknown => DontCare
      case s: SuperposedType =>
        val name = namePrefix + genTypeName()
        rt(new WeakUnion(s.of.map(AssignedType.apply(scope, _, name)), name))
      case p: ParamType =>
        assert(p.isBaseType)
        val name = namePrefix + genTypeName()
        rt(new Parameterised(name, p.t, p.params.map(AssignedType.apply(scope, _, name))))
      case t: TupleType =>
        val name = namePrefix + genTypeName()
        rt(new StructType(t.ts.map {case (n, t) =>
          n -> AssignedType(scope, t, name)
        }, name))
      case u: UnionType =>
        val name = genTypeName()
        rt(new StrongUnion(u.of.map(AssignedType.apply(scope, _, namePrefix + name)), name))
      case m: Multiple => ???
      case WildCard => rt(new Unknown(namePrefix + genTypeName()))
      case n: NamedType =>
        // TODO: Handle shadowed types, etc. here (Maybe rename earlier based on scope)
        if (scope.names.contains(n.t)) scope.names(n.t).declaredType
        else rt(new Unknown(n.t))
      case _ => BaseType(t)
    }
  }
  sealed trait ExprType {
    def name: String
    def repr: Type
    def compatibleWith(that: ExprType): Boolean
    def merge(that: ExprType): ExprType
  }
  sealed abstract class FluxType(val name: String) extends ExprType {
    def repr: Type
  }
  case class BaseType(t: Type) extends AssignedType with ExprType {
    val name = t.toString()
    def repr: Type = t
    def compatibleWith(that: ExprType): Boolean =
      this.repr == that.repr || !that.isInstanceOf[BaseType] && (that compatibleWith this)
    def merge(to: ExprType): ExprType = {
      if (to.repr != this.repr) throw new UnificationException(to.repr, this.repr)
      this
    }
    def unifyWith(to: ExprType): Unit = {
      assert(this compatibleWith to)
    }
    def exprType: ExprType = this
  }
  class RenamedType(startT: FluxType) extends AssignedType {
    private var t: ExprType = startT
    override def unifyWith(to: ExprType): Unit = {
      if (!(t compatibleWith to)) throw new UnificationException(t.repr, to.repr)
      t = t merge to
    }
    override def exprType: ExprType = t
    def name: String = t.name
    def repr: Type = t.repr
  }
  case object DontCare extends AssignedType with ExprType {
    val name: String = "_"
    def repr: Type = EnforcedUnknown
    def unifyWith(to: ExprType): Unit = {
      // Nothing to be done here
    }
    def exprType: ExprType = this
    def compatibleWith(that: ExprType): Boolean = true    
    def merge(that: ExprType): ExprType = this
  }
  class Unknown(name: String = genTypeName()) extends FluxType(name) {
    def repr: Type = WildCard
    def compatibleWith(that: ExprType): Boolean = true // Probably?
    def merge(that: ExprType): ExprType = that
  }
  class WeakUnion(val of: Seq[AssignedType], name: String) extends FluxType(name) {
    def repr: Type = SuperposedType(of.map(_.repr))
    def compatibleWith(that: ExprType): Boolean = ???
    def merge(that: ExprType): ExprType = ???
  }
  class StrongUnion(val of: Seq[AssignedType], name: String) extends FluxType(name) {
    def repr: Type = UnionType(of.map(_.repr))
    def compatibleWith(that: ExprType): Boolean = {
      of.exists(_.exprType compatibleWith that)
    }
    def merge(that: ExprType): ExprType = that match {
      case that: BaseType => that // We assume that these are already compatible
      case that: StrongUnion =>
        new StrongUnion(that.of.intersect(this.of), this.name)
      case that: WeakUnion => ??? // This will be a pain lol
      case that: Unknown => this
      case _ =>
        throw new UnificationException(this.repr, that.repr)
    }
  }
  class Parameterised(name: String, val pName: String, val of: Seq[AssignedType]) extends FluxType(name) {
    def repr: Type = ParamType(pName, of.map(_.repr))
    def compatibleWith(that: ExprType): Boolean = that match {
      case that: Parameterised =>
        println(s"Check $pName/$of >:< ${that.pName}/${that.of}")
        def namesMatch = this.pName == that.pName
        def pLengthsMatch = this.of.length == that.of.length
        def psMatch = this.of.zip(that.of).forall {
          case (a,b) => a.exprType compatibleWith b.exprType
        }
        namesMatch && pLengthsMatch && psMatch
      case DontCare => true           // DontCare is a sink
      case that: Unknown => true      // Unknown is undefined
      case that: StrongUnion =>       // At least 1 is compatible
        that.of.exists(this compatibleWith _.exprType)
      case that: WeakUnion =>         // At least 1 is compatible
        that.of.exists(this compatibleWith _.exprType)
      case that: StructType => false  // Parameterised is not struct
      case that: BaseType => false    // Parameterised is not a base type
    }
    def merge(that: ExprType): ExprType = that match {
      case that: Parameterised => ??? // Assert eq
      case DontCare =>
        // I assume this would be assigning dontcare to a value, which is not possible?
        throw new UnificationException(this.repr, EnforcedUnknown)
      case that: Unknown => this
      case that: StrongUnion =>
        // Do we need to extract all possible cases from the strong union here?
        this
      case that: WeakUnion =>
        // Do we need to extract all possible cases from the weak union here?
        this
      case that: StructType =>
        throw new UnificationException(this.repr, that.repr)
      case that: BaseType =>
        throw new UnificationException(this.repr, that.repr)
    }
  }
  class StructType(val members: Seq[(String, AssignedType)], name: String) extends FluxType(name) {
    def repr: Type = TupleType(members.map(x => x._1 -> x._2.repr))
    def compatibleWith(that: ExprType): Boolean = that match {
      case that: StructType =>
        for ((name, member) <- this.members) {
          // There can be members not declared, but still shared.
          // We only care about those common to both.
          that.members.find(_._1 == name).map {case (tName, tMember) =>
            if (!(member.exprType compatibleWith tMember.exprType))
              return false  
          }
        }
        true
      // Others???
      case that => ???
    }
    def merge(that: ExprType): ExprType = ???
  }

  sealed trait ValueSource {
    val declaredType: AssignedType
    protected val expectedTypes = Buffer[AssignedType]()
    def expected: Seq[AssignedType] = expectedTypes.toSeq
    def expect(t: AssignedType): Unit = {
      expectedTypes += t
    }
    def expect(ts: Seq[AssignedType]): Unit = {
      for (t <- ts) expect(t)
    }
    def checkTypes(): Unit
  }

  object TypedExpression {
    def apply(e: Expression, scope: Scope): TypedExpression = e match {
      case a:ExtFun => TypedExtFun(a, scope)
      case a:MemAccess => TypedMemAccess(a, scope)
      case a:ByName => TypedByName(a, scope)
      case a:FloatLiteral => TypedFloatLiteral(a, scope)
      case a:IntLiteral => TypedIntLiteral(a, scope)
      case a:StringLiteral => TypedStringLiteral(a, scope)
      case a:AnonFun => TypedAnonFun(a, scope)
      case a:Assignment => TypedAssignment(a, scope)
      case a:Ident => TypedIdent(a, scope)
      case a:FunCall => TypedFunCall(a, scope)
      case a:Block => TypedBlock(a, scope)
      case a:StructLiteral => TypedStructLiteral(a, scope)
    }
  }
  case class FunParameter(scope: Scope, name: String, t: Type) extends ValueSource {
    lazy val declaredType = AssignedType(scope, t, name)
    def checkTypes(): Unit = {
      // Nothing needed here?
    }
  }
  sealed abstract class TypedExpression(_base: Expression, scope: Scope) extends ValueSource {
    val base: Expression
    def registerTypes(): Unit = {
      scope.types += declaredType.name -> declaredType
    }
  }
  case class TypedAnonFun(base: AnonFun, scope: Scope) extends TypedExpression(base, scope) {
    lazy val declaredType: AssignedType = AssignedType(scope)
    lazy val caseScopes =
      for (c <- base.cases) yield {
        new FunScope(c.expr, c.params, scope)
      }
    def checkTypes(): Unit = {
      caseScopes.foreach(_.checkTypes())
      val caseTypes = for (c <- caseScopes) yield {
        new RenamedType(c.funType)
      }
      declaredType unifyWith new WeakUnion(caseTypes, declaredType.name)
    }
  }
  case class TypedByName(base: ByName, scope: Scope) extends TypedExpression(base, scope) {
    lazy val declaredType: AssignedType = {
      // TODO: Look up type of base
      ???
    }
    def checkTypes(): Unit = ???
  }
  case class TypedFloatLiteral(base: FloatLiteral, scope: Scope) extends TypedExpression(base, scope) {
    lazy val declaredType: AssignedType = AssignedType.anyFloat
    def checkTypes(): Unit = {
      // Nothing to do here?
      // Maybe seeing expected types
      // but that may come later?
    }
  }
  case class TypedIntLiteral(base: IntLiteral, scope: Scope) extends TypedExpression(base, scope) {
    lazy val declaredType: AssignedType = AssignedType.anyInt
    def checkTypes(): Unit = {
      // Nothing to do here?
      // Maybe seeing expected types
      // but that may come later?
    }
  }
  case class TypedStringLiteral(base: StringLiteral, scope: Scope) extends TypedExpression(base, scope) {
    lazy val declaredType: AssignedType = AssignedType.anyString
    def checkTypes(): Unit = {
      // Nothing to do here?
      // Maybe seeing expected types
      // but that may come later?
    }
  }
  case class TypedExtFun(base: ExtFun, scope: Scope) extends TypedExpression(base, scope) {
    lazy val declaredType = AssignedType(scope, base.t, base.name)
    override def registerTypes(): Unit = {
      super.registerTypes()
      scope.names += base.name -> this
    }
    def checkTypes(): Unit = {
      // Nothing needed here?
    }
  }
  case class TypedIdent(base: Ident, scope: Scope) extends TypedExpression(base, scope) {
    // Lazy because scope names may not be added yet?
    lazy val declaredType: AssignedType = scope.names(base.name).declaredType
    override def expect(t: AssignedType): Unit = {
      super.expect(t)
      scope.names(base.name).expect(expected)
    }
    def checkTypes(): Unit = {
      // TODO: Somehow merge expected and declared types here?
    }
  }
  case class TypedStructLiteral(base: StructLiteral, scope: Scope) extends TypedExpression(base, scope) {
    lazy val declaredType: AssignedType = ???
    def checkTypes(): Unit = ???
  }
  case class TypedMemAccess(base: MemAccess, scope: Scope) extends TypedExpression(base, scope) {
    lazy val declaredType: AssignedType = ???
    def checkTypes(): Unit = ???
  }
  case class TypedAssignment(base: Assignment, scope: Scope) extends TypedExpression(base, scope) {
    lazy val declaredType = AssignedType(scope, base.t, base.name)
    lazy val subScope = new OneScope(base.expr, scope)
    override def registerTypes(): Unit = {
      super.registerTypes()
      scope.names += base.name -> this
      println(s"Added ${base.name} to scope $scope")
      println(scope.names.keySet)
    }
    def checkTypes(): Unit = {
      subScope.expr.expect(declaredType)
      subScope.checkTypes()
      declaredType unifyWith subScope.expr.declaredType.exprType
    }
  }
  case class TypedFunCall(base: FunCall, scope: Scope) extends TypedExpression(base, scope) {
    lazy val declaredType: AssignedType = AssignedType(scope)
    lazy val funScope = new OneScope(base.fun, scope)
    lazy val paramScopes = for (p <- base.params) yield new OneScope(p, scope)
    def checkTypes(): Unit = {
      funScope.checkTypes()
      paramScopes.foreach(_.checkTypes())
      // At a minimum, we expect the arity & function type to be present
      val baseFun = AssignedType.fun(scope, paramScopes.map(_.expr.declaredType), declaredType)
      funScope.expr.expect(new RenamedType(baseFun))
      funScope.expr.declaredType match {
        case r: RenamedType => 
          r.exprType match {
            case p: Parameterised =>
              assert(p.pName == "Fun")
              val params = p.of(0).exprType.asInstanceOf[StructType].members.map(_._2)
              println(s"Params are: $params")
              val rt = p.of(1)
              println(s"Rt is $rt")
              declaredType unifyWith rt.exprType
              // Can we do the other way around?
              for ((p, ps) <- params.zip(paramScopes)) {
                ps.expr.declaredType unifyWith p.exprType
              }
            case u: WeakUnion => 
              println(s"For ${base.fun}, exp type is a union: ${u.repr}")
            case u: StrongUnion =>
              println(s"For ${base.fun}, exp type is a union: ${u.repr}")
            case _ =>
              // Should we check for non-error/error here?
          }
        case _ =>
          // error? It is a basetype
      }
    }
  }
  case class TypedBlock(base: Block, scope: Scope) extends TypedExpression(base, scope) {
    lazy val declaredType: AssignedType = AssignedType(scope, WildCard)
    lazy val subScope = new BlockScope(base.exprs, scope)
    def checkTypes(): Unit = {
      subScope.checkTypes()
      declaredType unifyWith subScope.exprs.last.declaredType.exprType
    }
  }

  sealed trait Scope {
    val names = mutable.Map[String, ValueSource]()
    val types = mutable.Map[String, AssignedType]()
    def checkTypes(): Unit
  }
  trait SubScope extends Scope {
    val parent: Scope
    for (n <- parent.names) {
      names += n
    }
    for (t <- parent.types) {
      types += t
    }
  }
  class OneScope(input: Expression, val parent: Scope) extends SubScope {
    def repr: Expression = ???
    lazy val expr = TypedExpression(input, this)
    def checkTypes() = {
      expr.registerTypes()
      expr.checkTypes()
    }
  }
  class FunScope(input: Expression, val params: Seq[Param], val parent: Scope) extends SubScope {
    lazy val expr = TypedExpression(input, this)
    val args: Seq[AssignedType] = params.collect {
      case f: FloatLiteral =>
        AssignedType.anyFloat
      case i: IntLiteral =>
        AssignedType.anyInt
      case s: StringLiteral =>
        AssignedType.anyString
      case WildCard =>
        AssignedType(this) // Not like it affects anything anyway...
      case EnforcedUnknown =>
        DontCare
      case ByName(name) =>
        names(name).declaredType      
      case TypeParam(name, t) =>
        val fp = FunParameter(this, name, t)
        names += name -> fp
        fp.declaredType
    }
    lazy val returnType = AssignedType(this)
    def funType = {
      AssignedType.fun(this, args, returnType)
    }
    // TODO: Something to collect & reduce funcall types
    def checkTypes(): Unit = {
      // Need to compute args before expected type of expr
      // DeclaredType => 
      expr.declaredType unifyWith returnType.exprType
      // TODO: Add params
      println(s"Register type for ${Format(expr.base)}")
      expr.registerTypes()
      println(s"Registered ${Format(expr.base)} to ${Format(expr.declaredType.repr)}")
      expr.checkTypes()
      returnType unifyWith expr.declaredType.exprType
      println(s"Checked ${Format(expr.base)} to ${Format(expr.declaredType.repr)}\n")
    }
  }
  sealed abstract class MultiScope(input: Seq[Expression]) extends Scope {
    lazy val exprs = input.map(TypedExpression.apply(_, this))

    def checkTypes() = {
      for (expr <- exprs) {
        expr.registerTypes()
        println(s"Registered ${Format(expr.base)} to ${Format(expr.declaredType.repr)} in scope $this")
      }
      for (expr <- exprs) {
        println(s"Check type for ${Format(expr.base)} in scope $this")
        expr.checkTypes()
        println(s"Checked ${Format(expr.base)} to ${Format(expr.declaredType.repr)}\n")
      }
      for (expr <- exprs) {
        println(s"After all checks for ${Format(expr.base)}")
        println(s"Expected: ${expr.expected.map(x => s"${x.name} :: ${Format(x.repr)}").mkString("(", ", ", ")")}")
        println(s"Declared: ${expr.declaredType.name} :: ${Format(expr.declaredType.repr)}")
        println()
      }
    }
  }
  class BlockScope(input: Seq[Expression], val parent: Scope) extends MultiScope(input) with SubScope {

  }
  class RootScope(input: Seq[Expression]) extends MultiScope(input) {
    val output = exprs
  }
}