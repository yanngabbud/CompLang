package amyc
package prettyPrinter

import amyc.utils._
import ast.NominalTreeModule._
import scala.language.implicitConversions

object PrettyPrinter extends Pipeline[Program, Unit] {
  override def run(ctx: Context)(ast: Program): Unit = {
    println(createDocument(ast).print)
  }

  def binOp(e1: Expr, op: String, e2: Expr) = createDocument(e1) <:> " " + op + " " <:> createDocument(e2)

  implicit def stringToDoc(s: String): Raw = Raw(s)

  def createDocument(t: Tree): Document = t match {
    case Program(modules) =>
      Stacked(modules map (createDocument(_)), emptyLines = true)

    case ModuleDef(name, defs, optExpr) =>
      Stacked(
        "object " <:> name <:> " {",
        Indented(Stacked(defs ++ optExpr.toList map (createDocument(_)), emptyLines = true)),
        "}",
        ""
      )

    case AbstractClassDef(name) =>
      "abstract class " <:> name

    case CaseClassDef(name, fields, parent) =>
      def printField(f: TypeTree) = "v: " <:> createDocument(f)
      "case class " <:> name <:> "(" <:> Lined(fields map printField, ", ") <:> ") extends " <:> parent

    case FunDef(name, params, retType, body) =>
      Stacked(
        "def " <:> name <:> "(" <:> Lined(params map (createDocument(_)), ", ") <:> "): " <:> createDocument(retType) <:> " = {",
        Indented(createDocument(body)),
        "}"
      )

    case ParamDef(name, tpe) =>
      name <:> ": " <:> createDocument(tpe)

    /* Expressions */
    case Variable(name) =>
      name
    case IntLiteral(value) =>
      value.toString
    case BooleanLiteral(value) =>
      value.toString
    case StringLiteral(value) =>
      '"' + value + '"'
    case UnitLiteral() =>
      "()"
    case Plus(lhs, rhs) =>
      binOp(lhs, "+", rhs)
    case Minus(lhs, rhs) =>
      binOp(lhs, "-", rhs)
    case Times(lhs, rhs) =>
      binOp(lhs, "*", rhs)
    case Div(lhs, rhs) =>
      binOp(lhs, "/", rhs)
    case Mod(lhs, rhs) =>
      binOp(lhs, "%", rhs)
    case LessThan(lhs, rhs) =>
      binOp(lhs, "<", rhs)
    case LessEquals(lhs, rhs) =>
      binOp(lhs, "<=", rhs)
    case And(lhs, rhs) =>
      binOp(lhs, "&&", rhs)
    case Or(lhs, rhs) =>
      binOp(lhs, "||", rhs)
    case Equals(lhs, rhs) =>
      binOp(lhs, "==", rhs)
    case Concat(lhs, rhs) =>
      binOp(lhs, "++", rhs)
    case Not(e) =>
      "!(" <:> createDocument(e) <:> ")"
    case Neg(e) =>
      "-(" <:> createDocument(e) <:> ")"
    case Call(name, args) =>
      name.name <:> "(" <:> Lined(args map (createDocument(_)), ", ") <:> ")"
    case Sequence(lhs, rhs) =>
      val main = Stacked(
        createDocument(lhs) <:> ";",
        createDocument(rhs),
      )
      main
    case Let(df, value, body) =>
      val main = Stacked(
        "val " <:> createDocument(df) <:> " =",
        Indented(createDocument(value)) <:> ";",
        createDocument(body)
      )
      main
    case Ite(cond, thenn, elze) =>
      Stacked(
        "if (" <:> createDocument(cond) <:> ") {",
        Indented(createDocument(thenn)),
        "}",
        "else {",
        Indented(createDocument(elze)),
        "}"
      )
    case Match(scrut, cases) =>
      Stacked(
        createDocument(scrut) <:> " match {",
        Indented(Stacked(cases map (createDocument(_)))),
        "}"
      )
    case Error(msg) =>
      "error(" <:> createDocument(msg) <:> ")"

    /* cases and patterns */
    case MatchCase(pat, expr) =>
      Stacked(
        "case " <:> createDocument(pat) <:> " =>",
        Indented(createDocument(expr))
      )
    case WildcardPattern() =>
      "_"
    case IdPattern(name) =>
      name
    case LiteralPattern(lit) =>
      createDocument(lit)
    case CaseClassPattern(name, args) =>
      name.name <:> "(" <:> Lined(args map (createDocument(_)), ", ") <:> ")"

    /* Types */
    case TypeTree(tp) =>
      tp match {
        case IntType => "Int"
        case BooleanType => "Boolean"
        case StringType => "String"
        case UnitType => "Unit"
        case ClassType(name) => name.name
      }

  }
}
