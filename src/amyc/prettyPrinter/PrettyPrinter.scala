package amyc
package prettyPrinter

import amyc.parsing.Token
import amyc.parsing.Tokens.COMMENTLIT
import amyc.utils._
import ast.NominalTreeModule._
import amyc.ast.{NominalTreeModule => N}

import scala.language.implicitConversions

object PrettyPrinter extends Pipeline[(N.Program, List[COMMENTLIT]), Unit] {
  def run(ctx: Context)(pair: (Program, List[COMMENTLIT])): Unit = {
    println(print(pair).print)
  }

  implicit def stringToDoc(s: String): Raw = Raw(s)

  /*
   * format and print the ast and the comments
   */
  def print(pair: (Program, List[COMMENTLIT])): Document = {
    var comments = (COMMENTLIT("", NoPosition) :: pair._2).reverse

    def binOp(e1: Expr, op: String, e2: Expr) = {
      createDocument(e1) <:> " " + op + " " <:> createDocument(e2)
    }

    /*
     * Helper method to print the comments at the end of the line
     */
    def insertEndOfLineComments(p: Position): String = {
      var comment = comments.head
      var result = ""
      while (comment.pos.line == p.line) {
        result = result + " " + comment.value
        comments = comments.tail
        comment = comments.head
      }
      result
    }

    /*
     * Method that create the document that will be printed
     */
    def createDocument(t: Tree): Document = {
      def rec(t: Tree): Document = {
        t match {
          case Program(modules) =>
            Stacked(modules map (createDocument(_)), emptyLines = true)

          case ModuleDef(name, defs, optExpr) =>
            val comments = insertEndOfLineComments(t.position)
            Stacked(
              "object " <:> name <:> " { " <:> comments,
              Indented(Stacked(defs ++ optExpr.toList map (createDocument(_)), emptyLines = true)),
              "}",
              ""
            )

          case AbstractClassDef(name) =>
            val comments = insertEndOfLineComments(t.position)
            "abstract class " <:> name <:> " " <:> comments

          case CaseClassDef(name, fields, parent) =>
            def printField(f: TypeTree) = "v: " <:> createDocument(f)

            val comments = insertEndOfLineComments(t.position)
            "case class " <:> name <:> "(" <:> Lined(fields map printField, ", ") <:> ") extends " <:>
              parent <:> " " <:> comments

          case FunDef(name, params, retType, body) =>
            val comments = insertEndOfLineComments(t.position)
            Stacked(
              "def " <:> name <:> "(" <:> Lined(params map (x => createDocument(x)), ", ") <:> "): " <:>
              createDocument(retType) <:> " = {" <:> comments,
              Indented(createDocument(body)),
              "}"
            )

          case ParamDef(name, tpe) =>
            name <:> ": " <:> createDocument(tpe)

          /* Expressions */
          case Variable(name) =>
            name <:> insertEndOfLineComments(t.position)
          case IntLiteral(value) =>
            value.toString <:> insertEndOfLineComments(t.position)
          case BooleanLiteral(value) =>
            value.toString <:> insertEndOfLineComments(t.position)
          case StringLiteral(value) =>
            '"' + value + '"' <:> insertEndOfLineComments(t.position)
          case UnitLiteral() =>
            "()" <:> insertEndOfLineComments(t.position)
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
            val comments = insertEndOfLineComments(t.position)
            name.name <:> "(" <:> Lined(args map (createDocument(_)), ", ") <:> ")" <:> comments
          case Sequence(lhs, rhs) =>
            val main = Stacked(
              createDocument(lhs) <:> ";", createDocument(rhs)
            )
            main
          case Let(df, value, body) =>
            val comments = insertEndOfLineComments(t.position)
            val main = Stacked(
              "val " <:> createDocument(df) <:> " = " <:> createDocument(value) <:> ";" <:> comments,
              createDocument(body)
            )
            main
          case Ite(cond, thenn, elze) =>
            val comments = insertEndOfLineComments(t.position)
            Stacked(
              "if (" <:> createDocument(cond) <:> ") {" <:> comments,
              Indented(createDocument(thenn)),
              "}",
              "else {" ,
              Indented(createDocument(elze)),
              "}"
            )
          case Match(scrut, cases) =>
            val comments = insertEndOfLineComments(t.position)
            Stacked(
              createDocument(scrut) <:> " match {" <:> comments,
              Indented(Stacked(cases map (createDocument(_)))),
              "}"
            )
          case Error(msg) =>
            val comments = insertEndOfLineComments(t.position)
            "error(" <:> createDocument(msg) <:> ")" <:> comments

          /* cases and patterns */
          case MatchCase(pat, expr) =>
            val comments = insertEndOfLineComments(t.position)
            Stacked(
              "case " <:> createDocument(pat) <:> " =>" <:> comments,
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

      if (comments.nonEmpty) {
        var current = comments.head
        if (current.pos.line < t.position.line && current.pos.file == t.position.file) {
          var result: Document = current.value
          var last = current
          comments = comments.tail
          current = comments.head
          while (current.pos.line  < t.position.line && current.pos.file == t.position.file) {
            if (last.pos.line == current.pos.line) result = Stacked(result <:> " " <:> current.value)
            else result = Stacked(result, current.value)
            last = current
            comments = comments.tail
            current = comments.head
          }
          Stacked(result, createDocument(t))
        }
        else {
          rec(t)
        }
      }
      else rec(t)
    }

    createDocument(pair._1)
  }
}
