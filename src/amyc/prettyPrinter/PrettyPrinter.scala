package amyc
package prettyPrinter

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
    var comments = (COMMENTLIT("").setPos(NoPosition) :: pair._2).reverse

    def binOp(e1: Expr, op: String, e2: Expr) = {
      createDocument(e1) <:> " " + op + " " <:> createDocument(e2)
    }

    /*
     * Helper method to print the comments at the end of the line
     */
    def insertEndOfLineComments(p: Position): Document = {
      def rec(p: Position, d: Document): Document = {
        val comment = comments.head
        if (comment.position.line == p.line && comment.position.file == p.file) {
          comments = comments.tail
          rec(p, Stacked(d <:> " " <:> comment.value))
        }
        else d
      }
      val comment = comments.head
      if (comment.position.line == p.line && comment.position.file == p.file) {
        comments = comments.tail
        rec(p, Stacked(comment.value))
      } else {
        Stacked()
      }
    }

    /*
     * Helper method to print the comments next to "else {"
     */
    def insertElzeComments(p: Position): Document = {
      def rec(p: Position, d: Document): Document = {
        val comment = comments.head
        if (comment.position.line < p.line && comment.position.file == p.file) {
          comments = comments.tail
          rec(p, Stacked(d, comment.value))
        }
        else d
      }
      val comment = comments.head
      if (comment.position.line < p.line && comment.position.file == p.file) {
        comments = comments.tail
        rec(p, Stacked(comment.value))
      } else {
        Stacked()
      }
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
              createDocument(retType) <:> " = { " <:> comments,
              Indented(createDocument(body)),
              "}"
            )

          case ParamDef(name, tpe) =>
            name <:> ": " <:> createDocument(tpe)

          /* Expressions */
          case Variable(name) =>
            val comment = insertEndOfLineComments(t.position)
            if (comment != Stacked()){
              name <:> " " <:> comment
            } else {
              name
            }
          case IntLiteral(value) =>
            val comment = insertEndOfLineComments(t.position)
            if (comment != Stacked()){
              value.toString <:> " " <:> comment
            } else {
              value.toString
            }
          case BooleanLiteral(value) =>
            val comment = insertEndOfLineComments(t.position)
            if (comment != Stacked()){
              value.toString <:> " " <:> comment
            } else {
              value.toString
            }
          case StringLiteral(value) =>
            val comment = insertEndOfLineComments(t.position)
            if (comment != Stacked()){
              '"' + value + '"' <:> " " <:> comment
            } else {
              '"' + value + '"'
            }
          case UnitLiteral() =>
            val comment = insertEndOfLineComments(t.position)
            if (comment != Stacked()){
              "()" <:> " " <:> comment
            } else {
              "()"
            }
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
            name.name <:> "(" <:> Lined(args map (createDocument(_)), ", ") <:> ") " <:> comments
          case Sequence(lhs, rhs) =>
            val main = Stacked(
              createDocument(lhs) <:> ";", createDocument(rhs)
            )
            main
          case Let(df, value, body) =>
            val comments = insertEndOfLineComments(t.position)
            val main = Stacked(
              "val " <:> createDocument(df) <:> " = " <:> createDocument(value) <:> "; " <:> comments,
              createDocument(body)
            )
            main
          case Ite(cond, thenn, elze) =>
            val condComments = insertEndOfLineComments(t.position)
            val condition = createDocument(cond)
            val thennn = createDocument(thenn)
            val elseComments = insertElzeComments(elze.position)
            if (condComments != Stacked() && elseComments != Stacked()){
              Stacked(condComments, "if (" <:> condition <:> ") {", Indented(thennn), "}",
                elseComments, "else {" , Indented(createDocument(elze)), "}")
            }
            else if (condComments != Stacked()){
              Stacked(condComments, "if (" <:> condition <:> ") {", Indented(thennn), "}",
                "else {" , Indented(createDocument(elze)), "}")
            }
            else if (elseComments != Stacked()){
              Stacked("if (" <:> condition <:> ") {", Indented(thennn), "}",
                elseComments, "else {" , Indented(createDocument(elze)), "}")
            }
            else{
              Stacked("if (" <:> condition <:> ") {", Indented(thennn), "}",
                "else {" , Indented(createDocument(elze)), "}")
            }


          case Match(scrut, cases) =>
            val comments = insertEndOfLineComments(t.position)
            Stacked(
              createDocument(scrut) <:> " match { " <:> comments,
              Indented(Stacked(cases map (createDocument(_)))),
              "}"
            )
          case Error(msg) =>
            val comments = insertEndOfLineComments(t.position)
            "error(" <:> createDocument(msg) <:> ") " <:> comments

          /* cases and patterns */
          case MatchCase(pat, expr) =>
            val comments = insertEndOfLineComments(t.position)
            Stacked(
              "case " <:> createDocument(pat) <:> " => " <:> comments,
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

      /*
       * Print above expression comment if any
       */
      if (comments.nonEmpty) {
        def recu(t: Tree, l: COMMENTLIT, d: Document): Document = {
          val comment = comments.head
          if (comment.position.line < t.position.line && comment.position.file == t.position.file) {
            comments = comments.tail
            if (l.position.line == comment.position.line) recu(t, comment, Stacked(d <:> " " <:> comment.value))
            else recu(t, comment, Stacked(d, comment.value))
          }
          else Stacked(d, createDocument(t))
        }
        val comment = comments.head
        if (comment.position.line < t.position.line && comment.position.file == t.position.file) {
          comments = comments.tail
          recu(t, comment, Stacked(comment.value))
        }
        else rec(t)
      }
      else rec(t)
    }

    createDocument(pair._1)
  }
}
