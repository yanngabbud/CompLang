package amyc
package codegen

import java.util.Optional

import analyzer._
import ast.Identifier
import ast.SymbolicTreeModule.{And => AmyAnd, Call => AmyCall, Div => AmyDiv, Or => AmyOr, _}
import utils.{Context, Pipeline}
import wasm.{Instructions, _}
import Instructions._
import Utils._

import scala.util.parsing.combinator.token.StdTokens

// Generates WebAssembly code for an Amy program
object CodeGen extends Pipeline[(Program, SymbolTable), Module] {
  def run(ctx: Context)(v: (Program, SymbolTable)): Module = {
    val (program, table) = v

    // Generate code for an Amy module
    def cgModule(moduleDef: ModuleDef): List[Function] = {
      val ModuleDef(name, defs, optExpr) = moduleDef
      // Generate code for all functions
      defs.collect { case fd: FunDef if !builtInFunctions(fullName(name, fd.name)) =>
        cgFunction(fd, name, false)
      } ++
      // Generate code for the "main" function, which contains the module expression
      optExpr.toList.map { expr =>
        val mainFd = FunDef(Identifier.fresh("main"), Nil, TypeTree(IntType), expr)
        cgFunction(mainFd, name, true)
      }
    }

    // Generate code for a function in module 'owner'
    def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean): Function = {
      // Note: We create the wasm function name from a combination of
      // module and function name, since we put everything in the same wasm module.
      val name = fullName(owner, fd.name)
      Function(name, fd.params.size, isMain){ lh =>
        val locals = fd.paramNames.zipWithIndex.toMap
        val body = cgExpr(fd.body)(locals, lh)
        if (isMain) {
          body <:> Drop // Main functions do not return a value,
                        // so we need to drop the value generated by their body
        } else {
          body
        }
      }
    }

    // Generate code for an expression expr.
    // Additional arguments are a mapping from identifiers (parameters and variables) to
    // their index in the wasm local variables, and a LocalsHandler which will generate
    // fresh local slots as required.
    def cgExpr(expr: Expr)(implicit locals: Map[Identifier, Int], lh: LocalsHandler): Code = {
      expr match {
        case IntLiteral(x) =>
          Const(x)

        case BooleanLiteral(x) =>
          if (x) Const(1)
          else Const(0)

        case StringLiteral(x) =>
          mkString(x)

        case UnitLiteral() =>
          Const(0)

        case Variable(name) =>
          GetLocal(locals(name))

        case Plus(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Add

        case Minus(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Sub

        case Times(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Mul

        case AmyDiv(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Div

        case Mod(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Rem

        case LessThan(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Lt_s

        case LessEquals(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Le_s

        case AmyAnd(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> And

        case AmyOr(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Or

        case Equals(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Eq

        case Concat(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Call("String_concat")

        case Not(expr) =>
          cgExpr(expr) <:> Eqz

        case Neg(expr) =>
          Const(0) <:> cgExpr(expr) <:> Sub

        case AmyCall(qname, args) =>
          if (table.getFunction(qname).isDefined) {
            args.map(cgExpr(_)) <:> Call(fullName(table.getFunction(qname).get.owner, qname))
          }
          else if (table.getConstructor(qname).isDefined) {
            val newLocal = lh.getFreshLocal()
            val constrSig = table.getConstructor(qname).get
            GetGlobal(memoryBoundary) <:> SetLocal(newLocal) <:> GetGlobal(memoryBoundary) <:>
              adtField(constrSig.index) <:> SetGlobal(memoryBoundary) <:> GetGlobal(memoryBoundary) <:>
              Const(constrSig.index) <:> Store <:> cs2c(args.zipWithIndex.map(a => GetLocal(newLocal) <:>
              adtField(a._2) <:> cgExpr(a._1) <:> Store)) <:> GetLocal(newLocal)
          }
          else {
            Unreachable
          }

        case Sequence(e1, e2) =>
          cgExpr(e1) <:> Drop <:> cgExpr(e2)

        case Let(df, value, body) =>
          val newLocal = lh.getFreshLocal()
          cgExpr(value) <:> SetLocal(newLocal) <:> cgExpr(body)(locals + (df.name -> newLocal), lh)

        case Ite(cond, thenn, elze) =>
          cgExpr(cond) <:> If_i32 <:> cgExpr(thenn) <:> Else <:> cgExpr(elze) <:> End

        case Match(scrut, cases) =>
          val newLocal = lh.getFreshLocal()
          val patternAndExpr = cases.map(x => (x.pat, x.expr))

          def solve(code: Code, rest: List[(Pattern, Expr)]): Code = {
            rest match {
              case List() => code
              case List((pat, expr)) =>
                (pat, expr) match {
                  case (WildcardPattern(), body) =>
                    code <:> Const(1) <:> If_i32 <:> cgExpr(body) <:> Else <:>
                      mkString("Match error ! ") <:> Call("Std_printString") <:> Unreachable <:> End

                  case (IdPattern(name), body) =>
                    val newLocal0 = lh.getFreshLocal()
                    code <:> GetLocal(newLocal) <:> SetLocal(newLocal0) <:> Const(1) <:> If_i32 <:>
                      cgExpr(body)(locals + (name -> newLocal0), lh) <:> Else <:>
                      mkString("Match error ! ") <:> Call("Std_printString") <:> Unreachable <:> End

                  case (LiteralPattern(lit), body) =>
                    code <:> GetLocal(newLocal) <:> cgExpr(lit) <:> Eq <:> If_i32 <:> cgExpr(body) <:> Else <:>
                      mkString("Match error ! ") <:> Call("Std_printString") <:> Unreachable <:> End

//                  case (CaseClassPattern(constr, args), body) =>

                }
              case (pat, expr) :: next =>
                (pat, expr) match {
                  case (WildcardPattern(), body) =>
                     solve(code <:> Const(1) <:> If_i32 <:> cgExpr(body) <:> Else, next) <:> End

                  case (IdPattern(name), body) =>
                    val newLocal0 = lh.getFreshLocal()
                    solve(code <:> GetLocal(newLocal) <:> SetLocal(newLocal0) <:> Const(1) <:> If_i32 <:>
                      cgExpr(body)(locals + (name -> newLocal0), lh) <:> Else, next) <:> End

                  case (LiteralPattern(lit), body) =>
                    solve(code <:> GetLocal(newLocal) <:> cgExpr(lit) <:> Eq <:> If_i32 <:> cgExpr(body) <:> Else, next) <:> End

//                  case (CaseClassPattern(constr, args), body) =>

                }
            }
          }

          solve(cgExpr(scrut) <:> SetLocal(newLocal), patternAndExpr)

        case Error(msg) =>
          mkString("Error: ") <:> cgExpr(msg) <:> Call("String_concat") <:> Call("Std_printString") <:> Unreachable

      }
    }

    Module(
      program.modules.last.name.name,
      defaultImports,
      globalsNo,
      wasmFunctions ++ (program.modules flatMap cgModule)
    )

  }
}
