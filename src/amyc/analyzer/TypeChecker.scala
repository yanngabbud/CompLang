package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {

      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))

      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)

        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)

        case StringLiteral(_) =>
          topLevelConstraint(StringType)

        case UnitLiteral() =>
          topLevelConstraint(UnitType)

        case Variable(name) =>
          topLevelConstraint(env(name))

        case Plus(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Minus(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Times(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Div(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Mod(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case LessThan(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)

        case LessEquals(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)

        case And(lhs, rhs) =>
          genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)

        case Or(lhs, rhs) =>
          genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)

        case Equals(lhs, rhs) =>
          val newType = TypeVariable.fresh()
          genConstraints(lhs, newType) ++ genConstraints(rhs, newType) ++ topLevelConstraint(BooleanType)

        case Concat(lhs, rhs) =>
          genConstraints(lhs, StringType) ++ genConstraints(rhs, StringType) ++ topLevelConstraint(StringType)

        case Not(expr) =>
          genConstraints(expr, BooleanType) ++ topLevelConstraint(BooleanType)

        case Neg(expr) =>
          genConstraints(expr, IntType) ++ topLevelConstraint(IntType)

        case Call(qname, args) =>
          val argumentsType = args zip table.getFunction(qname).getOrElse(table.getConstructor(qname).get).argTypes
          val typesConstraints = argumentsType.flatMap(x => genConstraints(x._1, x._2))
          val topLevel = topLevelConstraint(table.getFunction(qname).getOrElse(table.getConstructor(qname).get).retType)
          typesConstraints ++ topLevel

        case Sequence(e1, e2) =>
          val newType = TypeVariable.fresh()
          genConstraints(e1, newType) ++ genConstraints(e2, expected) ++ topLevelConstraint(expected)

        case Let(df, value, body) =>
          genConstraints(value, df.tt.tpe) ++ genConstraints(body, expected)(env + (df.name -> df.tt.tpe))

        case Ite(cond, thenn, elze) =>
          genConstraints(cond, BooleanType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected)

        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type): (List[Constraint], Map[Identifier, Type]) = {
            pat match{
              case WildcardPattern() =>
                (List(), Map())

              case IdPattern(name) =>
                (List(), Map(name -> scrutExpected))

              case LiteralPattern(lit) =>
                (genConstraints(lit, scrutExpected), Map())

              case CaseClassPattern(constr, args) =>
                val construtor = table.getConstructor(constr).get
                val argsPairs = args zip construtor.argTypes
                val handled = argsPairs.map(x => handlePattern(x._1, x._2))
                (Constraint(construtor.retType, scrutExpected, pat.position) :: handled.flatMap(_._1), handled.flatMap(_._2).toMap)
            }
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, expected)(env ++ moreEnv)
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))

        case Error(msg) =>
          genConstraints(msg, StringType) ++ topLevelConstraint(expected)
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and
    //  call `typeError` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more => found match {

          case IntType => expected match {
            case IntType => solveConstraints(more)
            case TypeVariable(id) => solveConstraints(subst_*(more, id, IntType))
            case _ => error("Wrong type !", pos)
          }

          case BooleanType => expected match {
            case BooleanType => solveConstraints(more)
            case TypeVariable(id) => solveConstraints(subst_*(more, id, BooleanType))
            case _ => error("Wrong type !", pos)
          }

          case StringType => expected match {
            case StringType => solveConstraints(more)
            case TypeVariable(id) => solveConstraints(subst_*(more, id, StringType))
            case _ => error("Wrong type !", pos)
          }

          case UnitType => expected match {
            case UnitType => solveConstraints(more)
            case TypeVariable(id) => solveConstraints(subst_*(more, id, UnitType))
            case _ => error("Wrong type !", pos)
          }

          case ClassType(qnameFound) => expected match {
            case ClassType(qnameExpected) if qnameFound == qnameExpected => solveConstraints(more)
            case TypeVariable(id) => solveConstraints(subst_*(more, id, ClassType(qnameFound)))
            case _ => error("Wrong type !", pos)
          }

          case TypeVariable(idFound) => expected match {
            case TypeVariable(idExpected) if idFound == idExpected => solveConstraints(more)
            case _ => solveConstraints(subst_*(more, idFound, expected))
          }
        }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
