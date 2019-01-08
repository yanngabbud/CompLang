package amyc
package analyzer

import amyc.parsing.Tokens.COMMENTLIT
import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

import scala.util.parsing.combinator.token.StdTokens

// Name analyzer for Amy
// Takes a nominal program (names are plain strings, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns the symbol table.
object NameAnalyzer extends Pipeline[(N.Program, List[Object]), (S.Program, SymbolTable)] {
  def run(ctx: Context)(input: (N.Program, List[Object])): (S.Program, SymbolTable) = {
    import ctx.reporter._
    val p = input._1

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Step 1: Add modules to table 
    val modNames = p.modules.groupBy(_.name)
    modNames.foreach { case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }

    modNames.keys.toList foreach table.addModule


    // Helper method: will transform a nominal type 'tt' to a symbolic type,
    // given that we are within module 'inModule'.
    def transformType(tt: N.TypeTree, inModule: String): S.Type = {
      tt.tpe match {
        case N.IntType => S.IntType
        case N.BooleanType => S.BooleanType
        case N.StringType => S.StringType
        case N.UnitType => S.UnitType
        case N.ClassType(qn@N.QualifiedName(module, name)) =>
          table.getType(module getOrElse inModule, name) match {
            case Some(symbol) =>
              S.ClassType(symbol)
            case None =>
              fatal(s"Could not find type $qn", tt)
          }
      }
    }

    // Step 2: Check name uniqueness of definitions in each module
    p.modules.foreach { module =>
      val defName = module.defs.groupBy(_.name)
      defName.foreach {
        case (name, defs) =>
          if (defs.size > 1) {
            fatal(s"Two definitions named $name in program", defs.head.position)
          }
      }
    }

    val moduleList = p.modules.map{
      case N.ModuleDef(name, defs, _) => (name, defs)
    }
    moduleList foreach{
      case (name, defs) => defs.foreach {
        case N.AbstractClassDef(className) => table.addType(name, className)
        case _ =>
      }
    }
    moduleList foreach{
      case (name, defs) => defs.foreach{
        case N.CaseClassDef(className, fields, parent) =>
          val identifier = table.getType(name, parent)
          if(identifier.nonEmpty) table.addConstructor(name, className, fields.map(transformType(_, name)), identifier.get)
          else fatal("Unknown class")
        case _ =>
      }
    }
    moduleList foreach{
      case (name, defs) => defs.foreach {
        case N.FunDef(className, params, retType, body) =>
          table.addFunction(name, className, params.map(t => transformType(t.tt, name)), transformType(retType, name))
        case _ =>
      }
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions

    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // Keep in mind that we transform constructs of the NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = { df match {
      case N.AbstractClassDef(name) =>
        val Some(typpe) = table.getType(module, df.name)
        S.AbstractClassDef(typpe)
      case N.CaseClassDef(name, _, _) =>
        val Some((sym, sig)) = table.getConstructor(module, df.name)
        val argsType = sig.argTypes.map(x => S.TypeTree(x))
        S.CaseClassDef(sym, argsType, sig.parent)
      case fd: N.FunDef =>
        transformFunDef(fd, module)
    }}.setPos(df)

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body) = fd
      val Some((sym, sig)) = table.getFunction(module, name)

      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map()))
      ).setPos(fd)
    }

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
      val (params, locals) = names
      val res = expr match {
        case N.Match(scrut, cases) =>
          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            pat match {
              case N.WildcardPattern() => (S.WildcardPattern(), List())
              case N.IdPattern(identifier) =>
                if (locals.contains(identifier)) fatal(s"Unkown name")
                val name = Identifier.fresh(identifier)
                (S.IdPattern(name), List((identifier, name)))
              case N.LiteralPattern(lit) =>
                val literal = transformExpr(lit).asInstanceOf[S.Literal[_]]
                (S.LiteralPattern(literal), Nil)
              case N.CaseClassPattern(constr, args) =>
                val constructor: Option[(Identifier, ConstrSig)] = table.getConstructor(module, constr.name)
                if (constructor.isEmpty) fatal("Unkown constructor")
                if (constructor.get._2.argTypes.size != args.size) fatal("Incorrect number of arguments")
                args.filter(x => x.isInstanceOf[N.IdPattern]).groupBy{ case N.IdPattern(name) => name}
                  .foreach{ case (name, list) => if(list.size > 1) fatal(s"Duplicate match $name in $module")}
                val newArgs = args.map(transformPattern)
                (S.CaseClassPattern(constructor.get._1, newArgs.map(_._1)), newArgs.flatMap(_._2))
            }
          }

          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            val expr = transformExpr(rhs)(module, (params, locals ++ moreLocals))
            S.MatchCase(newPat, expr)
          }

          S.Match(transformExpr(scrut), cases.map(transformCase))

        case N.Variable(name) =>
          S.Variable(locals.getOrElse(name, params.getOrElse(name, fatal("Unknown variable"))))

        case N.IntLiteral(integer) =>
          S.IntLiteral(integer)

        case N.BooleanLiteral(boolean) =>
          S.BooleanLiteral(boolean)

        case N.StringLiteral(string) =>
          S.StringLiteral(string)

        case N.UnitLiteral() =>
          S.UnitLiteral()

        case N.Plus(lhs, rhs) =>
          S.Plus(transformExpr(lhs), transformExpr(rhs))

        case N.Minus(lhs, rhs) =>
          S.Minus(transformExpr(lhs), transformExpr(rhs))

        case N.Times(lhs, rhs) =>
          S.Times(transformExpr(lhs), transformExpr(rhs))

        case N.Div(lhs, rhs) =>
          S.Div(transformExpr(lhs), transformExpr(rhs))

        case N.Mod(lhs, rhs) =>
          S.Mod(transformExpr(lhs), transformExpr(rhs))

        case N.LessThan(lhs, rhs) =>
          S.LessThan(transformExpr(lhs), transformExpr(rhs))

        case N.LessEquals(lhs, rhs) =>
          S.LessEquals(transformExpr(lhs), transformExpr(rhs))

        case N.And(lhs, rhs) =>
          S.And(transformExpr(lhs), transformExpr(rhs))

        case N.Or(lhs, rhs) =>
          S.Or(transformExpr(lhs), transformExpr(rhs))

        case N.Equals(lhs, rhs) =>
          S.Equals(transformExpr(lhs), transformExpr(rhs))

        case N.Concat(lhs, rhs) =>
          S.Concat(transformExpr(lhs), transformExpr(rhs))

        case N.Not(exp) =>
          S.Not(transformExpr(exp))

        case N.Neg(exp) =>
          S.Neg(transformExpr(exp))

        case N.Call(qn, args) =>
          if (table.getFunction(qn.module.getOrElse(module), qn.name).isEmpty){
            if (table.getConstructor(qn.module.getOrElse(module), qn.name).isEmpty){
              fatal(s"Impossible call")
            }
            else {
              val Some((sym, sig)) = table.getConstructor(qn.module.getOrElse(module), qn.name)
              if (args.size != sig.argTypes.size) fatal(s"Incompatible number of arguments")
              val arguments = args.map(x => transformExpr(x))
              S.Call(sym, arguments)
            }
          }
          else {
            val Some((sym, sig)) = table.getFunction(qn.module.getOrElse(module), qn.name)
            if (args.size != sig.argTypes.size) fatal("Incompatible number of arguments")
            val arguments = args.map(x => transformExpr(x))
            S.Call(sym, arguments)
          }

        case N.Sequence(e1, e2) =>
          S.Sequence(transformExpr(e1), transformExpr(e2))

        case N.Let(df, value, body) =>
          if (locals.contains(df.name)){
            fatal("Duplicate locals")
          }
          val identifier = Identifier.fresh(df.name)
          val paramDef = S.ParamDef(identifier, S.TypeTree(transformType(df.tt, module)))
          S.Let(paramDef, transformExpr(value), transformExpr(body)(module, (params, locals + (df.name -> identifier))))

        case N.Ite(cond, thenn, elze) =>
          S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))

        case N.Error(msg) =>
          S.Error(transformExpr(msg))

        case _ => fatal("Unknown expression")

      }
      res.setPos(expr)
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)

  }
}
