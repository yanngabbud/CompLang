package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._
import amyc.ast.NominalTreeModule
import org.antlr.v4.runtime.atn.EpsilonTransition

// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1 extends ASTConstructor {
  override def constructQname(pTree: NodeOrLeaf[Token]): (QualifiedName, Positioned) = {
    pTree match {
      case Node('QName ::= _, List(id1, qnbis)) => qnbis match {
        case Node('QNameBis ::= List(DOT(), _), List(_, id2)) =>
          val (module, pos) = constructName(id1)
          val (name, _) = constructName(id2)
          (QualifiedName(Some(module), name), pos)
        case _ =>
          val (name, pos) = constructName(id1)
          (QualifiedName(None, name), pos)
      }
    }
  }

  override def constructPattern(pTree: NodeOrLeaf[Token]): NominalTreeModule.Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('Literal), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= ('Id :: _), List(id, id_prime)) =>
        id_prime match {
          case Node(_ ::= (DOT() :: _), List(_, id2, _, patts, _)) =>
            val (module, pos) = constructName(id)
            val (name, _) = constructName(id2)
            val qname = QualifiedName(Some(module), name)
            val patterns = constructList(patts, constructPattern, hasComma = true)
            CaseClassPattern(qname, patterns).setPos(pos)
          case Node(_ ::= (LPAREN() :: _), List(_, patts, _)) =>
            val (name, pos) = constructName(id)
            val qname = QualifiedName(None, name)
            val patterns = constructList(patts, constructPattern, hasComma = true)
            CaseClassPattern(qname, patterns).setPos(pos)
          case _ =>
            val (name, pos) = constructName(id)
            IdPattern(name).setPos(pos)
        }
    }
  }

  override def constructExpr(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Expr ::= (VAL() :: _), List(Leaf(vt), param, _, expr1, _, expr2)) =>
        Let(constructParam(param), constructMatch(expr1), constructExpr(expr2)).setPos(vt)
      case Node('Expr ::= ('LvlSemicolon :: _), List(semiCol)) =>
        val cons = constructSemiCol(semiCol)
        cons.setPos(cons)
    }
  }

  def constructSemiCol(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('LvlSemicolon ::= _, List(matchNotNull, semiCol)) => semiCol match {
        case Node('Semicolon ::= (SEMICOLON() :: _), List(_, expr)) =>
          val cons1 = constructMatch(matchNotNull)
          val cons2 = constructExpr(expr)
          Sequence(cons1, cons2).setPos(cons1)
        case _ =>
          val cons = constructMatch(matchNotNull)
          cons.setPos(cons)
      }
    }
  }

  def constructMatch(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('LvlMatch ::= _, List(orNotNull, matching)) => matching match {
        case Node('Match ::= (MATCH() :: _), List(_, _, cases, _)) =>
          val m = constructOr(orNotNull)
          Match(m, constructList1(cases, constructCase)).setPos(m)
        case _ =>
          val cons = constructOr(orNotNull)
          cons.setPos(cons)
      }
    }
  }

  def constructOr(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('LvlOr ::= _, List(andNotNull1, or)) =>
        val cons = constructAnd(andNotNull1)
        constructOpExpr(cons, or).setPos(cons)
    }
  }

  def constructAnd(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('LvlAnd ::= _, List(equalsNotNull1, and)) =>
        val cons = constructEquals(equalsNotNull1)
        constructOpExpr(cons, and).setPos(cons)
    }
  }

  def constructEquals(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('LvlEqual ::= _, List(lessNotNull1, equal)) =>
        val cons = constructLess(lessNotNull1)
        constructOpExpr(cons, equal).setPos(cons)
    }
  }

  def constructLess(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('LvlLess ::= _, List(plusNotNull1, less)) =>
        val cons = constructPlus(plusNotNull1)
        constructOpExpr(cons, less).setPos(cons)
    }
  }

  def constructPlus(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('LvlPlusAndMinus ::= _, List(divNotNull, plus)) =>
        val cons = constructDiv(divNotNull)
        constructOpExpr(cons, plus).setPos(cons)
    }
  }

  def constructDiv(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('LvlTimesDivAndMod ::= ('BangAndMinus :: _), List(bg, div)) =>
        val cons = constructBang(bg)
        constructOpExpr(cons, div).setPos(cons)
    }
  }

  def constructBang(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node(_ ::= (BANG() :: _), List(Leaf(bang), first)) =>
        Not(constructFirst(first)).setPos(bang)
      case Node(_ ::= (MINUS() :: _), List(Leaf(minus), first)) =>
        Neg(constructFirst(first)).setPos(minus)
      case Node(_, List(first)) =>
        val cons = constructFirst(first)
        cons.setPos(cons)
    }
  }

  def constructFirst(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('Cellar ::= List(LPAREN(), 'ExprOpt, RPAREN()), List(Leaf(lp), expr, _)) =>
        expr match {
          case Node('ExprOpt ::= ('Expr :: _), List(expr_prime)) =>
            constructExpr(expr_prime).setPos(lp)
          case _ => UnitLiteral().setPos(lp)
        }
      case Node('Cellar ::= (TRUE() :: _), List(Leaf(t))) => BooleanLiteral(true).setPos(t)
      case Node('Cellar ::= (FALSE() :: _), List(Leaf(t))) => BooleanLiteral(false).setPos(t)
      case Node('Cellar ::= List('Id, 'IdTer), List(id1, id_prime)) => id_prime match {
        case Node('IdTer ::= (DOT() :: _), List(_, id2, _, args, _)) =>
          val qNameCons = constructList(args, constructExpr, hasComma = true)
          val (module, p) = constructName(id1)
          val (name, _) = constructName(id2)
          val (x, pos) = (QualifiedName(Some(module), name), p)
          Call(x, qNameCons).setPos(pos)
        case Node('IdTer ::= (LPAREN() :: _), List(_, args, _)) =>
          val qNameCons = constructList(args, constructExpr, hasComma = true)
          val (name, p) = constructName(id1)
          val (x, pos) = (QualifiedName(None, name), p)
          Call(x, qNameCons).setPos(pos)
        case _ =>
          val (name, pos) = constructName(id1)
          Variable(name).setPos(pos)
      }
      case Node('Cellar ::= (STRINGLITSENT :: _), List(Leaf(stringLit@STRINGLIT(s)))) =>
        StringLiteral(s).setPos(stringLit)
      case Node('Cellar ::= (INTLITSENT :: _), List(Leaf(intLit@INTLIT(i)))) =>
        IntLiteral(i).setPos(intLit)
      case Node('Cellar ::= (ERROR() :: _), List(Leaf(ert), _, expr, _)) =>
        Error(constructExpr(expr)).setPos(ert)
      case Node('Cellar ::= (IF() :: _), List(Leaf(it), _, cond, _, _, thenn, _, _, _, elze, _)) =>
        cond match {
          case Node('LvlOr ::= _, _) =>
            val condAtom = constructOr(cond)
            Ite(
              condAtom,
              constructExpr(thenn),
              constructExpr(elze)
            ).setPos(it)
          case Node('LvlAnd ::= _, _) =>
            val condAtom = constructAnd(cond)
            Ite(
              condAtom,
              constructExpr(thenn),
              constructExpr(elze)
            ).setPos(it)
          case Node('LvlEqual ::= _, _) =>
            val condAtom = constructEquals(cond)
            Ite(
              condAtom,
              constructExpr(thenn),
              constructExpr(elze)
            ).setPos(it)
          case Node('LvlLess ::= _, _) =>
            val condAtom = constructLess(cond)
            Ite(
              condAtom,
              constructExpr(thenn),
              constructExpr(elze)
            ).setPos(it)
          case Node('LvlPlusAndMinus ::= _, _) =>
            val condAtom = constructPlus(cond)
            Ite(
              condAtom,
              constructExpr(thenn),
              constructExpr(elze)
            ).setPos(it)
          case Node('LvlTimesDivAndMod ::= _, _) =>
            val condAtom = constructDiv(cond)
            Ite(
              condAtom,
              constructExpr(thenn),
              constructExpr(elze)
            ).setPos(it)
          case Node('Cellar ::= _, _) =>
            val condAtom = constructFirst(cond)
            Ite(
              condAtom,
              constructExpr(thenn),
              constructExpr(elze)
            ).setPos(it)
          case _ =>
            Ite(
              constructExpr(cond),
              constructExpr(thenn),
              constructExpr(elze)
            ).setPos(it)
        }
    }
  }

  /*
    'Paren_prime ::= 'Args ~ RPAREN(),
    'QName_prime ::= LPAREN() ~ 'Paren_prime | epsilon(),
     */
  def constructQname_prime(ptree: NodeOrLeaf[Token]): List[Expr] = {
    ptree match {
      case Node('QNameTer ::= (LPAREN() :: _), List(paren, paren_prime)) => paren_prime match {
        case Node('Paren ::= _, List(as, _)) =>
          constructList(as, constructExpr, hasComma = true)
      }
      case _ => Nil
    }
  }

  override def constructOp(ptree: NodeOrLeaf[Token]): (Expr, Expr) => Expr = {
    ptree match {
      case Node(_, List(Leaf(t))) =>
        tokenToExpr(t)
      case Leaf(l) => tokenToExpr(l)
    }
  }

  override def constructList1[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A, hasComma: Boolean = false): List[A] = {
    ptree match {
      case Node(_, List(t)) => List(constructor(t))
      case Node(_, List(t, ts)) =>
        constructor(t) :: constructList1(ts, constructor, hasComma)
      case Node(_, List(t, Leaf(COMMA()), ts)) if hasComma =>
        constructor(t) :: constructList1(ts, constructor, hasComma)
      case _ =>
        Nil
    }
  }

  // Important helper method:
  // Because LL1 grammar is not helpful in implementing left associativity,
  // we give you this method to reconstruct it.
  // This method takes the left operand of an operator (leftopd)
  // as well as the tree that corresponds to the operator plus the right operand (ptree)
  // It parses the right hand side and then reconstruct the operator expression
  // with correct associativity.
  // If ptree is empty, it means we have no more operators and the leftopd is returned.
  // Note: You may have to override constructOp also, depending on your implementation
  def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('Or, 'And, 'Equal, 'Less, 'PlusMinusAndConcat, 'TimesDivAndMod) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) =>
            nextOpd match {
              case Node('LvlAnd ::= _, _) =>
                val nextAtom = constructAnd(nextOpd)
                constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf)
              case Node('LvlEqual ::= _, _) =>
                val nextAtom = constructEquals(nextOpd)
                constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf)
              case Node('LvlLess ::= _, _) =>
                val nextAtom = constructLess(nextOpd)
                constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf)
              case Node('LvlPlusAndMinus ::= _, _) =>
                val nextAtom = constructPlus(nextOpd)
                constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf)
              case Node('LvlTimesDivAndMod ::= _, _) =>
                val nextAtom = constructDiv(nextOpd)
                constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf)
              case Node('BangAndMinus ::= _, _) =>
                val nextAtom = constructBang(nextOpd)
                constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf)
            }
          case Node('BangAndMinus ::= _, _) =>
            val nextAtom = constructBang(rightNode)
            constructOp(op)(leftopd, nextAtom).setPos(leftopd)
        }
    }
  }
}
