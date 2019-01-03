package amyc
package parsing

import utils._

import scala.io.Source
import java.io.File
import parsing.Tokens.COMMENTLIT

// The lexer for Amy.
// Transforms an iterator coming from scala.io.Source to a stream of (Char, Position),
// then uses a functional approach to consume the stream.
object Lexer extends Pipeline[List[File], (Stream[Token], Stream[COMMENTLIT])] {
  import Tokens._

  /** Maps a string s to the corresponding keyword,
    * or None if it corresponds to no keyword
    */
  private def keywords(s: String): Option[Token] = s match {
    case "abstract" => Some(ABSTRACT())
    case "Boolean"  => Some(BOOLEAN())
    case "case"     => Some(CASE())
    case "class"    => Some(CLASS())
    case "def"      => Some(DEF())
    case "else"     => Some(ELSE())
    case "error"    => Some(ERROR())
    case "extends"  => Some(EXTENDS())
    case "false"    => Some(FALSE())
    case "if"       => Some(IF())
    case "Int"      => Some(INT())
    case "match"    => Some(MATCH())
    case "object"   => Some(OBJECT())
    case "String"   => Some(STRING())
    case "true"     => Some(TRUE())
    case "Unit"     => Some(UNIT())
    case "val"      => Some(VAL())
    case _          => None
  }

  // Comments containers
  private var commentStream: Stream[COMMENTLIT] = Stream.Empty

  private def lexFile(ctx: Context)(f: File): Stream[Token] = {
    import ctx.reporter._

    // Special character which represents the end of an input file
    val EndOfFile: Char = scala.Char.MaxValue

    val source = Source.fromFile(f)

    // Useful type alias:
    // The input to the lexer will be a stream of characters,
    // along with their positions in the files
    type Input = (Char, Position)

    def mkPos(i: Int) = Position.fromFile(f, i)

    // The input to the lexer
    val inputStream: Stream[Input] =
      source.toStream.map(c => (c, mkPos(source.pos))) #::: Stream((EndOfFile, mkPos(source.pos)))

    /** Gets rid of whitespaces and comments and calls readToken to get the next token.
      * Returns the first token and the remaining input that did not get consumed
      */
    @scala.annotation.tailrec
    def nextToken(stream: Stream[Input]): (Token, Stream[Input]) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      if (Character.isWhitespace(currentChar)) {
        nextToken(stream.dropWhile{ case (c, _) => Character.isWhitespace(c) } )

      } else if (currentChar == '/' && nextChar == '/') {
        // Single-line comment
        val commentChar = stream.takeWhile{ case (_, p) => p.line == currentPos.line}
        val comment = commentChar.map(x => x._1).mkString
        commentStream = COMMENTLIT(comment, currentPos) #:: commentStream
        val next = stream.dropWhile{ case (_, p) => p.line == currentPos.line}
        if (next.isEmpty) (EOF().setPos(stream.last._2), Stream.empty)
        else nextToken(next)

      } else if (currentChar == '/' && nextChar == '*') {
        // Multi-line comment
        val (stream1, stream2) = stream.drop(2).zip(stream.drop(3)).dropWhile{ case ((c1, p1), (c2, p2)) =>
            c1 != '*' || c2 != '/' || p1.line != p2.line || p1.col + 1 != p2.col
        }.unzip
        if (stream1.nonEmpty && stream2.nonEmpty)
          nextToken(stream2.tail)
        else {
          ctx.reporter.error("Unclosed comment", currentPos)
          (EOF().setPos(stream.last._2), Stream.empty)
        }

      } else {
        readToken(stream)
      }
    }

    /** Reads the next token from the stream. Assumes no whitespace or comments at the beginning.
      * Returns the first token and the remaining input that did not get consumed.
      */
    def readToken(stream: Stream[Input]): (Token, Stream[Input]) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      // Returns input token with correct position and uses up one character of the stream
      def useOne(t: Token) = (t.setPos(currentPos), rest)
      // Returns input token with correct position and uses up two characters of the stream
      def useTwo(t: Token) = (t.setPos(currentPos), rest.tail)

      currentChar match {
        case `EndOfFile` => useOne(EOF())

        // Reserved word or Identifier
        case _ if Character.isLetter(currentChar) =>
          val (wordLetters, afterWord) = stream.span { case (ch, _) =>
            Character.isLetterOrDigit(ch) || ch == '_'
          }
          val word = wordLetters.map(_._1).mkString
          keywords(word) match {
            case None => (ID(word).setPos(currentPos), afterWord)
            case Some(t) => (t.setPos(currentPos), afterWord)
          }

        // Int literal
        case _ if Character.isDigit(currentChar) =>
          val (digitNumbers, afterNumber) = stream.span { case (d, _) =>
              Character.isDigit(d)
          }
          try {
            val number = digitNumbers.map(_._1).mkString.toInt // If the number does not fit 32 bits, this line fails
            (INTLIT(number).setPos(currentPos), afterNumber)
          } catch {
            case e: NumberFormatException => {
              ctx.reporter.error("Value out of integer range", currentPos)
              (BAD().setPos(currentPos), afterNumber)
            }
          }

        // String literal
        case '"' =>
          val (stringCharacters, afterWord) = stream.tail.span { case (ch, _) =>
            ch != '"'
          }
          if (afterWord.nonEmpty && afterWord.head._2.line == currentPos.line) {
            val string = stringCharacters.map(_._1).mkString
            (STRINGLIT(string).setPos(currentPos), afterWord.tail)
          }
          else {
            ctx.reporter.error("Unclosed string literal", currentPos)
            useOne(BAD())
          }

        case ';' =>
          useOne(SEMICOLON())

        case '+' =>
          if (nextChar == '+') useTwo(CONCAT())
          else useOne(PLUS())

        case '-' =>
          useOne(MINUS())

        case '*' =>
          useOne(TIMES())

        case '/' =>
          useOne(DIV())

        case '%' =>
          useOne(MOD())

        case '<' =>
          if (nextChar == '=') useTwo(LESSEQUALS())
          else useOne(LESSTHAN())

        case '&' if nextChar == '&' =>
          useTwo(AND())

        case '|' if nextChar == '|' =>
          useTwo(OR())

        case '=' =>
          if (nextChar == '=') useTwo(EQUALS())
          else if (nextChar == '>') useTwo(RARROW())
          else useOne(EQSIGN())

        case '!' =>
          useOne(BANG())

        case '{' =>
          useOne(LBRACE())

        case '}' =>
          useOne(RBRACE())

        case '(' =>
          useOne(LPAREN())

        case ')' =>
          useOne(RPAREN())

        case ',' =>
          useOne(COMMA())

        case ':' =>
          useOne(COLON())

        case '.' =>
          useOne(DOT())

        case '_' =>
          useOne(UNDERSCORE())

        case _ =>
          ctx.reporter.error("Invalid character", currentPos)
          useOne(BAD())
      }
    }

    // To lex a file, call nextToken() until it returns the empty Stream as "rest"
    def tokenStream(s: Stream[Input]): Stream[Token] = {
      if (s.isEmpty) Stream()
      else {
        val (token, rest) = nextToken(s)
        token #:: tokenStream(rest)
      }
    }

    tokenStream(inputStream)
  }

  // Lexing all input files means putting the tokens from each file one after the other
  def run(ctx: Context)(files: List[File]): (Stream[Token], Stream[COMMENTLIT]) = {
    println("lexer : " + commentStream.size)
    (files.toStream flatMap lexFile(ctx), commentStream)
  }
}

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Stream[Token], Unit] {
  def run(ctx: Context)(tokens: Stream[Token]): Unit = {
    tokens.toList foreach { t => println(s"$t(${t.position.withoutFile})") }
  }
}
